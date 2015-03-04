(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Description of intrinsic primitives *)

exception Intrin_error of string

type arg_kind =
  [ `Addr
  | `Float
  | `Int
  | `Int32
  | `Int64
  | `Nativeint
  | `M128
  | `M256
  | `Unit ]

type arg = {
  kind           : arg_kind;
  mach_register  : [ `a | `b | `c | `d | `S | `D ] option;
  copy_to_output : int list;
  commutative    : bool;
  earlyclobber   : bool;
  immediate      : bool;
  input          : bool;
  memory         : bool;
  output         : bool;
  register       : bool }

type intrin = {
  asm    : [ `Emit_string of string | `Emit_arg of int ] list;
  args   : arg array;
  cc     : bool;
  memory : bool;
  decl   : string array }

(** Parses assembly code given as string with arguments given as %i, for example
    "addpd %0 %1".  Double percentage is unescaped. *)
let parse_asm ~nargs asm =
  (* Returns the index of the first non digit character with index greater or equal [i].
     If such character does not exist, returns the length of the string. *)
  let rec first_non_digit s i =
    if i = String.length s then i
    else if s.[i] >= '0' && s.[i] <= '9' then first_non_digit s (i + 1)
    else i
  in
  let rec loop acc s i =
    let len = String.length s in
    if len = 0 then acc
    else if i + 1 = len then `Emit_string s :: acc
    else
      match s.[i], s.[i + 1] with
      | '%', '%' -> (* Unescape *)
        let s = (String.sub s 0 i) ^ (String.sub s (i + 1) (len - i - 1)) in
        loop acc s (i + 1)
      | '%', c when c >= '0' && c <= '9' -> (* Argument *)
        let acc = if i = 0 then acc else `Emit_string (String.sub s 0 i) :: acc in
        let j = first_non_digit s (i + 1) in
        let arg = int_of_string (String.sub s (i + 1) (j - i - 1)) in
        if arg < 0 || arg >= nargs then
          raise(Intrin_error(Printf.sprintf "Invalid ASM argument $%d" arg));
        let acc = `Emit_arg arg :: acc in
        if j < len then loop acc (String.sub s j (len - j)) 0
        else acc
      | _, _ -> loop acc s (i + 1)
  in
  List.rev (loop [] asm 0)

let parse_intrin kinds decl =
  let error f = Printf.ksprintf (fun msg -> raise (Intrin_error msg)) f in
  let kinds = Array.of_list kinds in
  let decl = Array.of_list decl in
  let nargs = Array.length kinds in
  let asm = parse_asm ~nargs decl.(0) in
  let args = Array.mapi (fun i kind ->
    let arg = ref {
      kind;
      mach_register  = None;
      copy_to_output = [];
      commutative    = false;
      earlyclobber   = false;
      immediate      = false;
      input          = true;
      memory         = false;
      output         = false;
      register       = false } in
    
    let cstrs = decl.(i + 1) in
    let copy_to_output = ref (0, "") in
    String.iteri (fun j -> function
        '%' ->
        if i >= Array.length kinds - 1 then error "'%%' constraint used with last operand"
        arg := { !arg with commutative = true }
      | '&' ->
        if not !arg.output then error "input operand constraint contains '&'"
        else arg := { !arg with earlyclobber = true }
      | '+' ->
        if j > 0 then
          error "output constraint '+' for operand %d is not at the beginning" i
        else arg := { !arg with input = true; output = true }
      | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c->
        let k, s = !copy_to_output in
        if s <> "" && k != j then begin
          arg := { !arg with copy_to_output = int_of_string s :: !arg.copy_to_output };
          copy_to_output := (i + 1, String.make 1 c)
        end else copy_to_output := (i + 1, s ^ String.make 1 c)
      | '=' ->
        if j > 0 then
          error "output constraint '=' for operand %d is not at the beginning" i
        else arg := { !arg with input = false; output = true }
      | 'a' -> arg := { !arg with register = true; mach_register = Some `a }
      | 'b' -> arg := { !arg with register = true; mach_register = Some `b }
      | 'c' -> arg := { !arg with register = true; mach_register = Some `c }
      | 'd' -> arg := { !arg with register = true; mach_register = Some `d }
      | 'g' -> arg := { !arg with immediate = true; memory = true; register = true }
      | 'i' -> arg := { !arg with immediate = true }
      | 'm' -> arg := { !arg with memory = true }
      | 'r' -> arg := { !arg with register = true }
      | 'D' -> arg := { !arg with register = true; mach_register = Some `D }
      | 'S' -> arg := { !arg with register = true; mach_register = Some `S }
      | c -> error "invalid punctuation '%c' in constraint" c) cstrs;
    let _, s = !copy_to_output in
    if s <> "" then
      arg := { !arg with copy_to_output = int_of_string s :: !arg.copy_to_output };
    !arg) kinds in
  Array.iter (fun arg ->
    List.iter (fun i ->
      if i >= Array.length args then
        error "matching constraint references invalid operand number";
      if not args.(i).output then
        error "matching constraint references non-output operand"
      ) arg.copy_to_output) args;
  let ret = args.(nargs - 1) in
  if ret.input && ret.kind != `Unit then
    error "output operand constraint lacks '='";
  let intrin = ref { decl; asm; args; cc = false; memory = false } in
  for i = nargs + 1 to Array.length decl - 1 do
    match decl.(i) with
      "cc"     -> intrin := { !intrin with cc     = true }
    | "memory" -> intrin := { !intrin with memory = true }
    | _ -> ()
  done;
  !intrin

let name intrin =
  String.concat "" (List.map (function
      `Emit_arg i -> Printf.sprintf "%%%d" i
    | `Emit_string s ->
        let rec escape s i =
          try
            let len = String.length s in
            let i = String.index_from s i '%' in
            let s = (String.sub s 0 i) ^ "%" ^ (String.sub s i (len - i)) in
            escape s (i + 2)
          with Not_found -> s
        in
        escape s 0) intrin.asm)

let description intrin = Array.to_list intrin.decl
