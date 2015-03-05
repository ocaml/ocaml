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

type alternative = {
  mach_register  : [ `all | `a | `b | `c | `d | `S | `D ];
  copy_to_output : int list;
  commutative    : bool;
  earlyclobber   : bool;
  immediate      : bool;
  memory         : [ `no | `m | `m8 | `m16 | `m32 | `m64 | `m128 | `m256 ];
  register       : bool }

type arg = {
  kind         : arg_kind;
  input        : bool;
  output       : bool;
  alternatives : alternative array }

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
  if Array.length decl < Array.length kinds + 1 then
    error "expected constraints for all arguments";
  let asm = parse_asm ~nargs decl.(0) in
  let args = Array.mapi (fun i kind ->
    let arg = ref {
      kind;
      input        = true;
      output       = false;
      alternatives = [| |] } in
    let alt = ref {
      mach_register  = `all;
      copy_to_output = [];
      commutative    = false;
      earlyclobber   = false;
      immediate      = false;
      memory         = `no;
      register       = false } in
    let decl = decl.(i + 1) in
    let add_digit =
      let s = ref 0 in
      let e = ref 0 in
      fun j c ->
        if !e > !s && !e != j then begin
          let a = String.sub decl !s (!e - !s) in
          if !s > 0 && decl.[!s - 1] = 'm' then
            let memory =
              match a with
                "8" -> `m8
              | "16" -> `m16
              | "32" -> `m32
              | "64" -> `m64
              | "128" -> `m128
              | "256" -> `m256
              | _ -> error "invalid memory alignment constraint 'm%s'" a
            in
            alt := { !alt with memory }
          else
            alt := { !alt with copy_to_output = int_of_string a :: !alt.copy_to_output };
          s := j;
          e := j + 1
        end else
          e := j + 1
    in
    String.iteri (fun j -> function
        '%' ->
          if i >= Array.length kinds - 1 then
            error "'%%' constraint used with last operand";
          alt := { !alt with commutative = true }
      | '&' ->
          if not !arg.output then error "input operand constraint contains '&'";
            alt := { !alt with earlyclobber = true }
      | '+' ->
          if j > 0 then
            error "output constraint '+' for operand %d is not at the beginning" i;
          arg := { !arg with input = true; output = true }
      | '-' ->
          arg := { !arg with alternatives = Array.append !arg.alternatives [| !alt |] };
          alt := {
            mach_register  = `all;
            copy_to_output = [];
            commutative    = false;
            earlyclobber   = false;
            immediate      = false;
            memory         = `no;
            register       = false }
      | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c ->
          add_digit j c
      | '=' ->
        if j > 0 then
          error "output constraint '=' for operand %d is not at the beginning" i;
        arg := { !arg with input = false; output = true }
      | 'a' -> alt := { !alt with register = true; mach_register = `a }
      | 'b' -> alt := { !alt with register = true; mach_register = `b }
      | 'c' -> alt := { !alt with register = true; mach_register = `c }
      | 'd' -> alt := { !alt with register = true; mach_register = `d }
      | 'g' -> alt := { !alt with immediate = true; memory = `m; register = true }
      | 'i' -> alt := { !alt with immediate = true }
      | 'm' -> alt := { !alt with memory = `m }
      | 'r' -> alt := { !alt with register = true }
      | 'D' -> alt := { !alt with register = true; mach_register = `D }
      | 'S' -> alt := { !alt with register = true; mach_register = `S }
      | c -> error "invalid punctuation '%c' in constraint" c) decl;
    add_digit (String.length decl + 1) '0'; (* flushes *)
    arg := { !arg with alternatives = Array.append !arg.alternatives [| !alt |] };
    !arg) kinds in
  List.iter (function
      `Emit_arg i -> if i >= Array.length args then error "operand number out of range"
    | `Emit_string _ -> ()) asm;
  Array.iter (fun arg ->
    if Array.length arg.alternatives != Array.length args.(0).alternatives then
      error "operand constraints for 'asm' differ in number of alternatives";
    Array.iter (fun alt ->
      List.iter (fun i ->
        if i >= Array.length args then
          error "matching constraint references invalid operand number";
        if not args.(i).output then
          error "matching constraint references non-output operand"
        ) alt.copy_to_output) arg.alternatives) args;
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
