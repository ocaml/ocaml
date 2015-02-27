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
  | `Int64
  | `M128
  | `M256
  | `Unit ]

type arg = {
  kind           : arg_kind;
  mach_register  : [ `a | `b | `c | `d | `S | `D ] option;
  copy_to_output : int option;
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
  memory : bool }

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
      copy_to_output = None;
      commutative    = false;
      earlyclobber   = false;
      immediate      = false;
      input          = true;
      memory         = false;
      output         = false;
      register       = false } in
    
    let cstrs = decl.(i + 1) in
    let i = ref 0 in
    while !i < String.length cstrs && cstrs.[!i] >= '0' && cstrs.[!i] <= '9' do
      incr i
    done;
    let cstrs =
      if !i > 0 then begin
        let copy_to_output = Some (int_of_string (String.sub cstrs 0 !i)) in
        arg := { !arg with copy_to_output };
        String.sub cstrs !i (String.length cstrs - !i)
      end else cstrs
    in

    String.iter (function
        'a' -> arg := { !arg with mach_register = Some `a }
      | 'b' -> arg := { !arg with mach_register = Some `b }
      | 'c' -> arg := { !arg with mach_register = Some `c }
      | 'd' -> arg := { !arg with mach_register = Some `d }
      | 'S' -> arg := { !arg with mach_register = Some `S }
      | 'D' -> arg := { !arg with mach_register = Some `D }
      | '%' -> arg := { !arg with commutative = true }
      | '&' -> arg := { !arg with earlyclobber = true }
      | 'i' -> arg := { !arg with immediate = true }
      | 'm' -> arg := { !arg with memory = true }
      | 'r' -> arg := { !arg with register = true }
      | '=' -> arg := { !arg with input = false; output = true }
      | '+' -> arg := { !arg with input = true; output = true }
      | c -> error "Unknown argument modifier '%c'" c) cstrs;
    !arg) kinds in
  let ret = args.(nargs - 1) in
  if ret.input && ret.kind != `Unit then
    error "The last argument is input";
  let intrin = ref { asm; args; cc = false; memory = false } in
  for i = nargs + 1 to Array.length decl - 1 do
    match decl.(i) with
      "cc"     -> intrin := { !intrin with cc     = true }
    | "memory" -> intrin := { !intrin with memory = true }
    | d -> error "Unknown argument \"%s\"" d
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

let description intrin =
  let args = Array.map (fun arg ->
      ( match arg.copy_to_output with None -> "" | Some x -> string_of_int x )
    ^ ( match arg.mach_register with None -> "" | Some r ->
          match r with
          | `a -> "a"
          | `b -> "b"
          | `c -> "c"
          | `d -> "d"
          | `S -> "S"
          | `D -> "D" )
    ^ ( if arg.commutative             then "x" else "" )
    ^ ( if arg.earlyclobber            then "&" else "" )
    ^ ( if arg.immediate               then "i" else "" )
    ^ ( if arg.memory                  then "m" else "" )
    ^ ( if arg.register                then "r" else "" )
    ^ ( if arg.output && arg.input     then "+" else "" )
    ^ ( if arg.output && not arg.input then "=" else "" )
    ) intrin.args in
  let result = "" in
  [name intrin] @ (Array.to_list args) @ [result]
