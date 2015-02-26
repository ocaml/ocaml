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
  kind        : arg_kind;
  cp_to_reg   : [ `No | `Result | `A | `C | `D ];
  reload      : [ `No | `M64 | `M128 | `M256 ];
  immediate   : bool;
  output      : bool;
  register    : bool;
  commutative : bool }

type intrin = {
  asm  : [ `Emit_string of string | `Emit_arg of int ] list;
  args : arg array }

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
  match decl with
    asm :: params ->
      let asm = parse_asm ~nargs:(List.length kinds) asm in
      let rec loop kinds params acc_args =
        match kinds, params with
          [], [] -> List.rev acc_args
        | [], param :: params -> (* XXX vbrankov *) loop [] params acc_args
        | kind :: kinds, param :: params ->
          let arg = ref {
            kind;
            cp_to_reg   = `No;
            reload      = `No;
            immediate   = false;
            output      = false;
            register    = false;
            commutative = false } in
          String.iter (function
              '0' -> arg := { !arg with cp_to_reg = `Result }
            | 'a' -> arg := { !arg with cp_to_reg = `A }
            | 'c' -> arg := { !arg with cp_to_reg = `C }
            | 'd' -> arg := { !arg with cp_to_reg = `D }
            | 'i' -> arg := { !arg with immediate = true }
            | 'm' ->
                arg := { !arg with reload = (
                  match !arg.reload with
                  | `No  -> `M64
                  | `M64 -> `M128
                  | _    -> `M256) }
            | 'r' -> arg := { !arg with register = true }
            | 'x' -> arg := { !arg with commutative = true }
            | '=' ->
                arg := { !arg with output = true }
            | c -> error "Unknown argument modifier '%c'" c) param;
          loop kinds params (!arg :: acc_args)
        | _, _ -> error "Missing argument descripions"
      in
      let args = loop kinds params [] in
      { asm; args = Array.of_list args }
  | [] -> error "Primitive.parse_intrin"

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
    let cp_to_reg =
      match arg.cp_to_reg with
      | `No -> ""
      | `Result -> "0"
      | `A -> "a"
      | `C -> "c"
      | `D -> "d"
    in
    let reload =
      match arg.reload with
      | `No   -> ""
      | `M64  -> "m"
      | `M128 -> "mm"
      | `M256 -> "mmm"
    in
    let commutative = if arg.commutative then "x" else "" in
    cp_to_reg ^ reload ^ commutative) intrin.args in
  let result = "" in
  [name intrin] @ (Array.to_list args) @ [result]
