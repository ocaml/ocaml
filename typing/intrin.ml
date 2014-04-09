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
  [ `Array_float
  | `Array_m128
  | `Array_m256
  | `Float
  | `Imm
  | `Int
  | `Int64
  | `M128
  | `M256
  | `Unit ]

type arg = {
  kind        : arg_kind;
  cp_to_reg   : [ `No | `Result | `A | `C | `D ];
  reload      : [ `No | `M64 | `M128 | `M256 ];
  commutative : bool }

type intrin = {
  asm         : [ `Emit_string of string | `Emit_arg of int ] list;
  args        : arg list;
  result      : [ `Float | `Int | `Int64 | `M128 | `M256 | `Unit ];
  result_reg  : [ `Any | `C ] }

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

let parse_intrin args decl =
  let error f = Printf.ksprintf (fun msg -> raise (Intrin_error msg)) f in
  match decl with
    asm :: params ->
      let asm = parse_asm ~nargs:(List.length args) asm in
      let cut_tail l =
        match List.rev l with
        | h :: t -> List.rev t, h
        | [] -> error "Primitive.parse_intrin"
      in
      let args, result = cut_tail args in
      let params, result_param = cut_tail params in
      if List.length args <> List.length params then
        error "The number of arguments and parameters must be the same";
      let args = List.map2 (fun kind param ->
        let kind = ref kind in
        let cp_to_reg = ref `No in
        let reload = ref `No in
        let commutative = ref false in
        String.iter (function
            '0' -> cp_to_reg := `Result
          | 'a' -> cp_to_reg := `A
          | 'c' -> cp_to_reg := `C
          | 'd' -> cp_to_reg := `D
          | 'i' -> if !kind = `Int then kind := `Imm
                   else error "Only integer argument can be immediate"
          | 'x' -> commutative := true
          | 'm' ->
              reload := (
                match !reload with
                | `No  -> `M64
                | `M64 -> `M128
                | _    -> `M256)
          | c -> error "Unknown argument modifier '%c'" c) param;
        { kind        = !kind;
          cp_to_reg   = !cp_to_reg;
          reload      = !reload;
          commutative = !commutative }) args params in
      let result =
        let error s = error "%s cannot be the result of intrin" s in
        match result with
          `Array_float -> error "float array"
        | `Array_m128 -> error "array_m128"
        | `Array_m256 -> error "array_m256"
        | `Float -> `Float
        | `Imm -> error "immediate integer"
        | `Int -> `Int
        | `Int64 -> `Int64
        | `M128 -> `M128
        | `M256 -> `M256
        | `Unit -> `Unit
      in
      let result_reg =
        let reg = ref `Any in
        String.iter (function
            'c' -> reg := `C
          | c -> error "Unknown result modifier '%c'" c) result_param;
        !reg
      in
      { asm; args; result; result_reg }
  | [] -> error "Primitive.parse_intrin"

let intrin_name intrin =
  String.concat "" (List.map (function
      `Emit_arg i -> Printf.sprintf "$%d" i
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

let intrin_description intrin =
  let args = List.map (fun arg ->
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
  [intrin_name intrin] @ args @ [result]
