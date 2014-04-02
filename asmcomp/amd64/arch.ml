(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Machine-specific command-line options *)

let pic_code = ref true

let command_line_options =
  [ "-fPIC", Arg.Set pic_code,
      " Generate position-independent machine code (default)";
    "-fno-PIC", Arg.Clear pic_code,
      " Generate position-dependent machine code" ]

(* Specific operations for the AMD64 processor *)

open Format

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2 of int                    (* reg + reg + displ *)
  | Iscaled of int * int                (* reg * scale + displ *)
  | Iindexed2scaled of int * int        (* reg + reg * scale + displ *)

type intrin_arg =
    Iintrin_arg_ord
  | Iintrin_arg_imm of int
  | Iintrin_arg_addr of addressing_mode

type specific_operation =
    Ilea of addressing_mode             (* "lea" gives scaled adds *)
  | Istore_int of nativeint * addressing_mode (* Store an integer constant *)
  | Istore_symbol of string * addressing_mode (* Store a symbol *)
  | Ioffset_loc of int * addressing_mode (* Add a constant to a location *)
  | Ifloatarithmem of float_operation * addressing_mode
                                       (* Float arith operation with memory *)
  | Ibswap of int                      (* endiannes conversion *)
  | Isqrtf                             (* Float square root *)
  | Ifloatsqrtf of addressing_mode     (* Float square root from memory *)
  | Iintrin of Intrin.intrin * intrin_arg list
and float_operation =
    Ifloatadd | Ifloatsub | Ifloatmul | Ifloatdiv

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

let allow_unaligned_access = true

(* Behavior of division *)

let division_crashes_on_overflow = true

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)
  | Iindexed2 n -> Iindexed2(n + delta)
  | Iscaled(scale, n) -> Iscaled(scale, n + delta)
  | Iindexed2scaled(scale, n) -> Iindexed2scaled(scale, n + delta)

let num_args_addressing = function
    Ibased(s, n) -> 0
  | Iindexed n -> 1
  | Iindexed2 n -> 2
  | Iscaled(scale, n) -> 1
  | Iindexed2scaled(scale, n) -> 2

(* Printing operations and addressing modes *)

let print_addressing' printreg addr i ppf arg =
  match addr with
  | Ibased(s, 0) ->
      fprintf ppf "\"%s\"" s
  | Ibased(s, n) ->
      fprintf ppf "\"%s\" + %i" s n
  | Iindexed n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a%s" printreg arg.(i) idx
  | Iindexed2 n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a + %a%s" printreg arg.(i) printreg arg.(i + 1) idx
  | Iscaled(scale, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a  * %i%s" printreg arg.(i) scale idx
  | Iindexed2scaled(scale, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a + %a * %i%s" printreg arg.(i) printreg arg.(i + 1) scale idx

let print_addressing printreg addr ppf arg = print_addressing' printreg addr 0 ppf arg

let print_specific_operation printreg op ppf arg =
  match op with
  | Ilea addr -> print_addressing printreg addr ppf arg
  | Istore_int(n, addr) ->
      fprintf ppf "[%a] := %nd" (print_addressing printreg addr) arg n
  | Istore_symbol(lbl, addr) ->
      fprintf ppf "[%a] := \"%s\"" (print_addressing printreg addr) arg lbl
  | Ioffset_loc(n, addr) ->
      fprintf ppf "[%a] +:= %i" (print_addressing printreg addr) arg n
  | Isqrtf ->
      fprintf ppf "sqrtf %a" printreg arg.(0)
  | Ifloatsqrtf addr ->
     fprintf ppf "sqrtf float64[%a]"
             (print_addressing printreg addr) [|arg.(0)|]
  | Ifloatarithmem(op, addr) ->
      let op_name = function
      | Ifloatadd -> "+f"
      | Ifloatsub -> "-f"
      | Ifloatmul -> "*f"
      | Ifloatdiv -> "/f" in
      fprintf ppf "%a %s float64[%a]" printreg arg.(0) (op_name op)
                   (print_addressing printreg addr)
                   (Array.sub arg 1 (Array.length arg - 1))
  | Ibswap i ->
      fprintf ppf "bswap_%i %a" i printreg arg.(0)
  | Iintrin (intrin, iargs) ->
      let open Intrin in
      let instr_to_arg = Array.create (List.length intrin.args) 0 in
      let _ = List.fold_left (fun (instr_i, arg_i) iarg ->
        instr_to_arg.(instr_i) <- arg_i;
        let arg_i = arg_i +
          match iarg with
            Iintrin_arg_imm _ -> 0
          | Iintrin_arg_ord -> 1
          | Iintrin_arg_addr addr ->
              match addr with
                Ibased _ -> 0
              | Iindexed _
              | Iscaled _ -> 1
              | Iindexed2 _
              | Iindexed2scaled _ -> 2
        in
        instr_i + 1, arg_i) (0, 0) iargs
      in
      List.iter (function
          `Emit_string s -> fprintf ppf "%s" s
        | `Emit_arg 0 -> fprintf ppf "r"
        | `Emit_arg i ->
            let i = i - 1 in
            match List.nth iargs i with
            | Iintrin_arg_ord -> fprintf ppf "%a" printreg arg.(instr_to_arg.(i))
            | Iintrin_arg_imm n -> fprintf ppf "%n" n
            | Iintrin_arg_addr addr ->
                fprintf ppf "%a" (print_addressing' printreg addr instr_to_arg.(i)) arg
        ) intrin.asm

let map_intrin_regs arg intrin iargs f =
  let open Intrin in
  let arg' = Array.copy arg in
  let arg_i = ref 0 in
  List.iter2 (fun a iarg ->
    let copy () =
      arg'.(!arg_i) <- f arg.(!arg_i) a;
      incr arg_i
    in
    match iarg with
      Iintrin_arg_imm _ -> ()
    | Iintrin_arg_ord -> copy ()
    | Iintrin_arg_addr addr ->
        match addr with
        | Ibased _ -> ()
        | Iindexed _
        | Iscaled _ -> copy ()
        | Iindexed2 _
        | Iindexed2scaled _ -> copy (); copy ()) intrin.args iargs;
  arg'
