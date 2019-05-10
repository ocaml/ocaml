(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Machine-specific command-line options *)

let command_line_options =
  [ "-fPIC", Arg.Set Clflags.pic_code,
      " Generate position-independent machine code (default)";
    "-fno-PIC", Arg.Clear Clflags.pic_code,
      " Generate position-dependent machine code" ]

(* Specific operations for the AMD64 processor *)

open Format

type addressing_mode =
    Ibased of Backend_sym.t * int       (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2 of int                    (* reg + reg + displ *)
  | Iscaled of int * int                (* reg * scale + displ *)
  | Iindexed2scaled of int * int        (* reg + reg * scale + displ *)

type specific_operation =
    Ilea of addressing_mode             (* "lea" gives scaled adds *)
  | Istore_int of nativeint * addressing_mode * bool
                                        (* Store an integer constant *)
  | Ioffset_loc of int * addressing_mode (* Add a constant to a location *)
  | Ifloatarithmem of float_operation * addressing_mode
                                       (* Float arith operation with memory *)
  | Ibswap of int                      (* endianness conversion *)
  | Isqrtf                             (* Float square root *)
  | Ifloatsqrtf of addressing_mode     (* Float square root from memory *)
  | Isextend32                         (* 32 to 64 bit conversion with sign
                                          extension *)
and float_operation =
    Ifloatadd | Ifloatsub | Ifloatmul | Ifloatdiv

let spacetime_node_hole_pointer_is_live_before _specific_op = false

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
    Ibased _ -> 0
  | Iindexed _ -> 1
  | Iindexed2 _ -> 2
  | Iscaled _ -> 1
  | Iindexed2scaled _ -> 2

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Ibased(s, 0) ->
      fprintf ppf "\"%a\"" Backend_sym.print s
  | Ibased(s, n) ->
      fprintf ppf "\"%a\" + %i" Backend_sym.print s n
  | Iindexed n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a%s" printreg arg.(0) idx
  | Iindexed2 n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a + %a%s" printreg arg.(0) printreg arg.(1) idx
  | Iscaled(scale, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a  * %i%s" printreg arg.(0) scale idx
  | Iindexed2scaled(scale, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a + %a * %i%s" printreg arg.(0) printreg arg.(1) scale idx

let print_specific_operation printreg op ppf arg =
  match op with
  | Ilea addr -> print_addressing printreg addr ppf arg
  | Istore_int(n, addr, is_assign) ->
      fprintf ppf "[%a] := %nd %s"
         (print_addressing printreg addr) arg n
         (if is_assign then "(assign)" else "(init)")
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
  | Isextend32 ->
      fprintf ppf "sextend32 %a" printreg arg.(0)

let win64 =
  match Config.system with
  | "win64" | "mingw64" | "cygwin" -> true
  | _                   -> false
