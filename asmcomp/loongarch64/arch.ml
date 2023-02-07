# 2 "asmcomp/loongarch64/arch.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                yala <zhaojunchao@loongson.cn>                          *)
(*                                                                        *)
(*               Copyright © 2008-2023 LOONGSON                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Specific operations for the Loongarch processor *)

open Format

(* Machine-specific command-line options *)

let command_line_options = []

(* Specific operations *)

type specific_operation =
  | Imultaddf of bool        (* multiply, optionally negate, and add *)
  | Imultsubf of bool        (* multiply, optionally negate, and subtract *)
  | Isqrtf                   (* floating-point square root *)

(* Addressing modes *)

type addressing_mode =
  | Iindexed of int                     (* reg + displ *)

let is_immediate n =
  (n <= 0x7FF) && (n >= -0x800)

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = size_addr
let size_float = 8

let allow_unaligned_access = false

(* Behavior of division *)

let division_crashes_on_overflow = false

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
  | Iindexed n -> Iindexed(n + delta)

let num_args_addressing = function
  | Iindexed _ -> 1

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Iindexed n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a%s" printreg arg.(0) idx

let print_specific_operation printreg op ppf arg =
  match op with
  | Imultaddf false ->
      fprintf ppf "%a *f %a +f %a"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Imultaddf true ->
      fprintf ppf "-f (%a *f %a +f %a)"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Imultsubf false ->
      fprintf ppf "%a *f %a -f %a"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Imultsubf true ->
      fprintf ppf "-f (%a *f %a -f %a)"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Isqrtf ->
      fprintf ppf "sqrtf %a"
        printreg arg.(0)

(* Specific operations that are pure *)

let operation_is_pure _ = true

(* Specific operations that can raise *)

let operation_can_raise _ = false
