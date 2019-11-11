(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Nicolas Ojeda Bar <n.oje.bar@gmail.com>                 *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Specific operations for the RISC-V processor *)

open Format

(* Machine-specific command-line options *)

let command_line_options = []

(* Specific operations *)

type specific_operation =
  | Imultaddf of bool                   (* multiply, optionally negate, and add *)
  | Imultsubf of bool                   (* multiply, optionally negate, and subtract *)

let spacetime_node_hole_pointer_is_live_before = function
  | Imultaddf _ | Imultsubf _ -> false

(* Addressing modes *)

type addressing_mode =
  | Iindexed of int                     (* reg + displ *)

let is_immediate n =
  (n <= 2047) && (n >= -2048)

(* Sizes, endianness *)

let big_endian = false

let rv64 =
  match Config.model with "riscv64" -> true | "riscv32" -> false | _ -> assert false

let size_addr = if rv64 then 8 else 4
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
