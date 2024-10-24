# 2 "asmcomp/power/arch.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Specific operations for the PowerPC processor *)

open Format

type cmm_label = int
(* Do not introduce a dependency to Cmm *)

(* Machine-specific command-line options *)

let command_line_options = []

let archdep_unit_infos () = []

(* Specific operations *)

type specific_operation =
    Imultaddf                           (* multiply and add *)
  | Imultsubf                           (* multiply and subtract *)
  | Ialloc_far of                       (* allocation in large functions *)
      { bytes : int; dbginfo : Debuginfo.alloc_dbginfo }
  | Ipoll_far of { return_label : cmm_label option }
                                        (* poll point in large functions *)
  | Icheckbound_far                     (* bounds check in large functions *)
  | Icheckbound_imm_far of int          (* bounds check in large functions,
                                           constant 2nd arg (the index) *)

(* Addressing modes *)

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2                           (* reg + reg *)

(* Sizes, endianness *)

let big_endian =
  match Config.model with
  | "ppc64" -> true
  | "ppc64le" -> false
  | _ -> assert false
let size_addr = 8
let size_int = size_addr
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
  | Iindexed2 -> assert false

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Ibased(s, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "\"%s\"%s" s idx
  | Iindexed n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a%s" printreg arg.(0) idx
  | Iindexed2 ->
      fprintf ppf "%a + %a" printreg arg.(0) printreg arg.(1)

let print_specific_operation printreg op ppf arg =
  match op with
  | Imultaddf ->
      fprintf ppf "%a *f %a +f %a"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Imultsubf ->
      fprintf ppf "%a *f %a -f %a"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Ialloc_far { bytes; _ } ->
      fprintf ppf "alloc_far %d" bytes
  | Ipoll_far _ ->
      fprintf ppf "poll_far"
  | Icheckbound_far ->
      fprintf ppf "check_far > %a %a" printreg arg.(0) printreg arg.(1)
  | Icheckbound_imm_far n ->
      fprintf ppf "check_far > %a %d" printreg arg.(0) n

(* Specific operations that are pure *)

let operation_is_pure = function
  | Ialloc_far _
  | Ipoll_far _
  | Icheckbound_far
  | Icheckbound_imm_far _ -> false
  | _ -> true

(* Specific operations that can raise *)

let operation_can_raise = function
  | Ialloc_far _
  | Ipoll_far _
  | Icheckbound_far
  | Icheckbound_imm_far _ -> true
  | _ -> false
