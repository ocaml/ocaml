(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Functions for constructing DWARF operators including some simple
    optimisations thereon. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val register_as_lvalue : dwarf_reg_number:int -> Dwarf_operator.t

val contents_of_register : dwarf_reg_number:int -> Dwarf_operator.t

val address_of_stack_slot
   : offset_in_bytes:Targetint.t
  -> Dwarf_operator.t list

val contents_of_stack_slot
   : offset_in_bytes:Targetint.t
  -> Dwarf_operator.t list

val value_of_symbol : symbol:string -> Dwarf_operator.t

val signed_int_const : Targetint.t -> Dwarf_operator.t

val add_unsigned_const : Targetint.t -> Dwarf_operator.t

val implicit_pointer
   : offset_in_bytes:Targetint.t
  -> die_label:Linearize.label
  -> Dwarf_version.t
  -> Dwarf_operator.t

val call
   : die_label:Linearize.label
  -> compilation_unit_header_label:Linearize.label
  -> Dwarf_operator.t

val conditional
   : if_zero:Dwarf_operator.t list
  -> if_nonzero:Dwarf_operator.t list
  -> Dwarf_operator.t list

val optimize_sequence : Dwarf_operator.t list -> Dwarf_operator.t list
