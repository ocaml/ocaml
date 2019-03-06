(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Management of DWARF "abstract instances" for functions. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val attributes
   : Debuginfo.Function.t
  -> Dwarf_attribute_values.Attribute_value.t list

val find_or_add
   : Dwarf_state.t
  -> Debuginfo.Function.t
  -> Proto_die.t * Asm_symbol.t

val find_maybe_in_another_unit_or_add
   : Dwarf_state.t
  -> Debuginfo.Function.t
  -> Asm_symbol.t option
