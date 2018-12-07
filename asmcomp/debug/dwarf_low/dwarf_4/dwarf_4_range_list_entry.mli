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

(** DWARF range list entries. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

include Dwarf_emittable.S with type t := t

val create_range_list_entry : start_of_code_symbol:Asm_symbol.t
  -> first_address_when_in_scope:Asm_label.t
  -> first_address_when_not_in_scope:Asm_label.t
  -> first_address_when_not_in_scope_offset:int option
  -> t

val create_base_address_selection_entry : base_address_symbol:Asm_symbol.t -> t

val compare_ascending_vma : t -> t -> int
