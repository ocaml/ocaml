(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t

include Dwarf_emittable.S with type t := t

val create_location_list_entry : start_of_code_symbol:Symbol.t
  -> first_address_when_in_scope:Linearize.label
  -> first_address_when_not_in_scope:Linearize.label
  -> first_address_when_not_in_scope_offset:int option
  -> location_expression:Location_expression.t
  -> t

val create_base_address_selection_entry : base_address_symbol:Symbol.t -> t

val compare_ascending_vma : t -> t -> int
