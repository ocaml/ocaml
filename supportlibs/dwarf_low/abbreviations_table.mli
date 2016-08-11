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

val create : unit -> t

val add : t -> Abbreviations_table_entry.t -> t

val find
   : t
  -> tag:Dwarf_tag.t
  -> has_children:Child_determination.t
  -> attribute_specs:Dwarf_attributes.Attribute_specification.Sealed.Set.t
  -> Abbreviation_code.t option
