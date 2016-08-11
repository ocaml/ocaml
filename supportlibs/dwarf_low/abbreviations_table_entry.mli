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

(* One entry in an abbreviations table. *)

type t

include Dwarf_emittable.S with type t := t

val create : abbreviation_code:Abbreviation_code.t
  -> tag:Dwarf_tag.t
  -> has_children:Child_determination.t
  -> attribute_specs:Dwarf_attributes.Attribute_specification.Sealed.Set.t
  -> t

val abbreviation_code : t -> Abbreviation_code.t
val tag : t -> Dwarf_tag.t
val has_children : t -> Child_determination.t
val attribute_specs
   : t
  -> Dwarf_attributes.Attribute_specification.Sealed.Set.t
