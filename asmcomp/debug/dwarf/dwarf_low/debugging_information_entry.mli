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

(** Representation of DWARF DIEs (debugging information entries). *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

include Dwarf_emittable.S with type t := t

(** If [name] is provided, then a global symbol will be created at the
    same place as [label]. *)
val create
   : label:Asm_label.t
  -> name:Asm_symbol.t option
  -> abbreviation_code:Abbreviation_code.t
  -> attribute_values:
       Dwarf_attribute_values.Attribute_value.t
         Dwarf_attributes.Attribute_specification.Sealed.Map.t
  -> t

val create_null : unit -> t
val is_null : t -> bool

val abbreviation_code : t -> Abbreviation_code.t
val attribute_values
   : t
  -> Dwarf_attribute_values.Attribute_value.t
       Dwarf_attributes.Attribute_specification.Sealed.Map.t

val label : t -> Asm_label.t
val symbol : t -> Asm_symbol.t option
