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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The values assigned to DWARF attributes. *)

module Value : sig
  type 'form t

  val flag_true
     : ?comment:string
    -> unit
    -> Dwarf_attributes.Form.flag_present t

  val bool : ?comment:string -> bool -> Dwarf_attributes.Form.data1 t

  val int8
     : ?comment:string
    -> Numbers.Int8.t
    -> Dwarf_attributes.Form.data1 t

  val int16
     : ?comment:string
    -> Numbers.Int16.t
    -> Dwarf_attributes.Form.data2 t

  val int32
     : ?comment:string
    -> Int32.t
    -> Dwarf_attributes.Form.data4 t

  val int64
     : ?comment:string
    -> Int64.t
    -> Dwarf_attributes.Form.data8 t

  val string : ?comment:string -> string -> Dwarf_attributes.Form.string t

  val indirect_string
     : ?comment:string
    -> string
    -> Dwarf_attributes.Form.strp t

  val code_address_from_label
     : ?comment:string
    -> Asm_label.t
    -> Dwarf_attributes.Form.addr t

  val code_address_from_symbol
     : ?comment:string
    -> Asm_symbol.t
    -> Dwarf_attributes.Form.addr t

  val symbol_32 : Asm_symbol.t -> Dwarf_attributes.Form.data4 t
  val symbol_64 : Asm_symbol.t -> Dwarf_attributes.Form.data8 t

  val offset_into_debug_line
     : Asm_label.t
    -> Dwarf_attributes.Form.sec_offset t

  val offset_into_debug_line_from_symbol
     : Asm_symbol.t
    -> Dwarf_attributes.Form.sec_offset t

  val offset_into_debug_info
     : ?comment:string
    -> Asm_label.t
    -> Dwarf_attributes.Form.ref_addr t

  val offset_into_debug_info_from_symbol
     : ?comment:string
    -> Asm_symbol.t
    -> Dwarf_attributes.Form.ref_addr t

  val offset_into_debug_loc
     : Asm_label.t
    -> Dwarf_attributes.Form.sec_offset t

  val single_location_description
     : Single_location_description.t
    -> Dwarf_attributes.Form.exprloc t

  val composite_location_description
     : Composite_location_description.t
    -> Dwarf_attributes.Form.exprloc t

  val encoding_attribute
     : Encoding_attribute.t
    -> Dwarf_attributes.Form.data1 t
end

module Attribute_value : sig
  type t

  val create : 'form Dwarf_attributes.Attribute_specification.t
    -> 'form Value.t
    -> t

  val attribute_spec : t -> Dwarf_attributes.Attribute_specification.Sealed.t

  include Dwarf_emittable.S with type t := t
end
