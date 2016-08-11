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

(** Helper functions for constructing attribute values that do not
    require a knowledge of DWARF forms. *)

val create_low_pc
   : address_label:Linearize.label
  -> Dwarf_attribute_values.Attribute_value.t

val create_high_pc
   : address_label:Linearize.label
  -> Dwarf_attribute_values.Attribute_value.t

val create_low_pc_from_symbol
   : symbol:Symbol.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_high_pc_from_symbol
   : symbol:Symbol.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_producer
   : producer_name:string
  -> Dwarf_attribute_values.Attribute_value.t

val create_name
   : string
  -> Dwarf_attribute_values.Attribute_value.t

val create_comp_dir
   : directory:string
  -> Dwarf_attribute_values.Attribute_value.t

val create_stmt_list
   : debug_line_label:Linearize.label
  -> Dwarf_attribute_values.Attribute_value.t

val create_external
   : is_visible_externally:bool
  -> Dwarf_attribute_values.Attribute_value.t

val create_location
   : location_list_label:Linearize.label
  -> Dwarf_attribute_values.Attribute_value.t

val create_type
   : proto_die:Proto_die.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_import
   : proto_die:Proto_die.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_encoding
   : encoding:Encoding_attribute.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_byte_size_exn
   : byte_size:int
  -> Dwarf_attribute_values.Attribute_value.t

val create_linkage_name
   : linkage_name:Linkage_name.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_sibling
   : proto_die:Proto_die.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_single_location_description
   : Single_location_description.t
  -> Dwarf_attribute_values.Attribute_value.t
