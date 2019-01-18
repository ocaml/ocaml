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

(** Helper functions for constructing attribute values that do not
    require a knowledge of DWARF forms. *)

val create_entry_pc
   : Asm_label.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_low_pc
   : Asm_label.t
  -> Dwarf_attribute_values.Attribute_value.t

(** Creates a [DW_AT_high_pc] attribute value by taking the offset in
    bytes from the [DW_AT_low_pc] attribute value. *)
val create_high_pc_offset
   : Targetint.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_high_pc
   : low_pc:Asm_symbol.t
  -> Asm_label.t
  -> Dwarf_attribute_values.Attribute_value.t

(* CR mshinwell: Make labels consistent / remove unnecessary ones. *)

val create_entry_pc_from_symbol
   : Asm_symbol.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_low_pc_from_symbol
   : Asm_symbol.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_high_pc_from_symbol
   : low_pc:Asm_symbol.t
  -> Asm_symbol.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_producer
   : string
  -> Dwarf_attribute_values.Attribute_value.t

val create_name
   : string
  -> Dwarf_attribute_values.Attribute_value.t

val create_comp_dir
   : string
  -> Dwarf_attribute_values.Attribute_value.t

val create_stmt_list
   : debug_line_label:Asm_label.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_external
   : is_visible_externally:bool
  -> Dwarf_attribute_values.Attribute_value.t

val create_call_file
   : int
  -> Dwarf_attribute_values.Attribute_value.t

val create_call_line
   : int
  -> Dwarf_attribute_values.Attribute_value.t

val create_call_column
   : int
  -> Dwarf_attribute_values.Attribute_value.t

val create_call_pc
   : Asm_label.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_call_return_pc
   : Asm_label.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_call_tail_call
   : is_tail:bool
  -> Dwarf_attribute_values.Attribute_value.t

val create_call_all_calls
   : unit
  -> Dwarf_attribute_values.Attribute_value.t

val create_call_target
   : Single_location_description.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_call_target_clobbered
   : Single_location_description.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_location
   : Location_list_table.Index.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_ranges
   : Range_list_table.Index.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_type
   : proto_die:Proto_die.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_type_from_reference
   : proto_die_reference:Proto_die.reference
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

val create_bit_size
   : Int64.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_data_member_location
   : byte_offset:Int64.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_linkage_name
   : Linkage_name.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_sibling
   : proto_die:Proto_die.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_single_location_description
   : Single_location_description.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_composite_location_description
   : Composite_location_description.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_single_call_value_location_description
   : Single_location_description.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_composite_call_value_location_description
   : Composite_location_description.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_single_call_data_location_description
   : Single_location_description.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_single_call_data_value_location_description
   : Single_location_description.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_const_value_from_symbol
   : symbol:Asm_symbol.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_addr_base
   : Asm_label.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_loclists_base
   : Asm_label.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_rnglists_base
   : Asm_label.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_inline
   : Inline_code.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_call_origin
   : die_symbol:Asm_symbol.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_abstract_origin
   : die_symbol:Asm_symbol.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_language
   : Dwarf_language.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_declaration
   : unit
  -> Dwarf_attribute_values.Attribute_value.t

(** OCaml-specific DWARF attributes. *)

val create_ocaml_compiler_version
   : string
  -> Dwarf_attribute_values.Attribute_value.t

val create_ocaml_unit_name
   : Ident.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_ocaml_config_digest
   : Digest.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_ocaml_prefix_name
   : string
  -> Dwarf_attribute_values.Attribute_value.t

val create_ocaml_linker_dirs
   : Misc.Stdlib.String.Set.t
  -> Dwarf_attribute_values.Attribute_value.t

val create_ocaml_cmt_file_digest
   : Digest.t
  -> Dwarf_attribute_values.Attribute_value.t
