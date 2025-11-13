(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | DW_AT_sibling | DW_AT_location | DW_AT_name | DW_AT_ordering
  | DW_AT_byte_size | DW_AT_bit_offset | DW_AT_bit_size | DW_AT_stmt_list
  | DW_AT_low_pc | DW_AT_high_pc | DW_AT_language | DW_AT_discr
  | DW_AT_discr_value | DW_AT_visibility | DW_AT_import | DW_AT_string_length
  | DW_AT_common_reference | DW_AT_comp_dir | DW_AT_const_value
  | DW_AT_containing_type | DW_AT_default_value | DW_AT_inline
  | DW_AT_is_optional | DW_AT_lower_bound | DW_AT_producer | DW_AT_prototyped
  | DW_AT_return_addr | DW_AT_start_scope | DW_AT_bit_stride
  | DW_AT_upper_bound | DW_AT_abstract_origin | DW_AT_accessibility
  | DW_AT_address_class | DW_AT_artificial | DW_AT_base_types
  | DW_AT_calling_convention | DW_AT_count | DW_AT_data_member_location
  | DW_AT_decl_column | DW_AT_decl_file | DW_AT_decl_line | DW_AT_declaration
  | DW_AT_discr_list | DW_AT_encoding | DW_AT_external | DW_AT_frame_base
  | DW_AT_friend | DW_AT_identifier_case | DW_AT_macro_info
  | DW_AT_namelist_item | DW_AT_priority | DW_AT_segment
  | DW_AT_specification | DW_AT_static_link | DW_AT_type | DW_AT_use_location
  | DW_AT_variable_parameter | DW_AT_virtuality | DW_AT_vtable_elem_location
  | DW_AT_allocated | DW_AT_associated | DW_AT_data_location
  | DW_AT_byte_stride | DW_AT_entry_pc | DW_AT_use_UTF8 | DW_AT_extension
  | DW_AT_ranges | DW_AT_trampoline | DW_AT_call_column | DW_AT_call_file
  | DW_AT_call_line | DW_AT_description | DW_AT_binary_scale
  | DW_AT_decimal_scale | DW_AT_small | DW_AT_decimal_sign | DW_AT_digit_count
  | DW_AT_picture_string | DW_AT_mutable | DW_AT_threads_scaled
  | DW_AT_explicit | DW_AT_object_pointer | DW_AT_endianity | DW_AT_elemental
  | DW_AT_pure | DW_AT_recursive | DW_AT_signature | DW_AT_main_subprogram
  | DW_AT_data_bit_offset | DW_AT_const_expr | DW_AT_enum_class
  | DW_AT_linkage_name | DW_AT_call_site_value | DW_AT_call_site_data_value
  | DW_AT_call_site_target | DW_AT_call_site_target_clobbered
  | DW_AT_tail_call | DW_AT_all_tail_call_sites | DW_AT_all_call_sites
  | DW_AT_all_source_call_sites | DW_AT_call_pc | DW_AT_call_origin
  | DW_AT_call_parameter | DW_AT_call_return_pc | DW_AT_call_value
  | DW_AT_call_target | DW_AT_call_target_clobbered | DW_AT_call_data_location
  | DW_AT_call_data_value | DW_AT_noreturn | DW_AT_alignment
  | DW_AT_export_symbols | DW_AT_deleted | DW_AT_defaulted
  | DW_AT_loclists_base | DW_AT_rnglists_base | DW_AT_reference
  | DW_AT_rvalue_reference | DW_AT_macros | DW_AT_call_all_calls
  | DW_AT_call_all_source_calls | DW_AT_call_all_tail_calls
  | DW_AT_call_return_value | DW_AT_call_target_address | DW_AT_rank
  | DW_AT_str_offsets_base | DW_AT_addr_base
  | DW_AT_GNU_call_site_value | DW_AT_GNU_call_site_data_value
  | DW_AT_GNU_call_site_target | DW_AT_GNU_call_site_target_clobbered
  | DW_AT_GNU_tail_call | DW_AT_GNU_all_tail_call_sites
  | DW_AT_GNU_all_call_sites | DW_AT_GNU_all_source_call_sites

(* DWARF 4 specification section 7.5.4 *)
let to_code = function
  | DW_AT_sibling -> 0x01 | DW_AT_location -> 0x02
  | DW_AT_name -> 0x03 | DW_AT_ordering -> 0x09
  | DW_AT_byte_size -> 0x0b | DW_AT_bit_offset -> 0x0c
  | DW_AT_bit_size -> 0x0d | DW_AT_stmt_list -> 0x10
  | DW_AT_low_pc -> 0x11 | DW_AT_high_pc -> 0x12
  | DW_AT_language -> 0x13 | DW_AT_discr -> 0x15
  | DW_AT_discr_value -> 0x16 | DW_AT_visibility -> 0x17
  | DW_AT_import -> 0x18 | DW_AT_string_length -> 0x19
  | DW_AT_common_reference -> 0x1a | DW_AT_comp_dir -> 0x1b
  | DW_AT_const_value -> 0x1c | DW_AT_containing_type -> 0x1d
  | DW_AT_default_value -> 0x1e | DW_AT_inline -> 0x20
  | DW_AT_is_optional -> 0x21 | DW_AT_lower_bound -> 0x22
  | DW_AT_producer -> 0x25 | DW_AT_prototyped -> 0x27
  | DW_AT_return_addr -> 0x2a | DW_AT_start_scope -> 0x2c
  | DW_AT_bit_stride -> 0x2e | DW_AT_upper_bound -> 0x2f
  | DW_AT_abstract_origin -> 0x31 | DW_AT_accessibility -> 0x32
  | DW_AT_address_class -> 0x33 | DW_AT_artificial -> 0x34
  | DW_AT_base_types -> 0x35 | DW_AT_calling_convention -> 0x36
  | DW_AT_count -> 0x37 | DW_AT_data_member_location -> 0x38
  | DW_AT_decl_column -> 0x39 | DW_AT_decl_file -> 0x3a
  | DW_AT_decl_line -> 0x3b | DW_AT_declaration -> 0x3c
  | DW_AT_discr_list -> 0x3d | DW_AT_encoding -> 0x3e
  | DW_AT_external -> 0x3f | DW_AT_frame_base -> 0x40
  | DW_AT_friend -> 0x41 | DW_AT_identifier_case -> 0x42
  | DW_AT_macro_info -> 0x43 | DW_AT_namelist_item -> 0x44
  | DW_AT_priority -> 0x45 | DW_AT_segment -> 0x46
  | DW_AT_specification -> 0x47 | DW_AT_static_link -> 0x48
  | DW_AT_type -> 0x49 | DW_AT_use_location -> 0x4a
  | DW_AT_variable_parameter -> 0x4b | DW_AT_virtuality -> 0x4c
  | DW_AT_vtable_elem_location -> 0x4d | DW_AT_allocated -> 0x4e
  | DW_AT_associated -> 0x4f | DW_AT_data_location -> 0x50
  | DW_AT_byte_stride -> 0x51 | DW_AT_entry_pc -> 0x52
  | DW_AT_use_UTF8 -> 0x53 | DW_AT_extension -> 0x54
  | DW_AT_ranges -> 0x55 | DW_AT_trampoline -> 0x56
  | DW_AT_call_column -> 0x57 | DW_AT_call_file -> 0x58
  | DW_AT_call_line -> 0x59 | DW_AT_description -> 0x5a
  | DW_AT_binary_scale -> 0x5b | DW_AT_decimal_scale -> 0x5c
  | DW_AT_small -> 0x5d | DW_AT_decimal_sign -> 0x5e
  | DW_AT_digit_count -> 0x5f | DW_AT_picture_string -> 0x60
  | DW_AT_mutable -> 0x61 | DW_AT_threads_scaled -> 0x62
  | DW_AT_explicit -> 0x63 | DW_AT_object_pointer -> 0x64
  | DW_AT_endianity -> 0x65 | DW_AT_elemental -> 0x66
  | DW_AT_pure -> 0x67 | DW_AT_recursive -> 0x68
  | DW_AT_signature -> 0x69 | DW_AT_main_subprogram -> 0x6a
  | DW_AT_data_bit_offset -> 0x6b | DW_AT_const_expr -> 0x6c
  | DW_AT_enum_class -> 0x6d | DW_AT_linkage_name -> 0x6e
  | DW_AT_call_site_value -> 0x70 | DW_AT_call_site_data_value -> 0x71
  | DW_AT_call_site_target -> 0x72 | DW_AT_call_site_target_clobbered -> 0x73
  | DW_AT_tail_call -> 0x74 | DW_AT_all_tail_call_sites -> 0x75
  | DW_AT_all_call_sites -> 0x76 | DW_AT_all_source_call_sites -> 0x77
  | DW_AT_call_pc -> 0x78 | DW_AT_call_origin -> 0x79
  | DW_AT_call_parameter -> 0x7a | DW_AT_call_return_pc -> 0x7b
  | DW_AT_call_value -> 0x7c | DW_AT_call_target -> 0x7d
  | DW_AT_call_target_clobbered -> 0x7e | DW_AT_call_data_location -> 0x7f
  | DW_AT_call_data_value -> 0x80 | DW_AT_noreturn -> 0x81
  | DW_AT_alignment -> 0x88 | DW_AT_export_symbols -> 0x89
  | DW_AT_deleted -> 0x8a | DW_AT_defaulted -> 0x8b
  | DW_AT_loclists_base -> 0x8c | DW_AT_rnglists_base -> 0x8d
  | DW_AT_reference -> 0x8e | DW_AT_rvalue_reference -> 0x8f
  | DW_AT_macros -> 0x90 | DW_AT_call_all_calls -> 0x91
  | DW_AT_call_all_source_calls -> 0x92 | DW_AT_call_all_tail_calls -> 0x93
  | DW_AT_call_return_value -> 0x94 | DW_AT_call_target_address -> 0x95
  | DW_AT_rank -> 0x96 | DW_AT_str_offsets_base -> 0x97
  | DW_AT_addr_base -> 0x98
  (* GNU extensions *)
  | DW_AT_GNU_call_site_value -> 0x2111
  | DW_AT_GNU_call_site_data_value -> 0x2112
  | DW_AT_GNU_call_site_target -> 0x2113
  | DW_AT_GNU_call_site_target_clobbered -> 0x2114
  | DW_AT_GNU_tail_call -> 0x2115
  | DW_AT_GNU_all_tail_call_sites -> 0x2116
  | DW_AT_GNU_all_call_sites -> 0x2117
  | DW_AT_GNU_all_source_call_sites -> 0x2118

let to_string = function
  | DW_AT_sibling -> "DW_AT_sibling" | DW_AT_location -> "DW_AT_location"
  | DW_AT_name -> "DW_AT_name" | DW_AT_ordering -> "DW_AT_ordering"
  | DW_AT_byte_size -> "DW_AT_byte_size" | DW_AT_bit_offset -> "DW_AT_bit_offset"
  | DW_AT_bit_size -> "DW_AT_bit_size" | DW_AT_stmt_list -> "DW_AT_stmt_list"
  | DW_AT_low_pc -> "DW_AT_low_pc" | DW_AT_high_pc -> "DW_AT_high_pc"
  | DW_AT_language -> "DW_AT_language" | DW_AT_discr -> "DW_AT_discr"
  | DW_AT_discr_value -> "DW_AT_discr_value" | DW_AT_visibility -> "DW_AT_visibility"
  | DW_AT_import -> "DW_AT_import" | DW_AT_string_length -> "DW_AT_string_length"
  | DW_AT_common_reference -> "DW_AT_common_reference" | DW_AT_comp_dir -> "DW_AT_comp_dir"
  | DW_AT_const_value -> "DW_AT_const_value" | DW_AT_containing_type -> "DW_AT_containing_type"
  | DW_AT_default_value -> "DW_AT_default_value" | DW_AT_inline -> "DW_AT_inline"
  | DW_AT_is_optional -> "DW_AT_is_optional" | DW_AT_lower_bound -> "DW_AT_lower_bound"
  | DW_AT_producer -> "DW_AT_producer" | DW_AT_prototyped -> "DW_AT_prototyped"
  | DW_AT_return_addr -> "DW_AT_return_addr" | DW_AT_start_scope -> "DW_AT_start_scope"
  | DW_AT_bit_stride -> "DW_AT_bit_stride" | DW_AT_upper_bound -> "DW_AT_upper_bound"
  | DW_AT_abstract_origin -> "DW_AT_abstract_origin"
  | DW_AT_accessibility -> "DW_AT_accessibility"
  | DW_AT_address_class -> "DW_AT_address_class" | DW_AT_artificial -> "DW_AT_artificial"
  | DW_AT_base_types -> "DW_AT_base_types"
  | DW_AT_calling_convention -> "DW_AT_calling_convention"
  | DW_AT_count -> "DW_AT_count"
  | DW_AT_data_member_location -> "DW_AT_data_member_location"
  | DW_AT_decl_column -> "DW_AT_decl_column" | DW_AT_decl_file -> "DW_AT_decl_file"
  | DW_AT_decl_line -> "DW_AT_decl_line" | DW_AT_declaration -> "DW_AT_declaration"
  | DW_AT_discr_list -> "DW_AT_discr_list" | DW_AT_encoding -> "DW_AT_encoding"
  | DW_AT_external -> "DW_AT_external" | DW_AT_frame_base -> "DW_AT_frame_base"
  | DW_AT_friend -> "DW_AT_friend" | DW_AT_identifier_case -> "DW_AT_identifier_case"
  | DW_AT_macro_info -> "DW_AT_macro_info" | DW_AT_namelist_item -> "DW_AT_namelist_item"
  | DW_AT_priority -> "DW_AT_priority" | DW_AT_segment -> "DW_AT_segment"
  | DW_AT_specification -> "DW_AT_specification" | DW_AT_static_link -> "DW_AT_static_link"
  | DW_AT_type -> "DW_AT_type" | DW_AT_use_location -> "DW_AT_use_location"
  | DW_AT_variable_parameter -> "DW_AT_variable_parameter"
  | DW_AT_virtuality -> "DW_AT_virtuality"
  | DW_AT_vtable_elem_location -> "DW_AT_vtable_elem_location"
  | DW_AT_allocated -> "DW_AT_allocated" | DW_AT_associated -> "DW_AT_associated"
  | DW_AT_data_location -> "DW_AT_data_location"
  | DW_AT_byte_stride -> "DW_AT_byte_stride" | DW_AT_entry_pc -> "DW_AT_entry_pc"
  | DW_AT_use_UTF8 -> "DW_AT_use_UTF8" | DW_AT_extension -> "DW_AT_extension"
  | DW_AT_ranges -> "DW_AT_ranges" | DW_AT_trampoline -> "DW_AT_trampoline"
  | DW_AT_call_column -> "DW_AT_call_column" | DW_AT_call_file -> "DW_AT_call_file"
  | DW_AT_call_line -> "DW_AT_call_line" | DW_AT_description -> "DW_AT_description"
  | DW_AT_binary_scale -> "DW_AT_binary_scale" | DW_AT_decimal_scale -> "DW_AT_decimal_scale"
  | DW_AT_small -> "DW_AT_small" | DW_AT_decimal_sign -> "DW_AT_decimal_sign"
  | DW_AT_digit_count -> "DW_AT_digit_count" | DW_AT_picture_string -> "DW_AT_picture_string"
  | DW_AT_mutable -> "DW_AT_mutable" | DW_AT_threads_scaled -> "DW_AT_threads_scaled"
  | DW_AT_explicit -> "DW_AT_explicit" | DW_AT_object_pointer -> "DW_AT_object_pointer"
  | DW_AT_endianity -> "DW_AT_endianity" | DW_AT_elemental -> "DW_AT_elemental"
  | DW_AT_pure -> "DW_AT_pure" | DW_AT_recursive -> "DW_AT_recursive"
  | DW_AT_signature -> "DW_AT_signature" | DW_AT_main_subprogram -> "DW_AT_main_subprogram"
  | DW_AT_data_bit_offset -> "DW_AT_data_bit_offset"
  | DW_AT_const_expr -> "DW_AT_const_expr" | DW_AT_enum_class -> "DW_AT_enum_class"
  | DW_AT_linkage_name -> "DW_AT_linkage_name"
  | DW_AT_call_site_value -> "DW_AT_call_site_value"
  | DW_AT_call_site_data_value -> "DW_AT_call_site_data_value"
  | DW_AT_call_site_target -> "DW_AT_call_site_target"
  | DW_AT_call_site_target_clobbered -> "DW_AT_call_site_target_clobbered"
  | DW_AT_tail_call -> "DW_AT_tail_call"
  | DW_AT_all_tail_call_sites -> "DW_AT_all_tail_call_sites"
  | DW_AT_all_call_sites -> "DW_AT_all_call_sites"
  | DW_AT_all_source_call_sites -> "DW_AT_all_source_call_sites"
  | DW_AT_call_pc -> "DW_AT_call_pc" | DW_AT_call_origin -> "DW_AT_call_origin"
  | DW_AT_call_parameter -> "DW_AT_call_parameter"
  | DW_AT_call_return_pc -> "DW_AT_call_return_pc"
  | DW_AT_call_value -> "DW_AT_call_value" | DW_AT_call_target -> "DW_AT_call_target"
  | DW_AT_call_target_clobbered -> "DW_AT_call_target_clobbered"
  | DW_AT_call_data_location -> "DW_AT_call_data_location"
  | DW_AT_call_data_value -> "DW_AT_call_data_value"
  | DW_AT_noreturn -> "DW_AT_noreturn" | DW_AT_alignment -> "DW_AT_alignment"
  | DW_AT_export_symbols -> "DW_AT_export_symbols"
  | DW_AT_deleted -> "DW_AT_deleted" | DW_AT_defaulted -> "DW_AT_defaulted"
  | DW_AT_loclists_base -> "DW_AT_loclists_base"
  | DW_AT_rnglists_base -> "DW_AT_rnglists_base"
  | DW_AT_reference -> "DW_AT_reference"
  | DW_AT_rvalue_reference -> "DW_AT_rvalue_reference"
  | DW_AT_macros -> "DW_AT_macros"
  | DW_AT_call_all_calls -> "DW_AT_call_all_calls"
  | DW_AT_call_all_source_calls -> "DW_AT_call_all_source_calls"
  | DW_AT_call_all_tail_calls -> "DW_AT_call_all_tail_calls"
  | DW_AT_call_return_value -> "DW_AT_call_return_value"
  | DW_AT_call_target_address -> "DW_AT_call_target_address"
  | DW_AT_rank -> "DW_AT_rank"
  | DW_AT_str_offsets_base -> "DW_AT_str_offsets_base"
  | DW_AT_addr_base -> "DW_AT_addr_base"
  | DW_AT_GNU_call_site_value -> "DW_AT_GNU_call_site_value"
  | DW_AT_GNU_call_site_data_value -> "DW_AT_GNU_call_site_data_value"
  | DW_AT_GNU_call_site_target -> "DW_AT_GNU_call_site_target"
  | DW_AT_GNU_call_site_target_clobbered -> "DW_AT_GNU_call_site_target_clobbered"
  | DW_AT_GNU_tail_call -> "DW_AT_GNU_tail_call"
  | DW_AT_GNU_all_tail_call_sites -> "DW_AT_GNU_all_tail_call_sites"
  | DW_AT_GNU_all_call_sites -> "DW_AT_GNU_all_call_sites"
  | DW_AT_GNU_all_source_call_sites -> "DW_AT_GNU_all_source_call_sites"

let print ppf attr =
  Format.fprintf ppf "%s" (to_string attr)
