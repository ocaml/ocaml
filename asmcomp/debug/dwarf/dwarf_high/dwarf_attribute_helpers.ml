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

module AS = Dwarf_attributes.Attribute_specification
module AV = Dwarf_attribute_values.Attribute_value
module V = Dwarf_attribute_values.Value

module Uint64 = Numbers.Uint64

let needs_dwarf_five () =
  match !Clflags.gdwarf_version with
  | Four -> Misc.fatal_error "Attribute not supported for DWARF-4"
  | Five -> ()

let create_entry_pc address_label =
  let spec = AS.create Entry_pc Addr in
  AV.create spec (V.code_address_from_label
    ~comment:"entry PC value" address_label)

let create_low_pc address_label =
  let spec = AS.create Low_pc Addr in
  AV.create spec (V.code_address_from_label
    ~comment:"low PC value" address_label)

let create_high_pc_offset offset =
  match Targetint.repr offset with
  | Int32 offset ->
    let spec = AS.create High_pc Data4 in
    AV.create spec (V.int32 ~comment:"high PC value as offset" offset)
  | Int64 offset ->
    let spec = AS.create High_pc Data8 in
    AV.create spec (V.int64 ~comment:"high PC value as offset" offset)

let create_high_pc ~low_pc high_pc =
  match Arch.size_addr with
  | 4 ->
    (* CR mshinwell: Shouldn't these be of form [Addr]? *)
    let spec = AS.create High_pc Data4 in
    AV.create spec (V.distance_between_label_and_symbol_32_bit
      ~comment:"high PC value" ~upper:high_pc ~lower:low_pc ())
  | 8 ->
    let spec = AS.create High_pc Data8 in
    AV.create spec (V.distance_between_label_and_symbol_64_bit
      ~comment:"high PC value" ~upper:high_pc ~lower:low_pc ())
  | _ -> Misc.fatal_errorf "Unknown [Arch.size_addr] = %d" Arch.size_addr

let create_entry_pc_from_symbol symbol =
  let spec = AS.create Entry_pc Addr in
  AV.create spec (V.code_address_from_symbol
    ~comment:"entry PC value" symbol)

let create_low_pc_from_symbol symbol =
  let spec = AS.create Low_pc Addr in
  AV.create spec (V.code_address_from_symbol
    ~comment:"low PC value" symbol)

let create_high_pc_from_symbol ~low_pc high_pc =
  match Arch.size_addr with
  | 4 ->
    let spec = AS.create High_pc Data4 in
    AV.create spec (V.distance_between_symbols_32_bit
      ~comment:"high PC value" ~upper:high_pc ~lower:low_pc ())
  | 8 ->
    let spec = AS.create High_pc Data8 in
    AV.create spec (V.distance_between_symbols_64_bit
      ~comment:"high PC value" ~upper:high_pc ~lower:low_pc ())
  | _ -> Misc.fatal_errorf "Unknown [Arch.size_addr] = %d" Arch.size_addr

let create_producer producer_name =
  let spec = AS.create Producer Strp in
  AV.create spec (V.indirect_string ~comment:"producer name" producer_name)

let create_name name =
  let spec = AS.create Name Strp in
  AV.create spec (V.indirect_string ~comment:"name" name)

let create_comp_dir directory =
  let spec = AS.create Comp_dir Strp in
  AV.create spec (V.indirect_string ~comment:"compilation directory" directory)

let create_stmt_list ~debug_line_label =
  let spec = AS.create Stmt_list Sec_offset_lineptr in
  (* DWARF-4 standard section 3.1.1.4. *)
  AV.create spec (V.offset_into_debug_line debug_line_label)

let create_external ~is_visible_externally =
  if is_visible_externally then
    let spec = AS.create External Flag_present in
    AV.create spec (V.flag_true ~comment:"visible externally" ())
  else
    let spec = AS.create External Flag in
    AV.create spec (V.bool ~comment:"not visible externally" false)

let create_call_file file =
  let spec = AS.create Call_file Udata in
  let file = Uint64.of_int_exn file in
  AV.create spec (V.uleb128 ~comment:"file number" file)

let create_call_line line =
  let spec = AS.create Call_line Udata in
  let line = Uint64.of_int_exn line in
  AV.create spec (V.uleb128 ~comment:"line number" line)

let create_call_column column =
  (* CR mshinwell: Check whether "call_file", "call_line" and "call_column"
     are indeed supported pre DWARF 5.  The gdb manual suggests that they've
     been around since DWARF 2. *)
  let spec = AS.create Call_column Udata in
  let column = Uint64.of_int_exn column in
  AV.create spec (V.uleb128 ~comment:"column number" column)

let create_call_pc label =
  let spec = AS.create Call_pc Addr in
  needs_dwarf_five ();
  AV.create spec (V.code_address_from_label ~comment:"PC of call site" label)

let create_call_return_pc label =
  needs_dwarf_five ();
  let spec = AS.create Call_return_pc Addr in
  AV.create spec (V.code_address_from_label
    ~comment:"PC immediately after call site" label)

let create_call_tail_call ~is_tail =
  if is_tail then
    let spec =
      match !Clflags.gdwarf_version with
      | Four -> AS.create (Dwarf_4 GNU_tail_call) Flag_present
      | Five -> AS.create Call_tail_call Flag_present
    in
    AV.create spec (V.flag_true ~comment:"is a tail call" ())
  else
    let spec =
      match !Clflags.gdwarf_version with
      | Four -> AS.create (Dwarf_4 GNU_tail_call) Flag
      | Five -> AS.create Call_tail_call Flag
    in
    AV.create spec (V.bool ~comment:"is a non-tail call" false)

let create_call_all_calls () =
  let spec =
    match !Clflags.gdwarf_version with
    | Four -> AS.create (Dwarf_4 GNU_all_call_sites) Flag_present
    | Five -> AS.create Call_all_calls Flag_present
  in
  AV.create spec (V.flag_true ~comment:"DW_AT_call_all_calls is set" ())

let create_call_target loc_desc =
  let spec =
    match !Clflags.gdwarf_version with
    | Four -> AS.create (Dwarf_4 GNU_call_site_target) Exprloc
    | Five -> AS.create Call_target Exprloc
  in
  AV.create spec (V.single_location_description loc_desc)

let create_call_target_clobbered loc_desc =
  let spec =
    match !Clflags.gdwarf_version with
    | Four -> AS.create (Dwarf_4 GNU_call_site_target_clobbered) Exprloc
    | Five -> AS.create Call_target_clobbered Exprloc
  in
  AV.create spec (V.single_location_description loc_desc)

let create_location index =
  needs_dwarf_five ();
  let location_list_label = Location_list_table.Index.to_label index in
  let location_list_index = Location_list_table.Index.to_uint64 index in
  if not !Clflags.gdwarf_offsets then
    let spec = AS.create Location Sec_offset_loclist in
    AV.create spec (V.offset_into_debug_loclists location_list_label)
  else
    let spec = AS.create Location Loclistx in
    AV.create spec (V.loclistx ~index:location_list_index)

let create_ranges index =
  needs_dwarf_five ();
  let range_list_label = Range_list_table.Index.to_label index in
  let range_list_index = Range_list_table.Index.to_uint64 index in
  if not !Clflags.gdwarf_offsets then
    let spec = AS.create Ranges Sec_offset_rnglist in
    AV.create spec (V.offset_into_debug_rnglists range_list_label)
  else
    let spec = AS.create Ranges Rnglistx in
    AV.create spec (V.rnglistx ~index:range_list_index)

let create_single_location_description loc_desc =
  let spec = AS.create Location Exprloc in
  AV.create spec (V.single_location_description loc_desc)

let create_composite_location_description loc_desc =
  let spec = AS.create Location Exprloc in
  AV.create spec (V.composite_location_description loc_desc)

let create_single_call_value_location_description loc_desc =
  let spec =
    match !Clflags.gdwarf_version with
    | Four -> AS.create (Dwarf_4 GNU_call_site_value) Exprloc
    | Five -> AS.create Call_value Exprloc
  in
  AV.create spec (V.single_location_description loc_desc)

let create_composite_call_value_location_description loc_desc =
  let spec =
    match !Clflags.gdwarf_version with
    | Four -> AS.create (Dwarf_4 GNU_call_site_value) Exprloc
    | Five -> AS.create Call_value Exprloc
  in
  AV.create spec (V.composite_location_description loc_desc)

let create_single_call_data_location_description loc_desc =
  needs_dwarf_five ();
  let spec = AS.create Call_data_location Exprloc in
  AV.create spec (V.single_location_description loc_desc)

let create_single_call_data_value_location_description loc_desc =
  let spec =
    match !Clflags.gdwarf_version with
    | Four -> AS.create (Dwarf_4 GNU_call_site_data_value) Exprloc
    | Five -> AS.create Call_data_value Exprloc
  in
  AV.create spec (V.single_location_description loc_desc)

let create_encoding ~encoding =
  let spec = AS.create Encoding Data1 in
  AV.create spec (V.encoding_attribute encoding)

let reference_proto_die attribute proto_die =
  let spec = AS.create attribute Ref_addr in
  let label = Proto_die.reference proto_die in
  AV.create spec (V.offset_into_debug_info ~comment:"ref. to DIE" label)

let create_type ~proto_die = reference_proto_die Type proto_die
let create_sibling ~proto_die = reference_proto_die Sibling proto_die
let create_import ~proto_die = reference_proto_die Import proto_die

let create_type_from_reference ~proto_die_reference:label =
  let spec = AS.create Type Ref_addr in
  AV.create spec (V.offset_into_debug_info
    ~comment:"reference to type DIE" label)

(* CR-soon mshinwell: remove "_exn" prefix. *)
let create_byte_size_exn ~byte_size =
  let spec = AS.create Byte_size Data8 in
  AV.create spec (V.int64 ~comment:"byte size" (Int64.of_int byte_size))

let create_bit_size bit_size =
  let spec = AS.create Bit_size Data8 in
  AV.create spec (V.int64 ~comment:"bit size" bit_size)

let create_data_member_location ~byte_offset =
  let spec = AS.create Data_member_location Data8 in
  AV.create spec (V.int64 ~comment:"data member location" byte_offset)

let create_linkage_name linkage_name =
  let spec = AS.create Linkage_name Strp in
  AV.create spec (V.indirect_string ~comment:"linkage name"
    (Linkage_name.to_string linkage_name))

let create_const_value_from_symbol ~symbol =
  match Targetint.size with
  | 32 ->
    let spec = AS.create Const_value Data4 in
    AV.create spec (V.symbol_32 symbol)
  | 64 ->
    let spec = AS.create Const_value Data8 in
    AV.create spec (V.symbol_64 symbol)
  | size -> Misc.fatal_errorf "Unknown Targetint.size %d" size

let create_addr_base label =
  let spec = AS.create Addr_base Sec_offset_addrptr in
  AV.create spec (V.offset_into_debug_addr label)

let create_loclists_base label =
  let spec = AS.create Loclists_base Sec_offset_loclistsptr in
  AV.create spec (V.offset_into_debug_loclists label)

let create_rnglists_base label =
  let spec = AS.create Rnglists_base Sec_offset_rnglistsptr in
  AV.create spec (V.offset_into_debug_rnglists label)

let create_inline inline_code =
  let spec = AS.create Inline Data1 in
  AV.create spec (V.inline_code inline_code)

let create_call_origin ~die_symbol =
  let comment = "reference to call origin DIE" in
  let spec =
    match !Clflags.gdwarf_version with
    | Four ->
      (* The GDB code says that [DW_AT_abstract_origin] is a GNU extension
         alias, pre-DWARF 5, for [DW_AT_call_origin]. *)
      AS.create Abstract_origin Ref_addr
    | Five ->
      AS.create Call_origin Ref_addr
  in
  AV.create spec (V.offset_into_debug_info_from_symbol ~comment die_symbol)

let create_abstract_origin ~die_symbol =
  let spec = AS.create Abstract_origin Ref_addr in
  AV.create spec (V.offset_into_debug_info_from_symbol
    ~comment:"reference to abstract origin DIE" die_symbol)

let create_language lang =
  let spec = AS.create Language Data1 in
  AV.create spec (V.language lang)

let create_declaration () =
  let spec = AS.create Declaration Flag_present in
  AV.create spec (
    V.flag_true ~comment:"incomplete / non-defining declaration" ())

let create_ocaml_compiler_version version =
  let spec = AS.create (Ocaml_specific Compiler_version) Strp in
  AV.create spec (V.indirect_string ~comment:"OCaml compiler version" version)

let create_ocaml_unit_name unit_name =
  let spec = AS.create (Ocaml_specific Unit_name) Strp in
  AV.create spec (V.indirect_string ~comment:"unit name" (Ident.name unit_name))

let create_ocaml_config_digest digest =
  let hex = Digest.to_hex digest in
  let spec = AS.create (Ocaml_specific Config_digest) Strp in
  AV.create spec (V.indirect_string ~comment:"static config value digest" hex)

let create_ocaml_prefix_name name =
  let spec = AS.create (Ocaml_specific Prefix_name) Strp in
  AV.create spec (V.indirect_string ~comment:"prefix name" name)

let create_ocaml_linker_dirs dirs =
  let dirs =
    Dwarf_name_laundry.mangle_linker_dirs (Misc.Stdlib.String.Set.elements dirs)
  in
  let spec = AS.create (Ocaml_specific Linker_dirs) Strp in
  AV.create spec (V.indirect_string ~comment:"linker dirs" dirs)

let create_ocaml_cmt_file_digest digest =
  let hex = Digest.to_hex digest in
  let spec = AS.create (Ocaml_specific Cmt_file_digest) Strp in
  AV.create spec (V.indirect_string ~comment:".cmt file digest" hex)
