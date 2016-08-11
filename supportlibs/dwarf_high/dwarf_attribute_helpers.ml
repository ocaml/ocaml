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

module A = Dwarf_attributes.Attribute
module AS = Dwarf_attributes.Attribute_specification
module AV = Dwarf_attribute_values.Attribute_value
module F = Dwarf_attributes.Form
module V = Dwarf_attribute_values.Value

let create_low_pc ~address_label =
  let spec = AS.create A.Low_pc F.Addr in
  AV.create spec (V.code_address_from_label address_label)

let create_high_pc ~address_label =
  let spec = AS.create A.High_pc F.Addr in
  AV.create spec (V.code_address_from_label address_label)

let create_low_pc_from_symbol ~symbol =
  let spec = AS.create A.Low_pc F.Addr in
  AV.create spec (V.code_address_from_symbol symbol)

let create_high_pc_from_symbol ~symbol =
  let spec = AS.create A.High_pc F.Addr in
  AV.create spec (V.code_address_from_symbol symbol)

let create_producer ~producer_name =
  let spec = AS.create A.Producer F.Strp in
  AV.create spec (V.indirect_string producer_name)

let create_name name =
  let spec = AS.create A.Name F.Strp in
  AV.create spec (V.indirect_string name)

let create_comp_dir ~directory =
  let spec = AS.create A.Comp_dir F.Strp in
  AV.create spec (V.indirect_string directory)

let create_stmt_list ~debug_line_label =
  let spec = AS.create A.Stmt_list F.Sec_offset_lineptr in
  (* DWARF-4 standard section 3.1.1.4. *)
  AV.create spec (V.offset_into_debug_line debug_line_label)

let create_external ~is_visible_externally =
  if is_visible_externally then
    let spec = AS.create A.External F.Flag_present in
    AV.create spec V.flag_true
  else
    let spec = AS.create A.External F.Flag in
    AV.create spec (V.bool false)

let create_location ~location_list_label =
  let spec = AS.create A.Location F.Sec_offset_loclistptr in
  AV.create spec (V.offset_into_debug_loc location_list_label)

let create_single_location_description loc_desc =
  let spec = AS.create A.Location F.Exprloc in
  AV.create spec (V.location_description loc_desc)

let create_encoding ~encoding =
  let spec = AS.create A.Encoding F.Data1 in
  AV.create spec (V.encoding_attribute encoding)

let reference_proto_die attribute proto_die =
  let spec = AS.create attribute F.Ref_addr in
  let label = Proto_die.reference proto_die in
  AV.create spec (V.offset_into_debug_info label)

let create_type ~proto_die = reference_proto_die A.Type proto_die
let create_sibling ~proto_die = reference_proto_die A.Sibling proto_die
let create_import ~proto_die = reference_proto_die A.Import proto_die

let create_byte_size_exn ~byte_size =
  let spec = AS.create A.Byte_size F.Data1 in
  AV.create spec (V.int8 (Numbers.Int8.of_int_exn byte_size))

let create_linkage_name ~linkage_name =
  let spec = AS.create A.Linkage_name F.Strp in
  AV.create spec (V.indirect_string (Linkage_name.to_string linkage_name))
