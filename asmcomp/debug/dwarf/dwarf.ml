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

module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state

type t = {
  state : DS.t;
  mutable emitted : bool;
}

(* CR mshinwell: On OS X 10.11 (El Capitan), dwarfdump doesn't seem to be able
   to read our 64-bit DWARF output. *)

let create ~sourcefile ~prefix_name ~cmt_file_digest ~objfiles =
  begin match !Clflags.gdwarf_format with
  | Thirty_two -> Dwarf_format.set Thirty_two
  | Sixty_four -> Dwarf_format.set Sixty_four
  end;
  let dwarf_version =
    match !Clflags.gdwarf_version with
    | Four -> Dwarf_version.four
    | Five -> Dwarf_version.five
  in
  let start_of_code_symbol =
    Dwarf_name_laundry.mangle_symbol Text (
      Symbol.of_global_linkage (Compilation_unit.get_current_exn ())
        (Linkage_name.create "code_begin"))
  in
  let end_of_code_symbol =
    Dwarf_name_laundry.mangle_symbol Text (
      Symbol.of_global_linkage (Compilation_unit.get_current_exn ())
        (Linkage_name.create "code_end"))
  in
  let address_table = Address_table.create () in
  let debug_loc_table = Debug_loc_table.create () in
  let debug_ranges_table = Debug_ranges_table.create () in
  let location_list_table = Location_list_table.create () in
  let range_list_table = Range_list_table.create () in
  let compilation_unit_proto_die =
    Dwarf_compilation_unit.compile_unit_proto_die ~sourcefile ~prefix_name
      ~cmt_file_digest ~objfiles ~start_of_code_symbol ~end_of_code_symbol
      address_table location_list_table range_list_table
  in
  let value_type_proto_die =
    Proto_die.create ~parent:(Some compilation_unit_proto_die)
      ~tag:Base_type
      ~attribute_values:[
        DAH.create_name Dwarf_name_laundry.ocaml_value_type_name;
        DAH.create_encoding ~encoding:Encoding_attribute.signed;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
      ]
      ()
  in
  let naked_float_type_proto_die =
    Proto_die.create ~parent:(Some compilation_unit_proto_die)
      ~tag:Base_type
      ~attribute_values:[
        DAH.create_name Dwarf_name_laundry.ocaml_naked_float_type_name;
        DAH.create_encoding ~encoding:Encoding_attribute.float;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
      ]
      ()
  in
  let compilation_unit_header_label = Asm_label.create (DWARF Debug_info) in
  let state =
    DS.create ~compilation_unit_header_label
      ~compilation_unit_proto_die
      ~value_type_proto_die ~naked_float_type_proto_die
      ~start_of_code_symbol ~end_of_code_symbol
      address_table debug_loc_table debug_ranges_table
      location_list_table range_list_table dwarf_version
  in
  { state;
    emitted = false;
  }

let dwarf_for_fundecl t (result : Debug_passes.result) =
  if Clflags.debug_thing Debug_dwarf_functions then begin
    Dwarf_concrete_instances.for_fundecl t.state result
  end

let dwarf_for_toplevel_constants t constants =
  if Clflags.debug_thing Debug_dwarf_vars then begin
    Dwarf_toplevel_values.dwarf_for_toplevel_constants t.state constants
  end

let dwarf_for_closure_top_level_module_block t ~module_block_sym
      ~module_block_var =
  if Clflags.debug_thing Debug_dwarf_vars then begin
    Dwarf_toplevel_values.dwarf_for_closure_top_level_module_block t.state
        ~module_block_sym ~module_block_var
  end

let emit t =
  if t.emitted then begin
    Misc.fatal_error "Cannot call [Dwarf.emit] more than once on a given \
      value of type [Dwarf.t]"
  end;
  t.emitted <- true;
  Dwarf_world.emit
    ~compilation_unit_proto_die:(DS.compilation_unit_proto_die t.state)
    ~start_of_code_symbol:(DS.start_of_code_symbol t.state)
    ~end_of_code_symbol:(DS.end_of_code_symbol t.state)
    ~compilation_unit_header_label:(DS.compilation_unit_header_label t.state)
    ~address_table:(DS.address_table t.state)
    ~debug_loc_table:(DS.debug_loc_table t.state)
    ~debug_ranges_table:(DS.debug_ranges_table t.state)
    ~location_list_table:(DS.location_list_table t.state)
    ~range_list_table:(DS.range_list_table t.state)

let emit t = Profile.record "emit_dwarf" emit t
