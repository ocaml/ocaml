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

let compile_unit_proto_die ~sourcefile ~prefix_name
      ~start_of_code_symbol ~end_of_code_symbol
      address_table location_list_table range_list_table =
  let unit_name =
    Compilation_unit.get_persistent_ident (Compilation_unit.get_current_exn ())
  in
  let cwd = Sys.getcwd () in
  let source_directory_path, source_filename =
    if Filename.is_relative sourcefile then cwd, sourcefile
    else Filename.dirname sourcefile, Filename.basename sourcefile
  in
  let prefix_name =
    if Filename.is_relative prefix_name then Filename.concat cwd prefix_name
    else prefix_name
  in
  let debug_line_label = Asm_label.for_section (DWARF Debug_line) in
  let addr_base = Address_table.base_addr address_table in
  let loclists_base = Location_list_table.base_addr location_list_table in
  let rnglists_base = Range_list_table.base_addr range_list_table in
  let config_digest = Config.digest_static_configuration_values () in
  let dwarf_5_only =
    match !Clflags.dwarf_version with
    | Four -> []
    | Five -> [
      DAH.create_addr_base addr_base;
      DAH.create_loclists_base loclists_base;
      DAH.create_rnglists_base rnglists_base;
    ]
  in
  let attribute_values =
    [ DAH.create_name source_filename;
      DAH.create_comp_dir source_directory_path;
      (* The [OCaml] attribute value here is only defined in DWARF-5, but
         it doesn't mean anything else in DWARF-4, so we always emit it.
         This saves special-case logic in gdb based on the producer name. *)
      DAH.create_language OCaml;
      DAH.create_producer "ocamlopt";
      DAH.create_ocaml_unit_name unit_name;
      DAH.create_ocaml_compiler_version Sys.ocaml_version;
      DAH.create_ocaml_config_digest config_digest;
      DAH.create_ocaml_prefix_name prefix_name;
      DAH.create_low_pc_from_symbol start_of_code_symbol;
      DAH.create_high_pc_from_symbol ~low_pc:start_of_code_symbol
        end_of_code_symbol;
      DAH.create_stmt_list ~debug_line_label;
    ] @ dwarf_5_only
  in
  Proto_die.create ~parent:None
    ~tag:Compile_unit
    ~attribute_values
    ()
