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

module V = Backend_var

type t = {
  compilation_unit_header_label : Asm_label.t;
  compilation_unit_proto_die : Proto_die.t;
  value_type_proto_die : Proto_die.t;
  naked_float_type_proto_die : Proto_die.t;
  address_table : Address_table.t;
  debug_loc_table : Debug_loc_table.t;
  debug_ranges_table : Debug_ranges_table.t;
  location_list_table : Location_list_table.t;
  range_list_table : Range_list_table.t;
  start_of_code_symbol : Asm_symbol.t;
  end_of_code_symbol : Asm_symbol.t;
  mutable rvalue_dies_required_for : V.Set.t;
  function_abstract_instances :
    (Proto_die.t * Asm_symbol.t) Debuginfo.Function.Id.Tbl.t;
  die_symbols_for_external_declarations : Asm_symbol.t Asm_symbol.Tbl.t;
  supports_call_sites : bool;
  can_reference_dies_across_units : bool;
  dwarf_version : Dwarf_version.t;
  type_dies_for_lifted_constants : Proto_die.t Asm_symbol.Tbl.t;
  module_path_dies : Proto_die.t Path.Tbl.t;
}

let can_reference_dies_across_units () =
  (* We don't know how to do this on macOS yet.  See comments in the
     implementation of [Asm_directives.offset_into_dwarf_section_symbol]. *)
  not (Target_system.macos_like ())

let create ~compilation_unit_header_label ~compilation_unit_proto_die
      ~value_type_proto_die ~naked_float_type_proto_die
      ~start_of_code_symbol ~end_of_code_symbol
      address_table debug_loc_table debug_ranges_table location_list_table
      range_list_table dwarf_version =
  { compilation_unit_header_label;
    compilation_unit_proto_die;
    value_type_proto_die;
    naked_float_type_proto_die;
    address_table;
    debug_loc_table;
    debug_ranges_table;
    location_list_table;
    range_list_table;
    start_of_code_symbol;
    end_of_code_symbol;
    rvalue_dies_required_for = V.Set.empty;
    function_abstract_instances = Debuginfo.Function.Id.Tbl.create 42;
    die_symbols_for_external_declarations = Asm_symbol.Tbl.create 42;
    supports_call_sites = Clflags.debug_thing Debug_dwarf_call_sites;
    can_reference_dies_across_units = can_reference_dies_across_units ();
    dwarf_version;
    type_dies_for_lifted_constants = Asm_symbol.Tbl.create 42;
    module_path_dies = Path.Tbl.create 42;
  }

let compilation_unit_header_label t = t.compilation_unit_header_label
let compilation_unit_proto_die t = t.compilation_unit_proto_die
let value_type_proto_die t = t.value_type_proto_die
let naked_float_type_proto_die t = t.naked_float_type_proto_die
let address_table t = t.address_table
let debug_loc_table t = t.debug_loc_table
let debug_ranges_table t = t.debug_ranges_table
let location_list_table t = t.location_list_table
let range_list_table t = t.range_list_table
let start_of_code_symbol t = t.start_of_code_symbol
let end_of_code_symbol t = t.end_of_code_symbol
let rvalue_dies_required_for t = t.rvalue_dies_required_for
let set_rvalue_dies_required_for t rvalue_dies_required_for =
  t.rvalue_dies_required_for <- rvalue_dies_required_for
let function_abstract_instances t = t.function_abstract_instances
let die_symbols_for_external_declarations t =
  t.die_symbols_for_external_declarations
let supports_call_sites t = t.supports_call_sites
let can_reference_dies_across_units t = t.can_reference_dies_across_units
let dwarf_version t = t.dwarf_version

let record_type_die_for_lifted_constant t symbol proto_die =
  Asm_symbol.Tbl.replace t.type_dies_for_lifted_constants symbol proto_die

let type_die_for_lifted_constant t symbol =
  match Asm_symbol.Tbl.find t.type_dies_for_lifted_constants symbol with
  | exception Not_found -> None
  | proto_die -> Some proto_die

let find_die_for_module_path t ~module_path =
  match Path.Tbl.find t.module_path_dies module_path with
  | exception Not_found -> None
  | proto_die -> Some proto_die

let record_die_for_module_path t ~module_path proto_die =
  Path.Tbl.replace t.module_path_dies module_path proto_die
