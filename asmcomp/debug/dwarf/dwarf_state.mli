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

(** State that is shared amongst the various dwarf_* modules. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val create
   : compilation_unit_header_label:Asm_label.t
  -> compilation_unit_proto_die:Proto_die.t
  -> value_type_proto_die:Proto_die.t
  -> naked_float_type_proto_die:Proto_die.t
  -> start_of_code_symbol:Asm_symbol.t
  -> end_of_code_symbol:Asm_symbol.t
  -> Address_table.t
  -> Debug_loc_table.t
  -> Debug_ranges_table.t
  -> Location_list_table.t
  -> Range_list_table.t
  -> Dwarf_version.t
  -> t

val compilation_unit_header_label : t -> Asm_label.t

val compilation_unit_proto_die : t -> Proto_die.t

val value_type_proto_die : t -> Proto_die.t

val naked_float_type_proto_die : t -> Proto_die.t

val address_table : t -> Address_table.t

val debug_loc_table : t -> Debug_loc_table.t

val debug_ranges_table : t -> Debug_ranges_table.t

val location_list_table : t -> Location_list_table.t

val range_list_table : t -> Range_list_table.t

val start_of_code_symbol : t -> Asm_symbol.t

val end_of_code_symbol : t -> Asm_symbol.t

val rvalue_dies_required_for : t -> Backend_var.Set.t

val set_rvalue_dies_required_for : t -> Backend_var.Set.t -> unit

val function_abstract_instances
   : t
  -> (Proto_die.t * Asm_symbol.t) Debuginfo.Function.Id.Tbl.t

val die_symbols_for_external_declarations : t -> Asm_symbol.t Asm_symbol.Tbl.t

val supports_call_sites : t -> bool

val can_reference_dies_across_units : t -> bool

val dwarf_version : t -> Dwarf_version.t

val record_type_die_for_lifted_constant
   : t
  -> Asm_symbol.t
  -> Proto_die.t
  -> unit

val type_die_for_lifted_constant
   : t
  -> Asm_symbol.t
  -> Proto_die.t option

val find_die_for_module_path
   : t
  -> module_path:Path.t
  -> Proto_die.t option

val record_die_for_module_path
   : t
  -> module_path:Path.t
  -> Proto_die.t
  -> unit
