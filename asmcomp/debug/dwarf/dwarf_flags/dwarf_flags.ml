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

type dwarf_fidelity = Clflags.dwarf_fidelity =
  | Upstream_compatible
  | Enhanced

type dwarf_fission = Clflags.dwarf_fission =
  | Fission_none
  | Fission_split

let dwarf_fidelity () = !Clflags.gdwarf_fidelity

let is_dwarf_enabled () =
  match !Clflags.debug, dwarf_fidelity () with
  | true, Some _ -> true
  | _ -> false

let is_enhanced_dwarf () =
  match dwarf_fidelity () with
  | Some Enhanced -> true
  | _ -> false

let is_upstream_compatible () =
  match dwarf_fidelity () with
  | Some Upstream_compatible -> true
  | _ -> false

let emit_inlined_frames () =
  is_enhanced_dwarf () && !Clflags.dwarf_inlined_frames

let dwarf_may_alter_codegen () =
  is_enhanced_dwarf () && !Clflags.dwarf_may_alter_codegen

let max_function_complexity () =
  !Clflags.dwarf_max_function_complexity

let compression_format () =
  !Clflags.dwarf_compression

let fission_mode () =
  !Clflags.dwarf_fission

let emit_dwarf_for_startup () =
  !Clflags.emit_dwarf_for_startup

(* Configuration limits *)

let shape_reduce_depth () =
  !Clflags.gdwarf_config_shape_reduce_depth

let shape_eval_depth () =
  !Clflags.gdwarf_config_shape_eval_depth

let max_cms_files_per_unit () =
  !Clflags.gdwarf_config_max_cms_files_per_unit

let max_cms_files_per_variable () =
  !Clflags.gdwarf_config_max_cms_files_per_variable

let max_type_to_shape_depth () =
  !Clflags.gdwarf_config_max_type_to_shape_depth

let max_shape_reduce_steps_per_variable () =
  !Clflags.gdwarf_config_max_shape_reduce_steps_per_variable

let max_evaluation_steps_per_variable () =
  !Clflags.gdwarf_config_max_evaluation_steps_per_variable

let shape_reduce_fuel () =
  !Clflags.gdwarf_config_shape_reduce_fuel

(* Debug output *)

let debug_dwarf_types () =
  !Clflags.ddwarf_types

let debug_dwarf_metrics () =
  !Clflags.ddwarf_metrics

type dwarf_debug_granularity =
  | Debug_dwarf_cfi
  | Debug_dwarf_loc
  | Debug_dwarf_functions
  | Debug_dwarf_scopes
  | Debug_dwarf_vars
  | Debug_dwarf_call_sites

let is_debug_granularity_enabled _granularity =
  (* This would check against a list/set of enabled granularities
     For now, return false as we haven't implemented granularity tracking yet *)
  false
