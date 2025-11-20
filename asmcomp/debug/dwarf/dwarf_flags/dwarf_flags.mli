(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Joel Reymont                                     *)
(*                                                                        *)
(*   Copyright 2024 Joel Reymont                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Configuration flags for DWARF emission. *)

(** DWARF fidelity mode *)
type dwarf_fidelity = Clflags.dwarf_fidelity =
  | Upstream_compatible  (** Emit minimal DWARF compatible with upstream OCaml *)
  | Enhanced             (** Emit full DWARF debugging information *)

(** DWARF fission/split-debug mode *)
type dwarf_fission = Clflags.dwarf_fission =
  | Fission_none         (** No fission *)
  | Fission_split        (** Split debug info into separate file *)

(** Get the current DWARF fidelity setting *)
val dwarf_fidelity : unit -> dwarf_fidelity option

(** Check if DWARF emission is enabled *)
val is_dwarf_enabled : unit -> bool

(** Check if enhanced DWARF mode is active *)
val is_enhanced_dwarf : unit -> bool

(** Check if upstream-compatible DWARF mode is active *)
val is_upstream_compatible : unit -> bool

(** Check if inlined frames should be emitted *)
val emit_inlined_frames : unit -> bool

(** Check if DWARF may alter codegen for better debugging *)
val dwarf_may_alter_codegen : unit -> bool

(** Get maximum function complexity for DWARF generation *)
val max_function_complexity : unit -> int option

(** Get DWARF compression format *)
val compression_format : unit -> string

(** Get DWARF fission mode *)
val fission_mode : unit -> dwarf_fission

(** Check if startup file DWARF should be emitted *)
val emit_dwarf_for_startup : unit -> bool

(** Configuration limits for type shape processing *)

(** Maximum depth for shape reduction *)
val shape_reduce_depth : unit -> int

(** Maximum depth for shape evaluation *)
val shape_eval_depth : unit -> int

(** Maximum CMS files per compilation unit *)
val max_cms_files_per_unit : unit -> int

(** Maximum CMS files per variable *)
val max_cms_files_per_variable : unit -> int

(** Maximum depth for type-to-shape conversion *)
val max_type_to_shape_depth : unit -> int

(** Maximum shape reduce steps per variable *)
val max_shape_reduce_steps_per_variable : unit -> int

(** Maximum evaluation steps per variable *)
val max_evaluation_steps_per_variable : unit -> int

(** Shape reduce fuel limit *)
val shape_reduce_fuel : unit -> int

(** Debug output flags *)

(** Check if DWARF type debug output is enabled *)
val debug_dwarf_types : unit -> bool

(** Check if DWARF metrics output is enabled *)
val debug_dwarf_metrics : unit -> bool

(** DWARF debug granularity levels *)
type dwarf_debug_granularity =
  | Debug_dwarf_cfi         (** Call frame information *)
  | Debug_dwarf_loc         (** Location information *)
  | Debug_dwarf_functions   (** Function information *)
  | Debug_dwarf_scopes      (** Scope information *)
  | Debug_dwarf_vars        (** Variable information *)
  | Debug_dwarf_call_sites  (** Call site information *)

(** Check if a specific debug granularity level is enabled *)
val is_debug_granularity_enabled : dwarf_debug_granularity -> bool
