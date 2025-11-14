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

(** Main entry point for DWARF debugging information generation.

    This module provides the top-level API for generating DWARF debugging
    information for OCaml native code compilation. It is called from the
    native code backend during assembly emission. *)

(** DWARF generation state for a compilation unit *)
type t

(** Initialize DWARF generation for a compilation unit.

    Parameters:
    - source_file: The original .ml source file
    - compilation_dir: The directory where compilation occurred
    - producer: Compiler version string
    - address_size: Target architecture address size in bytes (4 for 32-bit, 8 for 64-bit)
*)
val create :
  source_file:string ->
  compilation_dir:string ->
  producer:string ->
  address_size:int ->
  unit ->
  t

(** Add a function to the DWARF information.

    This is a simplified version for Phase 2 - full implementation
    will come in Phase 4 with complete type information.

    Parameters:
    - t: DWARF state
    - name: Function name
    - start_address: Function start label/address
    - end_address: Function end label/address
*)
val add_function :
  t ->
  name:string ->
  start_address:Code_address.t ->
  end_address:Code_address.t ->
  unit

(** Add a line number entry for source-level debugging.

    Parameters:
    - t: DWARF state
    - address: Instruction address (label or absolute)
    - file: Source file name
    - line: Line number (1-indexed)
    - column: Column number (1-indexed, 0 for unknown)
*)
val add_line_number :
  t ->
  address:Code_address.t ->
  file:string ->
  line:int ->
  column:int ->
  unit

(** Add a variable to the current function.

    Phase 5A: This adds a variable (parameter or local) to the most recently
    added function. For now, this tracks initial location only.

    Parameters:
    - t: DWARF state
    - name: Variable name
    - location: Variable location (register or stack)
    - is_parameter: true if this is a function parameter, false for local
*)
val add_variable :
  t ->
  name:string ->
  location:Variable_location.location ->
  is_parameter:bool ->
  unit

(** Emit all DWARF sections.

    Returns section data that can be written to the object file.
*)
val emit : t -> Dwarf_world.section_data

(** Check if DWARF emission is enabled *)
val is_enabled : unit -> bool

(** Get the DWARF world (for debugging) *)
val world : t -> Dwarf_world.t

(** Pretty-print DWARF state *)
val print : Format.formatter -> t -> unit
