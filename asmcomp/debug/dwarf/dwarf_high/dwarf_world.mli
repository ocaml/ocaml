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

(** DWARF World - Main orchestrator for DWARF generation.

    The DWARF world manages the entire DWARF generation process for a
    compilation unit. It collects DIEs, manages tables (.debug_loc,
    .debug_ranges), assigns abbreviations, and emits all DWARF sections.

    This is the primary entry point for DWARF generation. *)

type t

(** Create a new DWARF world for a compilation unit *)
val create :
  producer:string ->
  comp_dir:string ->
  source_file:string ->
  language:Dwarf_language.t ->
  unit ->
  t

(** Add a top-level DIE (e.g., a function or global variable) *)
val add_die : t -> Proto_die.t -> unit

(** Add a location list and return its offset in .debug_loc *)
val add_location_list :
  t ->
  Location_list_entry.t list ->
  int

(** Add a range list and return its offset in .debug_ranges *)
val add_range_list :
  t ->
  Range_list_entry.t list ->
  int

(** Add a line number entry for source-level debugging *)
val add_line_number_entry :
  t ->
  address:Code_address.t ->
  file:string ->
  line:int ->
  column:int ->
  unit

(** Get the compilation unit DIE *)
val compilation_unit : t -> Proto_die.t

(** Get all top-level DIEs *)
val all_dies : t -> Proto_die.t list

(** Get the location list table *)
val location_list_table : t -> Location_list_table.t

(** Get the range list table *)
val range_list_table : t -> Range_list_table.t

(** Type DIE offsets for referencing standard types *)
type type_offsets = {
  ocaml_value : int;  (** Generic OCaml value type *)
  ocaml_int : int;    (** OCaml integer type *)
}

(** Add standard OCaml type DIEs to the world and return their offsets *)
val add_standard_types : t -> type_offsets

(** Get the stored type offsets (must call add_standard_types first) *)
val get_type_offsets : t -> type_offsets

(** Emit all DWARF sections to a buffer *)
type relocation = {
  offset : int;
  label : string;
}

type str_relocation = {
  offset : int;  (* Offset in .debug_info where relocation is needed *)
  str_offset : int;  (* Byte offset of string in .debug_str section *)
}

type section_data = {
  debug_info : bytes;
  debug_info_relocs : relocation list;
  debug_info_sec_offset_relocs : relocation list;  (* Section offset relocations (4-byte) *)
  debug_str_relocs : str_relocation list;
  debug_abbrev : bytes;
  debug_str : bytes;
  debug_str_labels : (string * (string * int)) list;  (* (label, (string, offset)) for emission *)
  debug_str_offsets : (bytes * str_relocation list) option;  (* DWARF 5: string offsets with relocations *)
  debug_line : (bytes * relocation list) option;  (* line table with address relocations *)
  line_table_label : string option;  (* Label for line table start *)
  debug_loc : bytes option;
  debug_ranges : bytes option;
}

(** Generate all DWARF section data *)
val emit : t -> section_data

(** Pretty-print the DWARF world state *)
val print : Format.formatter -> t -> unit
