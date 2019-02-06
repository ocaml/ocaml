(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Representation of object file sections. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Sections that hold DWARF debugging information.  See:
       http://dwarfstd.org/doc/DWARF4.pdf
       http://dwarfstd.org/doc/DWARF5.pdf
*)
type dwarf_section =
  | Debug_info
    (** The debugging information entries. *)
  | Debug_abbrev
    (** The abbreviations table. *)
  | Debug_aranges
    (** The address ranges table. *)
  | Debug_addr
    (** The address table (DWARF-5 only). *)
  | Debug_loc
    (** The table of location lists (DWARF-4 and earlier only). *)
  | Debug_ranges
    (** The table of range lists (DWARF-4 and earlier only). *)
  | Debug_loclists
    (** The table of location lists (DWARF-5 only). *)
  | Debug_rnglists
    (** The table of range lists (DWARF-5 only). *)
  | Debug_str
    (** The string table. *)
  | Debug_line
    (** The line number table. *)

(** The linker may share constants in [Eight_byte_literals] and
    [Sixteen_byte_literals] sections. *)
type t =
  | Text
    (** The code section. *)
  | Data
    (** The read/write data section. *)
  | Read_only_data
    (** The read-only data section. *)
  | Eight_byte_literals
    (** A section containing eight-byte constants (for example unboxed floating
        point numbers). *)
  | Sixteen_byte_literals
    (** A section containing sixteen-byte numeric constants. *)
  | Jump_tables
    (** A section containing the code of jump tables. *)
  | DWARF of dwarf_section
    (** The sections holding DWARF debugging information. *)

(** Printing, comparison, sets, maps, etc. *)
include Identifiable.S with type t := t

(** Concise description of values of type [t] for error messages, etc. *)
val to_string : t -> string

(** All sections that this code knows about. *)
val all_sections_in_order : unit -> t list

(** [section_is_text] returns [true] if the section holds code. *)
val section_is_text : t -> bool

(** Details about a specific section, used for assembly emission. *)
type flags_for_section = private {
  names : string list;
  (** The name of the section.  Sometimes (e.g. on macOS for DWARF sections)
      this has multiple parts. *)
  flags : string option;
  (** Target-dependent flags for the section. *)
  args : string list;
  (** Target-dependent arguments for the section. *)
}

(** The necessary information for an assembler's ".section" directive.

    [first_occurrence] should be [true] iff the corresponding directive will
    be the first such in the relevant assembly file for the given section. *)
val flags : t -> first_occurrence:bool -> flags_for_section
