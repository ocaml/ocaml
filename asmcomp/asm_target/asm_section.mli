(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Representation of object file sections. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Sections that hold DWARF debugging information. *)
type dwarf_section =
  | Debug_info
  | Debug_abbrev
  | Debug_aranges
  | Debug_loc
  | Debug_str
  | Debug_line

(** The linker may share constants in [Eight_byte_literals] and
    [Sixteen_byte_literals] sections. *)
type t =
  | Text
  | Data
  | Read_only_data
  | Eight_byte_literals
  | Sixteen_byte_literals
  | Jump_tables
  | DWARF of dwarf_section

val all_sections_in_order : t list

(** Whether the section holds code. *)
val section_is_text : t -> bool

type flags_for_section = private {
  names : string list;
  flags : string option;
  args : string list;
}

(** The necessary information for a section directive.  [first_occurrence]
    should be [true] iff the corresponding directive will be the first such
    in the relevant assembly file for the given section. *)
val flags : t -> first_occurrence:bool -> flags_for_section

(** Retrieve a distinguished label that is suitable for use at the top of
    the given section.

    Aside: Why do we need labels at the start of sections rather than
    just referencing sections directly?
    They are necessary so that references (e.g. DW_FORM_ref_addr or
    DW_FORM_sec_offset when emitting DWARF) to places that are currently
    at the start of these sections get relocated correctly when those
    places become not at the start (e.g. during linking). *)
val label : t -> Asm_label.t
