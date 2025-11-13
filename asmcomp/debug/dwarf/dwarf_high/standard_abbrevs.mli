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

(** Standard abbreviation table for OCaml DWARF.

    Provides a fixed set of abbreviation codes that all compilation
    units share, solving the multi-CU abbreviation offset issue. *)

(** Standard abbreviation entry *)
type standard_entry = {
  code : int;
  tag : Dwarf_tag.t;
  has_children : bool;
  attributes : (Dwarf_attributes.t * Dwarf_form.t) list;
}

(** The standard abbreviation table used by all OCaml modules *)
val standard_table : standard_entry list

(** Get the abbreviation code for a DIE.
    Matches the DIE against standard entries and returns the code.
    Raises Failure if no match found. *)
val get_code_for_die : Proto_die.t -> int

(** Emit the standard abbreviation table as bytes.
    This is identical for all compilation units. *)
val emit_standard_table : unit -> bytes

(** Check if a DIE matches one of the standard entries *)
val is_standard_die : Proto_die.t -> bool
