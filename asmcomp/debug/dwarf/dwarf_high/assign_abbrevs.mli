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

(** Assign abbreviation codes to DIEs.

    DWARF uses abbreviation codes to compress repeated patterns of
    tags and attributes. This module analyzes a tree of proto_dies
    and assigns unique abbreviation codes to each unique combination
    of tag, has_children flag, and attribute signatures.

    The abbreviation table is emitted to the .debug_abbrev section. *)

(** A DIE with an assigned abbreviation code *)
type abbrev_die = {
  abbrev_code : int;
  proto_die : Proto_die.t;
}

(** An abbreviation table entry *)
type abbrev_entry = {
  code : int;
  tag : Dwarf_tag.t;
  has_children : bool;
  attributes : (Dwarf_attributes.t * Dwarf_form.t) list;
}

(** Abbreviation table *)
type abbrev_table = {
  entries : abbrev_entry list;
}

(** Assign abbreviation codes to a proto_die tree.
    Returns the tree with abbreviation codes assigned and the abbreviation table. *)
val assign : Proto_die.t -> abbrev_die * abbrev_table

(** Assign abbreviation codes to multiple root DIEs.
    Returns all DIEs with codes assigned and a single abbreviation table. *)
val assign_multi : Proto_die.t list -> abbrev_die list * abbrev_table

(** Get the abbreviation code for a DIE *)
val abbrev_code : abbrev_die -> int

(** Get the original proto_die *)
val proto_die : abbrev_die -> Proto_die.t

(** Get the children of an abbrev_die *)
val children : abbrev_die -> abbrev_die list

(** Pretty-print an abbreviation table *)
val print_abbrev_table : Format.formatter -> abbrev_table -> unit

(** Pretty-print an abbrev_die *)
val print : Format.formatter -> abbrev_die -> unit
