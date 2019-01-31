(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Construction of abbreviation tables from proto-DIEs together with
    flattening of the proto-DIE tree to a list of DIEs. *)

type result = {
  abbrev_table : Abbreviations_table.t;
  dies : Debugging_information_entry.t list;
  compilation_unit_die : Debugging_information_entry.t option;
  dwarf_4_location_lists : Dwarf_4_location_list.t list;
}

val run : proto_die_root:Proto_die.t -> result
