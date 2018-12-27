(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Construction of abbreviation tables from proto-DIEs. *)

type result = {
  abbrev_table : Abbreviations_table.t;
  dies : Debugging_information_entry.t list;
  compilation_unit_die : Debugging_information_entry.t option;
}

val run : proto_die_root:Proto_die.t -> result
