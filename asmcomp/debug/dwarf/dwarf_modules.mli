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

(** Create DWARF structures representing OCaml modules. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: improve function name *)
val dwarf
   : Dwarf_state.t
  -> module_path:Path.t
  -> Proto_die.t
