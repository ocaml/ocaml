(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Emission of DWARF information relating to lifted (statically allocated)
    constants. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** For dealing with Flambda [Let_symbol] bindings. *)
val dwarf_for_toplevel_constants
   : Dwarf_state.t
  -> Clambda.preallocated_constant list
  -> unit

(** For dealing with Flambda [Initialize_symbol] bindings. *)
val dwarf_for_toplevel_inconstants
   : Dwarf_state.t
  -> Clambda.preallocated_block list
  -> unit
