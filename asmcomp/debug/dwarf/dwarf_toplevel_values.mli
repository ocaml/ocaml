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

(** For dealing with constants lifted by [Closure]; and Flambda's [Let_symbol]
    bindings. *)
val dwarf_for_toplevel_constants
   : Dwarf_state.t
  -> Clambda.preallocated_constant list
  -> unit

(** For dealing with [Closure]'s top level module blocks.  The symbol for
    the module block and the corresponding variable must be provided. *)
val dwarf_for_closure_top_level_module_block
   : Dwarf_state.t
  -> module_block_sym:Backend_sym.t
  -> module_block_var:Backend_var.t
  -> unit
