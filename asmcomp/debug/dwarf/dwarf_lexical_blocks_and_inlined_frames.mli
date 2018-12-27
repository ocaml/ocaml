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

(** Generation of descriptions of lexical blocks and inlined frames in
    DWARF. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val find_scope_die_from_debuginfo
   : Debuginfo.t
  -> function_proto_die:Proto_die.t
  -> scope_proto_dies:Proto_die.t Debuginfo.Block.Map.t
  -> Proto_die.t option

val dwarf
   : Dwarf_state.t
  -> Linearize.fundecl
  -> Lexical_block_ranges.t
  -> function_proto_die:Proto_die.t
  -> unit
