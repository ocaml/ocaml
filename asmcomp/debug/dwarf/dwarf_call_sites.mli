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

(** Generation of descriptions of function call sites in DWARF. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val dwarf
   : Dwarf_state.t
  -> scope_proto_dies:Proto_die.t Debuginfo.Block.Map.t
  -> Linearize.fundecl
  -> external_calls_generated_during_emit
       : Emitaux.external_call_generated_during_emit list
  -> function_symbol:Asm_symbol.t
  -> function_proto_die:Proto_die.t
  -> bool
