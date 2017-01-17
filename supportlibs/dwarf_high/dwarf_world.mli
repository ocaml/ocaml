(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helper for emitting the various DWARF sections required for full
    debugging information. *)

val emit
   : compilation_unit_proto_die:Proto_die.t
  -> start_of_code_symbol:Symbol.t
  -> end_of_code_symbol:Symbol.t
  -> compilation_unit_header_label:Cmm.label
  -> debug_loc_table:Debug_loc_table.t
  -> (module Asm_directives.S)
  -> unit
