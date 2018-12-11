(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helper for emitting the various DWARF sections required for full
    debugging information. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val emit
   : compilation_unit_proto_die:Proto_die.t
  -> start_of_code_symbol:Asm_symbol.t
  -> end_of_code_symbol:Asm_symbol.t
  -> compilation_unit_header_label:Asm_label.t
  -> debug_loc_table:Debug_loc_table.t
  -> debug_ranges_table:Debug_ranges_table.t
  -> address_table:Address_table.t
  -> location_list_table:Location_list_table.t
  -> range_list_table:Range_list_table.t
  -> unit
