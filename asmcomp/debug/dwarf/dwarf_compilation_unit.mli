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

(** Construction of DWARF "compile_unit" DIEs and associated entities. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val compile_unit_proto_die
   : sourcefile:string
  -> prefix_name:string
  -> start_of_code_symbol:Asm_symbol.t
  -> end_of_code_symbol:Asm_symbol.t
  -> Address_table.t
  -> Location_list_table.t
  -> Range_list_table.t
  -> Proto_die.t
