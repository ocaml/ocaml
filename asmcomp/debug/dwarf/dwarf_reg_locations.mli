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

(** Construction of DWARF location descriptions for registers. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val reg_location_description
   : Reg.t
  -> offset_from_cfa_in_bytes:int
  -> need_rvalue:bool
  -> Simple_location_description.t option

val offset_from_cfa_in_bytes
   : Reg.t
  -> Reg.stack_location
  -> stack_offset:int
  -> int
