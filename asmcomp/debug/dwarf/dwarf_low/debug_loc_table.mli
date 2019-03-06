(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Representation of the DWARF .debug_loc table. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

include Dwarf_emittable.S with type t := t

val create : unit -> t

val insert
   : t
  -> location_list:Location_list.t
  -> Dwarf_attribute_values.Attribute_value.t
