(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** DWARF-4 location lists. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

include Dwarf_emittable.S with type t := t

val create
   : location_list_entries:Dwarf_4_location_list_entry.t list
  -> t

val label : t -> Asm_label.t
val compare_increasing_vma : t -> t -> int
