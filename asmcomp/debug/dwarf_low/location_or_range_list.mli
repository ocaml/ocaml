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

(** Functor for the production of DWARF location or range list modules. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (Entry : Location_or_range_list_entry.S) : sig
  type t

  val create : unit -> t

  val add : t -> Entry.t -> t

  val section : Asm_section.dwarf_section

  include Dwarf_emittable.S with type t := t
end
