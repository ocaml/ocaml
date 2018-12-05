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

(** Functor generating modules that handle DWARF location or range
    list tables. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (Location_or_range_list : Dwarf_emittable.S) : sig
  type t

  val create : unit -> t

  (** [Index.t] values are used in conjunction with DWARF attributes of
      forms [DW_FORM_loclistx] and [DW_FORM_rnglistx]. *)
  module Index : sig
    type t
    val to_uint64 : t -> Numbers.Uint64.t
  end

  val add : t -> Location_or_range_list.t -> Index.t

  val base_addr : t -> Asm_label.t

  include Dwarf_emittable.S with type t := t
end
