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

module Make (Location_or_range_list : sig
  include Dwarf_emittable.S
  val section : Asm_section.dwarf_section
end) : sig
  type t

  val create : unit -> t

  module Index : sig
    type t

    (** [to_uint64] is used in conjunction with DWARF attributes of
        forms [DW_FORM_loclistx] and [DW_FORM_rnglistx]. *)
    val to_uint64 : t -> Numbers.Uint64.t

    (** [to_uint64] is used in conjunction with DWARF attributes of
        form [DW_FORM_sec_offset].  Such attributes have to be used if offset
        arrays are not supported on the host system. *)
    val to_label : t -> Asm_label.t
  end

  val add : t -> Location_or_range_list.t -> Index.t

  val base_addr : t -> Asm_label.t

  include Dwarf_emittable.S with type t := t
end
