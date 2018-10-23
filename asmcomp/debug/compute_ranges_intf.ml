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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module L = Linearize

module type S_subrange_state =
  type t

  val create : unit -> t
  val advance_over_instruction : t -> L.instruction -> t
end

module type S_subrange_info =
  type t
  type subrange_state

  val create : subrange_state -> t
end

module type S_range_info =
  type t
  type key

  val create : L.fundecl -> key -> start_insn:L.instruction -> t
end

module type S_functor = sig
  module Index : Identifiable.S

  module Key : Identifiable.S

  module Subrange_state : Compute_ranges_intf.S_subrange_state

  module Subrange_info : Compute_ranges_intf.S_subrange_info
    with type subrange_state := Subrange_state.t

  module Range_info : Compute_ranges_intf.S_range_info
    with type key := Key.t

  val available_before : L.instruction -> Key.Set.t

  val end_pos_offset
     : prev_insn:L.instruction option
    -> key:Key.t
    -> int option

  val maybe_restart_ranges
     : proto_births:Key.Set.t
    -> proto_deaths:Key.Set.t
    -> bool
end

module type S = sig
  type subrange_info
  type range_info
  type index

  module Available_subrange : sig
    type t

    val info : t -> subrange_info

    val start_pos : t -> Linearize.label
    val end_pos : t -> Linearize.label
    val end_pos_offset : t -> int option
  end

  module Available_range : sig
    type t

    val info : t -> range_info

    val extremities : t -> Linearize.label * Linearize.label

    val iter
       : t
      -> f:(available_subrange:Available_subrange.t -> unit)
      -> unit

    val fold
       : t
      -> init:'a
      -> f:('a -> available_subrange:Available_subrange.t -> 'a)
      -> 'a
  end

  type t

  val create : Linearize.fundecl -> t * Linearize.fundecl

  val find : t -> index -> Available_range.t option

  val iter : t -> f:(index -> Available_range.t -> unit) -> unit

  val fold : t -> init:'a -> f:('a -> index -> Available_range.t -> 'a) -> 'a
end
