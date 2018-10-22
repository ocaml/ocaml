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

module type Subrange_state_intf = sig
  type t

  val create : unit -> t
  val advance_over_instruction : t -> L.instruction -> t
end

module type Subrange_info_intf = sig
  type t
  type subrange_state

  val create : subrange_state -> t
end

module type S = sig
  type subrange_info
  type subrange_state
  type range_info
  type range_uniqueness
  type range_index

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

  val find : t -> range_index -> Available_range.t option

  val fold
     : t
    -> init:'a
    -> f:('a
      -> range_index
      -> range_uniqueness
      -> Available_range.t
      -> 'a)
    -> 'a
end
