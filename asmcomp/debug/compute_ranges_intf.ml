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

(* Documentation

   This file defines types that are used to specify the interface of
   Compute_ranges. Compute_ranges defines a functor, whose argument
   has type S_functor, and whose result has type S. Both S_functor and S
   are defined here.

   The ranges being computed are basically composed of contiguous
   subranges, delimited by two labels (of type Linearize.label). These
   labels will actually be added to the code being inspected, which is
   why the create function in the result of the functor returns not only
   the ranges but also the updated function with the labels added.
   The start_pos_offset and end_pos_offset components of the subranges
   are there to allow a distinction between ranges starting (or ending)
   right at the start of the corresponding instruction (offset of 0),
   and ranges starting or ending one byte after the actual instruction
   (offset of one).

   The must_restart_ranges_upon_any_change boolean is there because
   some consumers of the ranges information may require that two
   subranges are either disjoint or included one in the other.
   When this function returns true, whenever a subrange is opened or
   closed, all other overlapping subranges will be split in two at the
   same point.

   S_functor requires five modules to be provided, plus three functions.

   The module Key corresponds to the low-level identifiers that define
   the ranges in Linear instructions.
   Each instruction is supposed to have two sets of keys,
   available_before and available_across, that can be retrieved using the
   eponymous functions.
   These sets are then used to cimpute subranges associated to each key.
   The Key interface also contains a function all_parents, to represent
   the fact that for performance reasons, an available set may only
   contain a subset of the real keys the need to be tracked, but
   collecting the subset with the result of calling all_parents on
   all the elements of the subset retrieves the whole set (as an
   example, storing only the leaves of a tree while computing ranges
   for all the nodes).

   The module Index is used to filter and group the generated subranges.
   Inclusion of a computed subrange in the result is conditionned to
   the existence of an index that can be associated to it.
   To give a concrete example, the keys associated to ranges can be
   virtual registers, and the index variable names. Every register that
   cannot be associated to a variable is dropped from the result.
   As its name suggests, values of type Index.t also serve as indices
   for accessing ranges in the result.
   The result may actually contain no reference to keys (only
   subrange_info may reliably contain it), and subranges with different
   keys will be agglomerated in a single range if all their keys are
   associated to the same index.

   The module Range_info is used to store additional information on a
   range, that is associated to a range at its creation and can be
   retrieved from the result.
   The association between keys and indices is also done here:
   Range_info.create serves both as mapper between keys and indices
   and actual creator of the Range_info.t structure.
   When several subranges are contained in a single range, the associated
   range_info will correspond to the first closed subrange.

   The module Subrange_info has a similar purpose to Range_info, but
   for subranges. Its main particularity is that it can store
   information about its context using the additional subrange_state
   parameter of its create function.

   The module Subrange_state describes the kind of information
   that needs to be propagated and passed to Subrange_info.create.
   The state that will be used for subrange creation is the state
   at the end of the subrange, not at the beginning.
*)

module L = Linearize

module type S_subrange_state = sig
  type t

  val create : unit -> t
  val advance_over_instruction : t -> L.instruction -> t
end

module type S_subrange_info = sig
  type t
  type key
  type subrange_state

  val create : key -> subrange_state -> t
end

module type S_range_info = sig
  type t
  type key
  type index

  val create
     : L.fundecl
    -> key
    -> start_insn:L.instruction
    -> (index * t) option
end

module type S_functor = sig
  module Index : Identifiable.S

  module Key : sig
    type t

    module Set : sig
      include Set.S with type elt = t
      val print : Format.formatter -> t -> unit
    end

    module Map : Map.S with type key = t

    val print : Format.formatter -> t -> unit

    val all_parents : t -> t list
  end

  module Subrange_state : S_subrange_state

  module Subrange_info : S_subrange_info
    with type key := Key.t
    with type subrange_state := Subrange_state.t

  module Range_info : S_range_info
    with type key := Key.t
    with type index := Index.t

  val available_before : L.instruction -> Key.Set.t

  val available_across : L.instruction -> Key.Set.t

  val must_restart_ranges_upon_any_change : unit -> bool
end

module type S = sig
  module Index : Identifiable.S

  module Key : sig
    type t
    module Set : Set.S with type elt = t
    module Map : Map.S with type key = t
  end

  module Subrange_state : S_subrange_state

  module Subrange_info : S_subrange_info
    with type key := Key.t
    with type subrange_state := Subrange_state.t

  module Range_info : S_range_info
    with type key := Key.t
    with type index := Index.t

  module Subrange : sig
    type t

    val info : t -> Subrange_info.t

    val start_pos : t -> Linearize.label
    val start_pos_offset : t -> int
    val end_pos : t -> Linearize.label
    val end_pos_offset : t -> int
  end

  module Range : sig
    type t

    val info : t -> Range_info.t

    val extremities : t -> Linearize.label * Linearize.label

    val lowest_address : t -> Linearize.label option

    val fold
       : t
      -> init:'a
      -> f:('a -> Subrange.t -> 'a)
      -> 'a
  end

  type t

  val empty : t

  val create : Linearize.fundecl -> t * Linearize.fundecl

  val iter : t -> f:(Index.t -> Range.t -> unit) -> unit

  val fold : t -> init:'a -> f:('a -> Index.t -> Range.t -> 'a) -> 'a

  val find : t -> Index.t -> Range.t

  val all_indexes : t -> Index.Set.t

  (** The [env] should come from [Coalesce_labels.fundecl]. *)
  val rewrite_labels_and_remove_empty_subranges_and_ranges
     : t
    -> env:int Numbers.Int.Map.t
    -> t
end
