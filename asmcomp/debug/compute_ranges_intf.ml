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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** This file defines types that are used to specify the interface of
    [Compute_ranges].  The description of [Compute_ranges] is:

      "Coalescing of per-instruction information into possibly-discontiguous
       regions of code delimited by labels. This is used for collating register
       availability and lexical block scoping information into a concise form."

    [Compute_ranges] defines a functor, whose argument has type [S_functor], and
    whose result has type [S]. Both [S_functor] and [S] are defined here.

    It is suggested that those unfamiliar with this module start by reading
    the documentation on module type [S], below.
*)

module L = Linear

(** The type of caller-defined contextual state associated with subranges.
    This may be used to track information throughout the range-computing
    process. *)
module type S_subrange_state = sig
  type t

  val create : unit -> t
  val advance_over_instruction : t -> L.instruction -> t
end

(** The type of caller-defined information associated with subranges. *)
module type S_subrange_info = sig
  type t
  type key
  type subrange_state

  val create : key -> subrange_state -> t
end

(** The type of caller-defined information associated with ranges. *)
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

(** This module type specifies what the caller has to provide in order to
    instantiate a module to compute ranges. *)
module type S_functor = sig
  (** The module [Index] is used to filter and group the generated subranges.
      Inclusion of a computed subrange in the result is conditional upon the
      existence of an index that can be associated to it. To give a concrete
      example, the keys associated to ranges might be pseudoregisters, and the
      indexes variable names (c.f. [Available_ranges_vars]). Every register that
      is not known to hold the value of some variable is dropped from the
      result.

      As the name suggests, values of type [Index.t] also serve as indices for
      accessing ranges in the result. The result may actually contain no
      reference to keys (only [Subrange_info.t] may reliably contain it), and
      subranges with different keys will be coalesced into a single range if all
      their keys are associated to the same index. *)
  module Index : Identifiable.S

  (** The module [Key] corresponds to the identifiers that define the ranges in
      [Linear] instructions. Each instruction should have two sets of keys,
      [available_before] and [available_across], with accessor functions of
      these names being provided to retrieve them. The notion of "availability"
      is not prescribed. The availability sets are used to compute subranges
      associated to each key. *)
  module Key : sig
    (** The type of identifiers that define ranges. *)
    type t

    module Set : sig
      include Set.S with type elt = t
      val print : Format.formatter -> t -> unit
    end

    module Map : Map.S with type key = t

    (** Print a representation (typically sexp) of the given key to the given
        formatter. *)
    val print : Format.formatter -> t -> unit

    (** In some situations, for performance reasons, an "available" set may only
        contain a subset of all keys that need to be tracked. For example, when
        using a notion of availability that describes which lexical block a
        given instruction lies in, using a standard notion of nested lexical
        blocks, the innermost lexical block uniquely determines the chain of its
        parents. (This is exploited in [Lexical_block_ranges].) The
        [all_parents] function must return, given an "available" [key], all
        those other keys that are also available and uniquely determined by
        [key]. *)
    val all_parents : t -> t list
  end

  (** The module [Range_info] is used to store additional information on a range
      that is associated to a range at its creation and can be retrieved from
      the result. The association between keys and indices is also done here:
      [Range_info.create] serves both as a map between keys and indices; and
      also as the creator of the [Range_info.t] structure. When several
      subranges are contained in a single range, the associated [Range_info.t]
      will correspond to the first closed subrange. *)
  module Range_info : S_range_info
    with type key := Key.t
    with type index := Index.t

  (** The module [Subrange_state] describes information that needs to be
      propagated and passed to [Subrange_info.create]. The state that will be
      used for subrange creation is the state at the end of the subrange, not at
      the beginning. *)
  module Subrange_state : S_subrange_state

  (** The module [Subrange_info] has a similar purpose to [Range_info], but for
      subranges. Its distinguishing property is that it can store information
      about its context using the additional [subrange_state] parameter of its
      [create] function. *)
  module Subrange_info : S_subrange_info
    with type key := Key.t
    with type subrange_state := Subrange_state.t

  (** How to retrieve from an instruction those keys that are available
      immediately before the instruction starts executing. *)
  val available_before : L.instruction -> Key.Set.t

  (** How to retrieve from an instruction those keys that are available
      between the points at which the instruction reads its arguments and
      writes its results. *)
  val available_across : L.instruction -> Key.Set.t

  (** This [must_restart_ranges_upon_any_change] boolean exists because some
      consumers of the range information may require that two subranges are
      disjoint rather than including one in another. When this function returns
      [true], whenever a subrange is opened or closed, all other overlapping
      subranges will be split in two at the same point. *)
  val must_restart_ranges_upon_any_change : unit -> bool
end

(** This module type is the result type of the [Compute_ranges.Make] functor.

    The _ranges_ being computed are composed of contiguous _subranges_ delimited
    by two labels (of type [Linear.label]). These labels will be added by
    this pass to the code being inspected, which is why the [create] function in
    the result of the functor returns not only the ranges but also the updated
    function with the labels added. The [start_pos_offset] and [end_pos_offset]
    components of the subranges are there to allow a distinction between ranges
    starting (or ending) right at the start of the corresponding instruction
    (offset of zero), and ranges starting or ending one byte after the actual
    instruction (offset of one). *)
module type S = sig
  (** Corresponds to [Index] in the [S_functor] module type. *)
  module Index : Identifiable.S

  (** Corresponds to [Key] in the [S_functor] module type. *)
  module Key : sig
    type t
    module Set : Set.S with type elt = t
    module Map : Map.S with type key = t
  end

  (** Corresponds to [Subrange_state] in the [S_functor] module type. *)
  module Subrange_state : S_subrange_state

  (** Corresponds to [Subrange_info] in the [S_functor] module type. *)
  module Subrange_info : S_subrange_info
    with type key := Key.t
    with type subrange_state := Subrange_state.t

  (** Corresponds to [Range_info] in the [S_functor] module type. *)
  module Range_info : S_range_info
    with type key := Key.t
    with type index := Index.t

  module Subrange : sig
    (** The type of subranges.  Each subrange is a contiguous region of
        code delimited by labels. *)
    type t

    (** The caller's information about the subrange. *)
    val info : t -> Subrange_info.t

    (** The label at the start of the range. *)
    val start_pos : t -> Linear.label

    (** How many bytes from the label at [start_pos] the range actually
        commences.  If this value is zero, then the first byte of the range
        has the address of the label given by [start_pos]. *)
    val start_pos_offset : t -> int

    (** The label at the end of the range. *)
    val end_pos : t -> Linear.label

    (** Like [start_pos_offset], but analogously for the end of the range. (The
        sense is not inverted; a positive [end_pos_offset] means the range ends
        at an address higher than the address of the [end_pos], just like a
        positive [start_pos_offset] means the range starts at an address higher
        than the [start_pos]. *)
    val end_pos_offset : t -> int
  end

  module Range : sig
    (** The type of ranges.  Each range is a list of subranges, so a
        possibly-discontiguous region of code. *)
    type t

    (** The caller's information about the range. *)
    val info : t -> Range_info.t

    (** Estimate the pair of ([start_pos], [start_pos_offset]) (c.f. [Subrange],
        above) found amongst the given ranges that yields the lowest machine
        address. The assumption is made that no [start_pos_offset] or
        [end_pos_offset] will cause the corresponding extremity of a range to
        cross an extremity of any other range. (This should be satisfied in
        typical uses because the offsets are typically zero or one.) If there
        are no ranges supplied then [None] is returned. *)
    val estimate_lowest_address : t -> (Linear.label * int) option

    (** Fold over all subranges within the given range. *)
    val fold
       : t
      -> init:'a
      -> f:('a -> Subrange.t -> 'a)
      -> 'a
  end

  (** The type holding information on computed ranges. *)
  type t

  (** A value of type [t] that holds no range information. *)
  val empty : t

  (** Compute ranges for the code in the given linearized function
      declaration, returning the ranges as a value of type [t] and the
      rewritten code that must go forward for emission. *)
  val create : Linear.fundecl -> t * Linear.fundecl

  (** Iterate through ranges.  Each range is associated with an index. *)
  val iter : t -> f:(Index.t -> Range.t -> unit) -> unit

  (** Like [iter], but a fold. *)
  val fold : t -> init:'a -> f:('a -> Index.t -> Range.t -> 'a) -> 'a

  (** Find the range for the given index, or raise an exception. *)
  val find : t -> Index.t -> Range.t

  (** All indexes for which the given value of type [t] contains ranges. *)
  val all_indexes : t -> Index.Set.t

  (** An internal function used by [Coalesce_labels].
      The [env] should come from [Coalesce_labels.fundecl]. *)
  val rewrite_labels_and_remove_empty_subranges_and_ranges
     : t
    -> env:int Numbers.Int.Map.t
    -> t
end
