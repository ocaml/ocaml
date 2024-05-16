(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Jean-Christophe Filliâtre                        *)
(*                                                                        *)
(*   Copyright 2023 CNRS                                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Priority queues.

    The {!Pqueue} module implements a data structure of priority queues,
    given a totally ordered type for priorities. This is a mutable
    data structure. Both min- and max-priority queues are provided.

    The implementation uses a heap stored in a dynamic array, and is
    therefore reasonably efficient: accessing the minimum
    (resp. maximum) element takes constant time, and insertion and
    removal take time logarithmic in the size of the priority
    queue. Note that [of_array] runs in linear time (and thus must be
    preferred to repeated insertions with [add]).

    It is fine to have several elements with the same priority.
    It is guaranteed that the element returned by [min_elt] (or
    [min_elt_opt]) is the one that is removed from the priority queue
    by [pop_min] (or [pop_min_opt] or [remove_min]). This is important
    in many algorithms, (e.g. when peeking at several priority queues
    and then selecting one to remove from).

    @since 5.3
*)

module type OrderedType =
  sig
    type t
      (** The type of priorities. *)

    val compare : t -> t -> int
      (** A total ordering function to compare priorities.

          This is a two-argument function [f] such that [f p1 p2] is
          zero if the priorities [p1] and [p2] are equal, [f p1 p2] is
          strictly negative if [p1] is smaller than [p2], and [f p1
          p2] is strictly positive if [p1] is greater than [p2].

          The generic structural comparison function {!Stdlib.compare}
          is a suitable ordering function for priority types such as
          [int] or [string]. *)
  end
(** Input signature of the functors {!MakeMinPriority} and
    {!MakeMaxPriority}. *)

module type Min =
  sig

    (** {1:pqueue Min-priority queues} *)

    type 'a t
    (** The type of priority queues. *)

    type 'a elt
    (** The type of priority queue elements. *)

    val create: unit -> 'a t
    (** Return a new priority queue, initially empty. *)

    val length: 'a t -> int
    (** Return the number of elements in a priority queue. *)

    val is_empty: 'a t -> bool
    (** [is_empty q] is [true] iff [q] is empty, that is, iff [length q = 0]. *)

    val add: 'a t -> 'a elt -> unit
    (** [add q x] adds the element [x] in the priority queue [q]. *)

    val add_seq: 'a t -> 'a elt Seq.t -> unit
    (** [add q s] adds the elements of [s] in the priority queue [q]. *)

    exception Empty
    (** Raised when {!min_elt}, {!pop_min} or {!remove_min} is applied
        to an empty queue. *)

    val min_elt: 'a t -> 'a elt
    (** [min_elt q] returns an element in queue [q] with minimal priority,
        without removing it from the queue,
        or raises {!Empty} if the queue is empty. *)

    val min_elt_opt: 'a t -> 'a elt option
    (** [min_elt_opt q] returns an element in queue [q] with minimal priority,
        without removing it from the queue, or returns [None] if the queue
        is empty. *)

    val pop_min: 'a t -> 'a elt
    (** [pop_min q] removes and returns an element in queue [q] with
        minimal priority, or raises {!Empty} if the queue is empty. *)

    val pop_min_opt: 'a t -> 'a elt option
    (** [pop_min_opt q] removes and returns an element in queue [q] with
        minimal priority, or returns [None] if the queue is empty. *)

    val remove_min: 'a t -> unit
    (** [pop_min_opt q] removes an element in queue [q] with
        minimal priority, or raises {!Empty} if the queue is empty. *)

    val clear: 'a t -> unit
    (** Discard all elements from a priority queue. *)

    val copy: 'a t -> 'a t
    (** [copy q] is a shallow copy of [q] (a new priority queue
        containing the same elements as [q]). *)

    (** {1:conversions Conversions from other data structures} *)

    val of_array: 'a elt array -> 'a t
    (** [of_array a] returns a new priority queue containing the
        elements of array [a]. Runs in linear time. *)

    val of_list: 'a elt list -> 'a t
    (** [of_list l] returns a new priority queue containing the
        elements of list [l]. Runs in linear time. *)

    val of_seq: 'a elt Seq.t -> 'a t
    (** [of_seq s] returns a new priority queue containing the
        elements of sequence [s]. Runs in linear time. *)

    (** {1:iteration Iteration}

        The order in which the elements of a priority queue are
        traversed is unspecified.

        It is a programming error to mutate a priority queue (by
        adding or removing elements) during an iteration of the queue.
        Such an error may be detected and signaled by the backing dynamic
        array implementation, but this is not guaranteed. *)

    val iter: ('a elt -> unit) -> 'a t -> unit
    (** [iter f q] calls [f] on each element of [q] in some
        unspecified order. *)

    val fold: ('acc -> 'a elt -> 'acc) -> 'acc -> 'a t -> 'acc
    (** [fold f accu q] is [(f (... (f (f accu x1) x2) ...) xn)]
        where [x1,x2,...,xn] are the elements of [q] in some
        unspecified order. *)

    val to_seq: 'a t -> 'a elt Seq.t
    (** [to_seq q] is the sequence of elements of [q] in some
        unspecified order. *)

  end
(** Output signature of the functor {!MakeMinPriority}. *)

module MakeMinPriority (Priority : OrderedType) :
  Min with type 'a elt := Priority.t * 'a
(** Functor building an implementation of the min-priority queue
    structure given a totally ordered type for priorities. *)

module type Max =
  sig
    type 'a t
    type 'a elt
    val create: unit -> 'a t
    val length: 'a t -> int
    val is_empty: 'a t -> bool
    val add: 'a t -> 'a elt -> unit
    val add_seq: 'a t -> 'a elt Seq.t -> unit
    exception Empty
    val max_elt: 'a t -> 'a elt
    val max_elt_opt: 'a t -> 'a elt option
    val pop_max: 'a t -> 'a elt
    val pop_max_opt: 'a t -> 'a elt option
    val remove_max: 'a t -> unit
    val clear: 'a t -> unit
    val copy: 'a t -> 'a t
    val of_array: 'a elt array -> 'a t
    val of_list: 'a elt list -> 'a t
    val of_seq: 'a elt Seq.t -> 'a t
    val iter: ('a elt -> unit) -> 'a t -> unit
    val fold: ('acc -> 'a elt -> 'acc) -> 'acc -> 'a t -> 'acc
    val to_seq: 'a t -> 'a elt Seq.t
end
(** Output signature of the functor {!MakeMaxPriority}. *)

module MakeMaxPriority (Priority : OrderedType) :
  Max with type 'a elt := Priority.t * 'a
(** Functor building an implementation of the max-priority queue
    structure given a totally ordered type for priorities. *)

(** {1 Monomorphic interfaces}

  The following functors provide implementations of min and
  max-priority queues given a totally ordered type of elements. This
  is convenient when the type of elements is already equipped with a
  comparison function.  Examples include integers, strings, etc.,
  where these functors can be readily applied to module [Int],
  [String], etc., from the standard library.
*)

module MakeMin (E : OrderedType) :
  sig
    type t
    include Min with type 'a elt := E.t and type 'a t := t
  end
(** Functor building an implementation of min-priority queues
    given a totally ordered type for the elements. *)

module MakeMax (E : OrderedType) :
  sig
    type t
    include Max with type 'a elt := E.t and type 'a t := t
  end
(** Functor building an implementation of max-priority queues
    given a totally ordered type for the elements. *)
