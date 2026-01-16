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
    given a totally ordered type for elements. This is a mutable
    data structure. Both min- and max-priority queues are provided.

    The implementation uses a heap stored in a dynamic array, and is
    therefore reasonably efficient: accessing the minimum
    (resp. maximum) element takes constant time, and insertion and
    removal take time logarithmic in the size of the priority
    queue. Note that [of_array] runs in linear time (and thus must be
    preferred to repeated insertions with [add]).

    It is fine to have several elements with the same priority.
    Nothing is guaranteed regarding the order in which they will be
    popped.  However, it is guaranteed that the element returned by
    [min_elt] (or [get_min_elt]) is the one that is removed from the
    priority queue by [pop_min] (or [remove_min]). This is important
    in many algorithms, (e.g. when peeking at several priority queues
    and then selecting one to remove from).

    @since 5.4
*)

module type OrderedType =
  sig
    type t
    (** The type of elements. *)

    val compare : t -> t -> int
    (** A total ordering function to compare elements.

        This is a two-argument function [f] such that [f e1 e2] is
        zero if the elements [e1] and [e2] are equal, [f e1 e2] is
        strictly negative if [e1] is smaller than [e2], and [f e1
        e2] is strictly positive if [e1] is greater than [e2].

        The generic structural comparison function {!Stdlib.compare}
        is a suitable ordering function for element types such as
        [int] or [string]. *)
  end
(** Input signature of the functors {!MakeMin} and {!MakeMax}. *)

module type Min =
  sig

    (** {1:pqueue Min-priority queues} *)

    type t
    (** The type of priority queues. *)

    type elt
    (** The type of priority queue elements. *)

    val create: unit -> t
    (** Return a new priority queue, initially empty. *)

    val length: t -> int
    (** Return the number of elements in a priority queue. *)

    val is_empty: t -> bool
    (** [is_empty q] is [true] iff [q] is empty, that is, iff [length q = 0]. *)

    val add: t -> elt -> unit
    (** [add q x] adds the element [x] in the priority queue [q]. *)

    val add_iter: t -> ((elt -> unit) -> 'x -> unit) -> 'x -> unit
    (** [add_iter q iter x] adds each element of [x] to the end of [q].
        This is [iter (add q) x]. *)

    val min_elt: t -> elt option
    (** [min_elt q] is an element of [q] with minimal priority or
        [None] if the queue is empty. The queue is not modified. *)

    val get_min_elt: t -> elt
    (** [get_min_elt q] returns an element of [q] with minimal
        priority, or raises {!Stdlib.Invalid_argument} if the queue is
        empty. The queue is not modified. *)

    val pop_min: t -> elt option
    (** [pop_min q] removes and returns an element in queue [q] with
        minimal priority, or returns [None] if the queue is empty. *)

    val remove_min: t -> unit
    (** [remove_min q] removes an element in queue [q] with minimal
        priority. It does nothing if [q] is empty. *)

    val clear: t -> unit
    (** [clear q] removes all elements from [q]. *)

    val copy: t -> t
    (** [copy q] is a new priority queue with the same elements [q] has. *)

    (** {1:conversions Conversions from other data structures} *)

    val of_array: elt array -> t
    (** [of_array a] returns a new priority queue containing the
        elements of array [a]. Runs in linear time. *)

    val of_list: elt list -> t
    (** [of_list l] returns a new priority queue containing the
        elements of list [l]. Runs in linear time. *)

    val of_iter: ((elt -> unit) -> 'x -> unit) -> 'x -> t
    (** [of_iter iter x] returns a new priority queue containing the
        elements of [x], obtained from [iter].

        For example, [of_iter Seq.iter s] returns a new priority queue
        containing all the elements of the sequence [s] (provided it
        is finite).

        Runs in linear time (excluding the time spent in [iter]). *)

    (** {1:iteration Iteration}

        The order in which the elements of a priority queue are
        traversed is unspecified.

        It is a programming error to mutate a priority queue (by
        adding or removing elements) during an iteration of the queue.
        Such an error may be detected and signaled by the backing dynamic
        array implementation, but this is not guaranteed. *)

    val iter_unordered: (elt -> unit) -> t -> unit
    (** [iter_unordered f q] applies [f] to all elements in [q].  The
        order in which the elements are passed to [f] is unspecified.

        The behavior is not specified if the priority queue is modified
        by [f] during the iteration. *)

    val fold_unordered: ('acc -> elt -> 'acc) -> 'acc -> t -> 'acc
    (** [fold_unordered f accu q] is [(f (... (f (f accu x1) x2) ...)
        xn)] where [x1,x2,...,xn] are the elements of [q]. The order
        in which the elements are passed to [f] is unspecified.

        The behavior is not specified if the priority queue is modified
        by [f] during the iteration. *)

  end
(** Output signature of the functor {!MakeMin}. *)

module MakeMin(E: OrderedType) : Min with type elt := E.t
(** Functor building an implementation of the min-priority queue
    structure given a totally ordered type for elements. *)

module type Max =
  sig
    type t
    type elt
    val create: unit -> t
    val length: t -> int
    val is_empty: t -> bool
    val add: t -> elt -> unit
    val add_iter: t -> ((elt -> unit) -> 'x -> unit) -> 'x -> unit
    val max_elt: t -> elt option
    val get_max_elt: t -> elt
    val pop_max: t -> elt option
    val remove_max: t -> unit
    val clear: t -> unit
    val copy: t -> t
    val of_array: elt array -> t
    val of_list: elt list -> t
    val of_iter: ((elt -> unit) -> 'x -> unit) -> 'x -> t
    val iter_unordered: (elt -> unit) -> t -> unit
    val fold_unordered: ('acc -> elt -> 'acc) -> 'acc -> t -> 'acc
end
(** Output signature of the functor {!MakeMax}. *)

module MakeMax(E: OrderedType) : Max with type elt := E.t
(** Functor building an implementation of the max-priority queue
    structure given a totally ordered type for elements. *)

(** {1 Polymorphic priority queues}

    The following, more complex functors create polymorphic queues of
    type ['a t], just like other polymorphic containers (lists,
    arrays...). They require a notion of "polymorphic elements" ['a
    elt] that can be compared without depending on the values of ['a].

    One usage scenario is when the user wants to pass priorities
    separately from the value stored in the queue. This is done by
    using pairs [priority * 'a] as elements.
    {[
      module Prio : OrderedType = ...

      module PrioQueue = Pqueue.MakeMinPoly(struct
        type 'a t = Prio.t * 'a
        let compare (p1, _) (p2, _) = Prio.compare p1 p2
      end)

      (* for example, we now have: *)
      PrioQueue.add: 'a PrioQueue.t -> Prio.t * 'a -> unit
      PrioQueue.min_elt: 'a PrioQueue.t -> (Prio.t * 'a) option
    ]}
*)

module type OrderedPolyType =
  sig
    type 'a t
    (** The polymorphic type of elements. *)

    val compare : 'a t -> 'b t -> int
    (** [compare] is a total order on values of type {!t}. *)
  end
(** Input signature of the functors {!MakeMinPoly} and {!MakeMaxPoly}. *)

module type MinPoly =
  sig
    type 'a t
    type 'a elt
    val create: unit ->'a t
    val length: 'a t -> int
    val is_empty: 'a t -> bool
    val add: 'a t -> 'a elt -> unit
    val add_iter: 'a t -> (('a elt -> unit) -> 'x -> unit) -> 'x -> unit
    val min_elt: 'a t -> 'a elt option
    val get_min_elt: 'a t -> 'a elt
    val pop_min: 'a t -> 'a elt option
    val remove_min: 'a t -> unit
    val clear: 'a t -> unit
    val copy: 'a t -> 'a t
    val of_array: 'a elt array -> 'a t
    val of_list: 'a elt list -> 'a t
    val of_iter: (('a elt -> unit) -> 'x -> unit) -> 'x -> 'a t
    val iter_unordered: ('a elt -> unit) -> 'a t -> unit
    val fold_unordered: ('acc -> 'a elt -> 'acc) -> 'acc -> 'a t -> 'acc
  end
(** Output signature of the functor {!MakeMinPoly}. *)

module MakeMinPoly (E : OrderedPolyType) :
  MinPoly with type 'a elt := 'a E.t
(** Functor building an implementation of min-priority queues
    given a totally ordered type for the elements. *)

module type MaxPoly =
  sig
    type 'a t
    type 'a elt
    val create: unit -> 'a t
    val length: 'a t -> int
    val is_empty: 'a t -> bool
    val add: 'a t -> 'a elt -> unit
    val add_iter: 'a t -> (('a elt -> unit) -> 'x -> unit) -> 'x -> unit
    val max_elt: 'a t -> 'a elt option
    val get_max_elt: 'a t -> 'a elt
    val pop_max: 'a t -> 'a elt option
    val remove_max: 'a t -> unit
    val clear: 'a t -> unit
    val copy: 'a t -> 'a t
    val of_array: 'a elt array -> 'a t
    val of_list: 'a elt list -> 'a t
    val of_iter: (('a elt -> unit) -> 'x -> unit) -> 'x -> 'a t
    val iter_unordered: ('a elt -> unit) -> 'a t -> unit
    val fold_unordered: ('acc -> 'a elt -> 'acc) -> 'acc -> 'a t -> 'acc
end
(** Output signature of the functor {!MakeMaxPoly}. *)

module MakeMaxPoly (E : OrderedPolyType) :
  MaxPoly with type 'a elt := 'a E.t
(** Functor building an implementation of max-priority queues
    given a totally ordered type for the elements. *)
