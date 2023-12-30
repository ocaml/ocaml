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
    given a total ordering function over the elements. This is a mutable
    data structure. Smaller elements are considered to have a higher
    priority (i.e., these are min-priority queues).

    The implementation uses a heap stored in a dynamic array, and is
    therefore reasonably efficient: accessing the minimum element
    takes constant time, and insertion and removal take time
    logarithmic in the size of the priority queue. Note that
    [of_array] runs in linear time (and thus must be preferred to
    repeated insertions with [add]).

    It is fine to have several elements that are considered equal by
    the ordering function and which are then considered as having the
    same priority. Said otherwise, the priority queue must be seen as
    a bag, not a set.  In particular, it is fine (and even expected)
    to have elements of a tuple type such as [priority * whatever]
    where the ordering function is only comparing the first component
    of the elements.

    It is guaranteed that the element returned by [min_elt] (or
    [min_elt_opt]) is the one that is removed from the priority queue
    by [pop_min] (or [pop_min_opt] or [remove_min]). This is important
    in many algorithms, (e.g. when peeking at several priority queues
    and then selecting one to remove from).

    @since 5.2
*)

module type OrderedType =
  sig
    type t
      (** The type of the priority queue elements. *)

    val compare : t -> t -> int
      (** A total ordering function over the elements.

          This is a two-argument function [f] such that [f e1 e2] is
          zero if the elements [e1] and [e2] are equal (i.e.  having
          the same priority), [f e1 e2] is strictly negative if [e1]
          is smaller than [e2] (i.e.  [e1] has higher priority than
          [e2]), and [f e1 e2] is strictly positive if [e1] is greater
          than [e2] (i.e. [e1] has lower priority than [e2]).

          The generic structural comparison function {!Stdlib.compare}
          is a suitable ordering function when the elements are
          identified with their priority (such as integers, strings,
          etc.) and when the smaller the better. *)
  end
(** Input signature of the functor {!Make}. *)

module type S =
  sig

    (** {1:pqueue Priority queues} *)

    type elt
    (** The type of the priority queue elements. *)

    type t
    (** The type of priority queues. *)

    val create: unit -> t
    (** Return a new priority queue, initially empty. *)

    val length: t -> int
    (** Return the number of elements in a priority queue. *)

    val is_empty: t -> bool
    (** [is_empty q] is [true] iff [q] is empty, that is, iff [length q = 0]. *)

    val add: t -> elt -> unit
    (** [add q x] adds the element [x] in the priority queue [q]. *)

    val add_seq: t -> elt Seq.t -> unit
    (** [add q s] adds the elements of [s] in the priority queue [q]. *)

    exception Empty
    (** Raised when {!min_elt}, {!pop_min} or {!remove_min} is applied
        to an empty queue. *)

    val min_elt: t -> elt
    (** [min_elt q] returns an element in queue [q] with minimal priority,
        without removing it from the queue,
        or raises {!Empty} if the queue is empty. *)

    val min_elt_opt: t -> elt option
    (** [min_elt_opt q] returns an element in queue [q] with minimal priority,
        without removing it from the queue, or returns [None] if the queue
        is empty. *)

    val pop_min: t -> elt
    (** [pop_min q] removes and returns an element in queue [q] with
        minimal priority, or raises {!Empty} if the queue is empty. *)

    val pop_min_opt: t -> elt option
    (** [pop_min_opt q] removes and returns an element in queue [q] with
        minimal priority, or returns [None] if the queue is empty. *)

    val remove_min: t -> unit
    (** [pop_min_opt q] removes an element in queue [q] with
        minimal priority, or raises {!Empty} if the queue is empty. *)

    val clear: t -> unit
    (** Discard all elements from a priority queue. *)

    val copy: t -> t
    (** [copy q] is a shallow copy of [q] (a new priority queue
        containing the same elements as [q]). *)

    (** {1:conversions Conversions from other data structures} *)

    val of_array: elt array -> t
    (** [of_array a] returns a new priority queue containing the
        elements of array [a]. Runs in linear time. *)

    val of_list: elt list -> t
    (** [of_list l] returns a new priority queue containing the
        elements of list [l]. Runs in linear time. *)

    val of_seq: elt Seq.t -> t
    (** [of_seq s] returns a new priority queue containing the
        elements of sequence [s]. Runs in linear time. *)

    (** {1:iteration Iteration}

        The order in which the elements of a priority queue are
        traversed is unspecified.

        It is a programming error to mutate a priority queue (by
        adding or removing elements) during an iteration of the queue.
        Such an error may be detected and signaled by the backing dynamic
        array implementation, but this is not guaranteed. *)

    val iter: (elt -> unit) -> t -> unit
    (** [iter f q] calls [f] on each element of [q] in some
        unspecified order. *)

    val fold: ('acc -> elt -> 'acc) -> 'acc -> t -> 'acc
    (** [fold f accu q] is [(f (... (f (f accu x1) x2) ...) xn)]
        where [x1,x2,...,xn] are the elements of [q] in some
        unspecified order. *)

    val to_seq: t -> elt Seq.t
    (** [to_seq q] is the sequence of elements of [q] in some
        unspecified order. *)

  end
(** Output signature of the functor {!Make}. *)

module Make (Ord : OrderedType) : S with type elt = Ord.t
(** Functor building an implementation of the priority queue structure
    given a totally ordered type. *)
