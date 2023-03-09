(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                            Simon Cruanes                               *)
(*             Gabriel Scherer, projet Partout, INRIA Paris-Saclay        *)
(*                                                                        *)
(*   Copyright 2022 Simon Cruanes.                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Dynamic arrays.

    The {!Array} module provide arrays of fixed length. {!Dynarray}
    provides array whose length can change over time, by adding or
    removing elements at the end of the array.

    This is typically used to accumulate elements whose number is not
    known in advance or changes during computation, while also
    providing fast access to elements at arbitrary positions.

{[
    let dynarray_of_list li =
      let arr = Dynarray.create () in
      List.iter (fun v -> Dynarray.add_last arr v) li;
      arr
]}

    The {!Buffer} module provides similar features, but it is
    specialized for accumulating characters into a dynamically-resized
    string.

    The {!Stack} module provides a last-in first-out data structure
    that can be easily implemented on top of dynamic arrays.

    {b Warning.} In their current implementation, the memory layout
    of dynamic arrays differs from the one of {!Array}s. See the
    {{!section:memory_layout} Memory Layout} section for more information.

    @since 5.1
*)

(** {b Unsynchronized accesses} *)

[@@@alert unsynchronized_accesses
    "Unsynchronized accesses to dynamic arrays are a programming error."
]

(**
   Unsynchronized accesses to a dynamic array may lead to an invalid
   dynamic array state. Thus, concurrent accesses to dynamic arrays
   must be synchronized (for instance with a {!Mutex.t}).
*)

(** {1:dynarrays Dynamic arrays} *)

type 'a t
(** A dynamic array containing values of type ['a].

    A dynamic array [a] provides constant-time [get] and [set]
    operation on indices between [0] and [Dynarray.length a - 1]
    included. Its {!length} may change over time by adding or removing
    elements to the end of the array.

    We say that an index into a dynarray [a] is valid if it is in
    [0 .. length a - 1] and invalid otherwise.
*)

val create : unit -> 'a t
(** [create ()] is a new, empty array. *)

val make : int -> 'a -> 'a t
(** [make n x] is a new array of length [n], filled with [x]. *)

val init : int -> (int -> 'a) -> 'a t
(** [init n f] is a new array [a] of length [n],
    such that [get a i] is [f i]. In other words,
    the elements of [a] are [f 0], then [f 1],
    then [f 2]... and [f (n - 1)] last, evaluated
    in that order.

    This is similar to {!Array.init}. *)

val get : 'a t -> int -> 'a
(** [get a i] is the [i]-th element of [a], starting with index [0].

    @raise Invalid_argument if the index is invalid *)

val set : 'a t -> int -> 'a -> unit
(** [set a i x] sets the [i]-th element of [a] to be [x].

    [i] must be a valid index. [set] does not add new elements to the
    array -- see {!add_last} to add an element.

    @raise Invalid_argument if the index is invalid. *)

val length : _ t -> int
(** [length a] is the number of elements in the array. *)

val is_empty : 'a t -> bool
(** [is_empty a] is [true] if [a] is empty, that is, if [length a = 0]. *)

val copy : 'a t -> 'a t
(** [copy a] is a shallow copy of [a], a new array
    containing the same elements as [a]. *)


(** {1:adding Adding elements}

    Note: all operations adding elements raise [Invalid_argument] if the
    length needs to grow beyond {!Sys.max_array_length}. *)

val add_last : 'a t -> 'a -> unit
(** [add_last a x] adds the element [x] at the end of the array [a]. *)

val append_array : 'a t -> 'a array -> unit
(** [append_array a b] adds all elements of [b] at the end of [a],
    in the order they appear in [b].

    For example:
    {[
      let a = Dynarray.of_list [1;2]
      let () = Dynarray.append a [|3; 4|]
      let () = assert (Dynarray.to_list a = [1; 2; 3; 4])
    ]}
*)

val append_list : 'a t -> 'a list -> unit
(** Like {!append_array} but with a list. *)

val append : 'a t -> 'a t -> unit
(** [append a b] is like [append_array a b],
    but [b] is itself a dynamic arreay instead of a fixed-size array.

    Beware! Calling [append a a] iterates on [a] and adds elements to
    it at the same time; it is a programming error and fails with
    [Invalid_argument].
*)

val append_seq : 'a t -> 'a Seq.t -> unit
(** Like {!append_array} but with a sequence.

    Beware! Calling [append_seq a (to_seq a)] is unspecified and may
    result in an infinite loop, see the {!append} comment above.
*)

val append_iter :
  'a t ->
  (('a -> unit) -> 'x -> unit) ->
  'x -> unit
(** [append_iter a iter x] adds each element of [x] to the end of [a].
    This is [iter (add_last a) x].

    For example, [append_iter a List.iter [1;2;3]] would add elements
    [1], [2], and then [3] at the end of [a].
    [append_iter a Queue.iter q] adds elements from the queue [q]. *)


(** {1:removing Removing elements} *)

val pop_last_opt : 'a t -> 'a option
(** [pop_last_opt a] removes and returns the last element of [a],
    or [None] if the array is empty. *)

val pop_last : 'a t -> 'a
(** [pop_last a] removes and returns the last element of [a].

    @raise Not_found on an empty array. *)

val remove_last : 'a t -> unit
(** [remove_last a] removes the last element of [a], if any.
    It does nothing if [a] is empty. *)

val clear : 'a t -> unit
(** [clear a] is [truncate a 0], it removes all the elements of [a]. *)

val truncate : 'a t -> int -> unit
(** [truncate a n] truncates [a] to have at most [n] elements.

    It removes elements whose index is greater or equal to [n].
    It does nothing if [n >= length a].

    [truncate a n] is equivalent to:
    {[
      while length a > n do
        remove_last a
      done
    ]}
*)


(** {1:iteration Iteration}

    The iteration functions traverse the elements of a dynamic array.
    Traversals of [a] are computed in increasing index order: from
    the element of index [0] to the element of index [length a - 1].

    It is a programming error to modify the length of an array
    (by adding or removing elements) during an iteration on the
    array. Any iteration function will fail with [Invalid_argument]
    if it detects such a length change.
*)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f a] calls [f] on each element of [a]. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iteri f a] calls [f i x] for each [x] at index [i] in [a]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f a] is a new array of elements of the form [f x]
    for each element [x] of [a].

    For example, if the elements of [a] are [x0, x1, x2],
    then the elements of [b] are [f x0, f x1, f x2].
*)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi f a] is a new array of elements of the form [f i x]
    for each element [x] of [a] at index [i].

    For example, if the elements of [a] are [x0, x1, x2],
    then the elements of [b] are [f 0 x0, f 1 x1, f 2 x2].
*)

val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
(** [fold_left f acc a] folds [f] over [a] in order,
    starting with accumulator [acc].

    For example, if the elements of [a] are [x0, x1],
    then [fold f acc a] is
    {[
      let acc = f acc x0 in
      let acc = f acc x1 in
      acc
    ]}
*)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists f a] is [true] if some element of [a] satisfies [f].

    For example, if the elements of [a] are [x0, x1, x2], then
    [exists f a] is [f x0 || f x1 || f x2].
*)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all f a] is [true] if all elements of [a] satisfy [f].
    This includes the case where [a] is empty.

    For example, if the elements of [a] are [x0, x1], then
    [exists f a] is [f x0 && f x1 && f x2].
*)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter f a] is a new array of all the elements of [a] that satisfy [f].
    In other words, it is an array [b] such that, for each element [x]
    in [a] in order, [x] is added to [b] if [f x] is [true].

    For example, [filter (fun x -> x >= 0) a] is a new array
    of all non-negative elements of [a], in order.
*)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f a] is a new array of elements [y]
    such that [f x] is [Some y] for an element [x] of [a].
    In others words, it is an array [b] such that, for each element
    [x] of [a] in order:
    {ul
    {- if [f x = Some y], then [y] is added to [b],}
    {- if [f x = None], then no element is added to [b].}}

    For example, [filter_map int_of_string_opt inputs] returns
    a new array of integers read from the strings in [inputs],
    ignoring strings that cannot be converted to integers.
*)

(** {1:conversions Conversions to other data structures}

    Note: the [of_*] functions raise [Invalid_argument] if the
    length needs to grow beyond {!Sys.max_array_length}.

    The [to_*] functions, except for {!to_seq}, iterate on their
    dynarray argument. In particular it is a programming error if the
    length of the dynarray changes during their execution, and the
    conversion functions raise [Invalid_argument] if they observe such
    a change.

    {!to_seq} produces an on-demand sequence of values, and is expected
    to be called with effects happening in-between. Its specification
    tolerates changes of length. (See below.)
*)

val of_array : 'a array -> 'a t
(** [of_array arr] returns a dynamic array corresponding to the
    fixed-sized array [a]. Operates in [O(n)] time by making a copy. *)

val to_array : 'a t -> 'a array
(** [to_array a] returns a fixed-sized array corresponding to the
    dynamic array [a]. This always allocate a new array and copies
    elements into it. *)

val of_list : 'a list -> 'a t
(** [of_list l] is the array containing the elements of [l] in
    the same order. *)

val to_list : 'a t -> 'a list
(** [to_list a] is a list with the elements contained in the array [a]. *)

val of_seq : 'a Seq.t -> 'a t
(** [of_seq seq] is an array containing the same elements as [seq].

    It traverses [seq] once and will terminate only if [seq] is finite. *)

val to_seq : 'a t -> 'a Seq.t
(** [to_seq a] is the sequence of elements
    [get a 0], [get a 1]... [get a (length a - 1)].

    Because sequences are computed on-demand, we have to assume that
    the array may be modified in the meantime, and give a precise
    specification for this case below.

    Demanding the [i]-th element of the resulting sequence (which can
    happen zero, one or several times) will access the [i]-th element
    of [a] at the time of the demand. The sequence stops if [a] has
    less than [i] elements at this point.
*)

val to_seq_rev : 'a t -> 'a Seq.t
(** [to_seq_rev a] is the sequence of elements
    [get a (l - 1)], [get a (l - 2)]... [get a 0],
    where [l] is [length a] at the time [to_seq_rev] is invoked.

    Elements that have been removed from the array by the time they
    are demanded in the sequence are skipped.
*)


(** {1:advanced Advanced topics for performance} *)

(** {2:capacity Backing array, capacity}

    Internally, a dynamic array uses a {b backing array} (a fixed-size
    array as provided by the {!Array} module) whose length is greater
    or equal to the length of the dynamic array. We call {b capacity}
    the length of the backing array.

    The capacity of a dynamic array is relevant in advanced scenarios,
    when reasoning about the performance of dynamic array programs:
    {ul
    {- The memory usage of a dynamic array is proportional to its capacity,
       rather than its length.}
    {- When then is no empty space left at the end of the backing array.
       adding elements requires allocating a new, larger backing array.}}

    The implementation uses a standard exponential reallocation
    strategy which guarantees amortized constant-time operation: the
    total capacity of all backing arrays allocated over the lifetime
    of a dynamic array is at worst proportional to the total number of
    elements added.

    In other words, users need not care about capacity and reallocations,
    and they will get reasonable behavior by default. However, in some
    performance-sensitive scenarios the functions below can help control
    memory usage or guarantee an optimal number of reallocations.
*)

val capacity : 'a t -> int
(** [capacity a] is the length of [a]'s backing array. *)

val ensure_capacity : 'a t -> int -> unit
(** [ensure_capacity a n] makes sure that the capacity of [a]
    is at least [n].

    @raise Invalid_argument if the requested capacity is
      outside the range [0 .. Sys.max_array_length].

    A use case would be to implement {!of_array} (without using {!init} directly):
    {[
    let of_array arr =
      let a = Dynarray.create () in
      Dynarray.ensure_capacity a (Array.length arr);
      Array.iter (fun v -> add_last a v) arr
    ]}

    Using [ensure_capacity] guarantees that at most one reallocation
    will take place, instead of possibly several.

    Without this [ensure_capacity] hint, the number of resizes would
    be logarithmic in the length of [arr], creating a constant-factor
    slowdown noticeable when [arr] is large.
*)

val ensure_extra_capacity : 'a t -> int -> unit
(** [ensure_extra_capacity a n] is [ensure_capacity a (length a + n)],
    it makes sure that [a] has room for [n] extra items.

    A use case would be to implement {!append_array}:
    {[
    let append_array a arr =
      ensure_extra_capacity a (Array.length arr);
      Array.iter (fun v -> add_last a v) arr
    ]}
*)

val fit_capacity : 'a t -> unit
(** [fit_capacity a] shrinks the backing array so that its capacity is
    exactly [length a], with no additional empty space at the
    end. This can be useful to make sure there is no memory wasted on
    a long-lived array.

    Note that calling [fit_capacity] breaks the amortized complexity
    guarantees provided by the default reallocation strategy. Calling
    it repeatedly on an array may have quadratic complexity, both in
    time and in total number of allocations.

    If you know that a dynamic array has reached its final length,
    which will remain fixed in the future, it is sufficient to call
    [to_array] and only keep the resulting fixed-size
    array. [fit_capacity] is useful when you need to keep a dynamic
    array for eventual future resizes.
*)

val truncate_capacity : 'a t -> int -> unit
(** [truncate_capacity a n] shrinks the backing array to have
    capacity at most [n]; in particular, like [truncate a n],
    all elements of index [n] or greater are removed.

    This is equivalent to [truncate a n; fit_capacity a] but more
    efficient: [truncate a n] needs to overwrite the removed elements
    to preserve the {{!section:noleaks} no leaks} guarantee.

    Like {!fit_capacity}, this function breaks the amortized
    complexity guarantees provided by the reallocation
    strategy. Calling it repeatedly on an array may have quadratic
    complexity, both in time and in total number of allocations.
*)

val reset : 'a t -> unit
(** [reset a] clears [a] and replaces its backing array by an empty array.

    It is equivalent to [clear a; fit_capacity a].
*)

(** {2:noleaks No leaks: preservation of memory liveness}

    The user-provided values reachable from a dynamic array [a] are
    exactly the elements in the positions [0] to [length a - 1]. In
    particular, no user-provided values are "leaked" by being present
    in the backing array in position [length a] or later.
*)

(** {2:memory_layout Memory layout of dynarrays}

    In the current implementation, the backing array of an
    ['a Dynarray.t] is not an ['a array], but something with the same
    representation as an ['a option array] or ['a ref array].
    Each element is in a "box", allocated when the element is first
    added to the array -- see the implementation for more details.

    Using a ['a array] would be delicate, as there is no obvious
    type-correct way to represent the empty space at the end of the
    backing array -- using user-provided values would either
    complicate the API or violate the {{!section:noleaks}no leaks}
    guarantee. The constraint of remaining memory-safe under
    unsynchronized concurrent usage makes it even more
    difficult. Various unsafe ways to do this have been discussed,
    with no consensus for a standard implementation so far.

    On a realistic automated-theorem-proving program that relies
    heavily on dynamic arrays, we measured the overhead of this extra
    "boxing" as at most 25%. We believe that the overhead for most
    uses of dynarray is much smaller, neglectible in many cases, but
    you may still prefer to use your own specialized implementation
    for performance. (If you know that you do not need the
    {{:noleaks}no leaks} guarantee, you can also speed up deleting
    elements.)
*)

