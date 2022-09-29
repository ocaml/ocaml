(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                            Simon Cruanes                               *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Growable, mutable array.

    @since 5.1
*)

type 'a t
(** A dynamic array containing values of type ['a].

    This contains an underlying {!array} along with a size.
    Operations such as {!add_last}, {!append}, and {!append_seq}, extend the
    size (and might reallocate the underlying array).

    Operations such as {!pop}, and {!truncate}, reduce the size. *)

val create : unit -> 'a t
(** [create ()] is a new, empty array. *)

val make : int -> 'a -> 'a t
(** [make n x] makes a array of length [n], filled with [x]. *)

val init : int -> (int -> 'a) -> 'a t
(** [init n f] is a new array of length [n],
    such that [get (init n f) i] is [f i].

    This is the equivalent of {!Array.init}. *)

val clear : 'a t -> unit
(** [clear a] clears the content of [a], and sets its length to 0.
    This ensures that [length v = 0] but the underlying array is kept,
    and possibly references to former elements, which are therefore
    not garbage collectible. *)

val ensure_capacity_with : 'a t -> filler:'a -> int -> unit
(** [ensure_capacity_with a ~filler n] makes sure that [a]
    has at least a capacity for storing [n] elements.

    This is a more advanced operation that is only useful for performance
    purposes.

    @param filler an element used if the underlying array is empty,
      to initialize it. It will be retained until the array is totally
      empty or until it is garbage collected.
    @raise Invalid_arg if the size is not suitable (negative, or too big for
    OCaml arrays)
*)

val ensure_capacity_nonempty : 'a t -> int -> unit
(** [ensure_capacity_nonempty a n] makes sure that [a] has at least the
    capacity [n], assuming it is already non-empty.
    The first element is used as the filler.

    This is a more advanced operation that is only useful for performance
    purposes.

    @raise Invalid_arg if the array is empty or
      if the size is not suitable (negative, or too big for OCaml arrays)
*)

val is_empty : 'a t -> bool
(** [is_empty a] is [true] if [a] is empty, that is, if [length a = 0].

    Note that an empty dynarray might still have non-0 underlying capacity
    and therefore non-0 memory footprint. *)

val add_last : 'a t -> 'a -> unit
(** [add_last a x] adds the element [x] at the end of the array [a].

    This might grow the underlying storage of [a] if it is full.

    Calling [add_last a] n times is amortized O(n) complexity,
    and O(ln(n)) reallocations of the underlying array. *)

val unsafe_add_last : 'a t -> 'a -> unit
(** [unsafe_add_last a x] adds [x] as the last element of [a],
    assuming there is room for it in [a] already
    (e.g. using {!ensure_capacity}).

    It is unspecified what happens if the capacity is not enough.
    This is for advanced use cases only. *)

val append : 'a t -> 'a t -> unit
(** [append a b] adds all elements of [b] at the end of [a],
    in the order they appear in [b]. [b] is not modified.

    For example, [a] will contain [1,2,3,4,5,6] after this code runs:
    {[
      let a = of_list [1;2;3];;
      let b = of_list [4;5;6];;
      let () = append a b;;
    ]}
    *)

val append_array : 'a t -> 'a array -> unit
(** Like {!append}, with an array. *)

val append_seq : 'a t -> 'a Seq.t -> unit
(** Like {!append} but with an iterator. *)

val append_list : 'a t -> 'a list -> unit
(** Like {!append} but with a list. *)

val append_iter :
  'a t ->
  (('a -> unit) -> 'iter-> unit) ->
  'iter -> unit
(** [append_iter a iter x] adds to [a] each element in [x]. It uses [iter]
    to iterate over [x].

    For example, [append_iter a List.iter [1;2;3]] would add elements
    [1], [2], and [3] at the end of [a].
    [append_iter a Queue.iter q] adds elements from the queue [q]. *)

val pop_last_opt : 'a t -> 'a option
(** [pop_last_opt a] removes and returns the last element of [a],
    or [None] if the array is empty. *)

val pop_last : 'a t -> 'a
(** [pop_last a] removes and returns the last element of [a], assuming
    [a] is not empty.
    @raise Not_found on an empty array. *)

val remove_last : 'a t -> unit
(** [remove_last a] removes the last element of [a], or does nothing
    if [is_empty a]. *)

val copy : 'a t -> 'a t
(** [copy a] is a shallow copy of [a], that can be modified independently. *)

val truncate : 'a t -> int -> unit
(** [truncate a n] truncates [a] to have at most [n] elements.

    It removes elements whose index is great or equal than [n].
    It does nothing if [n >= length a].

    It is similar to:
    {[
      while length a > n do
        remove_last a
      done
    ]} *)

val fit_capacity : 'a t -> unit
(** [fit_capacity a] shrinks the internal array to fit [length a] exactly,
    with no additional empty space at the end. This can be useful
    to make sure there is no memory wasted on a long-lived array.
    This does nothing if [a] is already full. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f a] calls [f] on each element of [a], in increasing index order. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iteri f a] calls [f i x] for each [x] at index [i] in [a].. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f a] is a new array of length [length a], with elements mapped
    from [a] using [f].

    It is similar to [to_array a |> Array.map f |> of_array]. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi f v] is just like {!map}, but it also passes in the index
    of each element as the first argument to the function [f]. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter f a] is an array containing all elements of [a] that satisfy [f] *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f a] is a new array [b], such that for each item [x] in [a]:
  - if [f x = Some y], then [y] is in [b]
  - if [f x = None], then no element is added to [b].

  It is similar to {!List.filter_map}. *)

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [fold_left f acc a] folds [f] over [a] starting with accumulator [acc].

    It is similar to [Array.fold_left f acc (to_array a)]. *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists f a] returns [true] if some element of [a] satisfies [f]. *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all f a] returns [true] if all elements of [a] satisfie [f].
    This includes the case where [a] is empty. *)

val get : 'a t -> int -> 'a
(** [get a i] is the [i]-th element of [a], starting with index [0].
    @raise Invalid_argument if the index is
      invalid (i.e. not in [[0.. length a-1]]). *)

val set : 'a t -> int -> 'a -> unit
(** [set a i x] sets the [i]-th element of [a] to be [x].
    Just like {!get}, indexing starts at 0.
    @raise Invalid_argument if the index is invalid. *)

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [blit a i b j len] copies [len] elements from [a],
    starting at index [i], into [b], starting at index [j].

    See {!Array.blit}.
    @raise Invalid_argument if the indices or lengthts are not valid.
*)

val rev : 'a t -> 'a t
(** [rev a] is a new array containing the same elements as [a], but in the
    reverse order. *)

val length : _ t -> int
(** [length a] is the number of elements in the array.
    The last element of [a], if not empty, is [get a (length a - 1)].
    This operation is constant time. *)

val of_array : 'a array -> 'a t
(** [of_array a] returns a array corresponding to the array [a].
    Operates in [O(n)] time by making a copy. *)

val of_list : 'a list -> 'a t
(** [of_list l] is the array containing the elements of [l] in
    the same order. *)

val to_array : 'a t -> 'a array
(** [to_array a] returns an array corresponding to the dynamic array [a].
    This always allocate a new array and copies item into it. *)

val to_list : 'a t -> 'a list
(** [to_list a] is a list with the elements contained in the array [a]. *)

val of_seq : 'a Seq.t -> 'a t
(** [of_seq seq] is an array containing the same elements as [seq].

    It traverses [seq] only once and will terminate only if [seq] is finite. *)

val to_seq : 'a t -> 'a Seq.t
(** [of_seq a] is the sequence of items [get a 0], [get a 1], etc.
    This sequence can be safely reused multiple times as long as [a]
    is not changed in the mean time. *)

val to_seq_rev : 'a t -> 'a Seq.t
(** [to_seq_rev a] is like [to_seq (rev a)].
    It yields the last element of [a] first. *)
