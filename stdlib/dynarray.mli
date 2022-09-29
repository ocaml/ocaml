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
(** Make sure that the array has at least the given capacity (underlying size).

    This is a more advanced operation that is only useful for performance
    purposes.

    @param filler an element used if the underlying array is empty,
      to initialize it. It will be retained until the array is totally
      empty or until it is garbage collected.
    @raise Invalid_arg if the size is not suitable (negative, or too big for
    OCaml arrays)
*)

val ensure_capacity_nonempty : 'a t -> int -> unit
(** Make sure that the array has at least the given capacity (underlying size),
    assuming it is non-empty. The first element is used as the filler.

    This is a more advanced operation that is only useful for performance
    purposes.

    @raise Invalid_arg if the array is empty or
      if the size is not suitable (negative, or too big for OCaml arrays)
*)

val is_empty : 'a t -> bool
(** Is the array empty? This is synonymous to [length a = 0]. *)

val add_last : 'a t -> 'a -> unit
(** [add_last a x] adds the element [x] at the end of the array [a].

    This might grow the underlying storage of [a] if it is full.

    Calling [add_last a] n times is amortized O(n) complexity,
    and O(ln(n)) reallocations of the underlying array. *)

val unsafe_add_last : 'a t -> 'a -> unit
(** [unsafe_add_last a x] pushes [x] as the last element of [a],
    assuming there is capacity for it in [a] already
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

val pop_last_opt : 'a t -> 'a option
(** Remove and return the last element, or [None] if the
    array is empty. *)

val pop_last : 'a t -> 'a
(** Remove the last element, or raise an exception if the
    array is empty.
    @raise Not_found on an empty array. *)

val remove_last : 'a t -> unit
(** [remove_last a] removes the last element of [a], or does nothing
    if [is_empty a]. *)

val copy : 'a t -> 'a t
(** Shallow copy. *)

val truncate : 'a t -> int -> unit
(** Truncate to the given size (remove elements above this size).
    Does nothing if the parameter is bigger than the current size.

    [truncate arr n] is similar to:
    [while length arr > n do ignore (pop_exn arr) done] *)

val shrink_capacity : 'a t -> unit
(** Shrink internal array to fit the size of the array. This can be useful
    to make sure there is no memory wasted on a long-held array. *)

val iter : ('a -> unit) -> 'a t -> unit
(** Iterate on the array's content. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Iterate on the array, with indexes. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map elements of the array, yielding a new array. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [map f v] is just like {!map}, but it also passes in the index
    of each element as the first argument to the function [f]. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter f a] is an array containing all elements of [a] that satisfy [f] *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f a] is a new array [b], such that for each item [x] in [a]:
  - if [f x = Some y], then [y] is in [b]
  - if [f x = None], then no element is added to [b].

  It is similar to {!List.filter_map}. *)

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold on elements of the array *)

val exists : ('a -> bool) -> 'a t -> bool

val for_all : ('a -> bool) -> 'a t -> bool

val get : 'a t -> int -> 'a
(** [get a i] is the [i]-th element of [a].
    @raise Invalid_argument if the index is
      invalid (i.e. not in [[0.. length a-1]]). *)

val set : 'a t -> int -> 'a -> unit
(** [set a i x] sets the [i]-th element of [a] to be [x].
    Just like {!Array.set}, indexing starts at 0.
    @raise Invalid_argument if the index is invalid. *)

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [blit a i b j len] copies [len] elements from [a],
    starting at index [i], into [b], starting at index [j].

    See {!Array.blit}.
    @raise Invalid_argument if the indices or lengthts are not valid.
*)

val length : _ t -> int
(** Number of elements in the array. *)

val of_array : 'a array -> 'a t
(** [of_array a] returns a array corresponding to the array [a].
    Operates in [O(n)] time. *)

val of_list : 'a list -> 'a t
(** [of_list l] is the array containing the elements of [l] in
    the same order. *)

val to_array : 'a t -> 'a array
(** [to_array a] returns an array corresponding to the dynamic array [a].
    This always allocate a new array and copies item into it. *)

val to_list : 'a t -> 'a list
(** [to_list a] is a list with the elements contained in the array [a]. *)

val of_seq : 'a Seq.t -> 'a t
(** Convert a sequence of items to an array containing them in the
    same order. *)

val to_seq : 'a t -> 'a Seq.t
(** [of_seq a] is the sequence of items [get a 0], [get a 1], etc. *)

val to_seq_rev : 'a t -> 'a Seq.t
(** Iterate over the array, starting from the last (top) element. *)
