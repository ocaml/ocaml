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

(** Growable, mutable array *)

type 'a t
(** A dynamic array containing values of type ['a].

    This contains an underlying {!array} along with a size.
    Operations such as {!push}, {!append}, and {!append_seq}, extend the
    size (and might reallocate the underlying array).

    Operations such as {!pop}, and {!truncate}, reduce the size. *)

val create : unit -> 'a t
(** Create a new, empty array. *)

val make : int -> 'a -> 'a t
(** [make n x] makes a array of size [n], filled with [x]. *)

val init : int -> (int -> 'a) -> 'a t
(** Init the array with the given function and size. *)

val clear : 'a t -> unit
(** Clear the content of the array.
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
    assuming it is non-empty.

    This is a more advanced operation that is only useful for performance
    purposes.

    @raise Invalid_arg if the array is empty or
      if the size is not suitable (negative, or too big for OCaml arrays)
*)

val is_empty : 'a t -> bool
(** Is the array empty? This is synonymous to [length a = 0]. *)

val push : 'a t -> 'a -> unit
(** Add an element at the end of the array. This might extend the underlying
    array if it is full.

    Calling [push] [n] times is amortized O(n) complexity,
    and O(ln(n)) reallocations of the underlying array. *)

val unsafe_push : 'a t -> 'a -> unit
(** Push an element, assuming there is capacity for it
    (e.g. using {!ensure_capacity}).

    It is unspecified what happens if the capacity is not enough.
    This is for advanced used only. *)

val append : 'a t -> 'a t -> unit
(** [append a b] adds all elements of [b] to [a]. [b] is not modified. *)

val append_array : 'a t -> 'a array -> unit
(** Like {!append}, with an array. *)

val append_seq : 'a t -> 'a Seq.t -> unit
(** Like {!append} but with an iterator. *)

val append_list : 'a t -> 'a list -> unit
(** Like {!append} but with a list. *)

val pop : 'a t -> 'a option
(** Remove and return the last element, or [None] if the
    array is empty. *)

val pop_exn : 'a t -> 'a
(** Remove the last element, or raise an exception if the
    array is empty.
    @raise Invalid_argument on an empty array. *)

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

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold on elements of the array *)

val exists : ('a -> bool) -> 'a t -> bool

val for_all : ('a -> bool) -> 'a t -> bool

val get : 'a t -> int -> 'a
(** Access element by its index, or
    @raise Invalid_argument if bad index. *)

val set : 'a t -> int -> 'a -> unit
(** Modify element at given index, or
    @raise Invalid_argument if the index is
    invalid (i.e. not in [[0.. length v-1]]). *)

val unsafe_get : 'a t -> int -> 'a

val unsafe_set : 'a t -> int -> 'a -> unit

val blit : 'a t -> int -> 'a t -> int -> int -> unit

val length : _ t -> int
(** Number of elements in the array. *)

val of_array : 'a array -> 'a t
(** [of_array a] returns a array corresponding to the array [a].
    Operates in [O(n)] time. *)

val of_list : 'a list -> 'a t

val to_array : 'a t -> 'a array
(** [to_array v] returns an array corresponding to the array [v]. *)

val to_list : 'a t -> 'a list
(** Return a list with the elements contained in the array. *)

val of_seq : 'a Seq.t -> 'a t
(** Convert an Iterator to a array. *)

val to_seq : 'a t -> 'a Seq.t
(** Return an iterator with the elements contained in the array. *)

val to_seq_rev : 'a t -> 'a Seq.t
(** Iterate over the array, starting from the last (top) element. *)
