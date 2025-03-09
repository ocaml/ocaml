(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*               Antal Spector-Zabusky, Jane Street, New York             *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

(** Operations on immutable arrays.  This module mirrors the API of [Array], but
    omits functions that assume mutability; in addition to obviously mutating
    functions, it omits [copy] along with the functions [make], [create_float],
    and [make_matrix] that produce all-constant arrays.  The exception is the
    sorting functions, which are given a copying API to replace the in-place
    one.

    @since 5.4
*)

type +'a t = 'a iarray
(** An alias for the type of immutable arrays. *)

external length : 'a iarray -> int = "%array_length"
(** Return the length (number of elements) of the given immutable array. *)

external get : 'a iarray -> int -> 'a = "%array_safe_get"
(** [get a n] returns the element number [n] of immutable array [a].
   The first element has number 0.
   The last element has number [length a - 1].

   @raise Invalid_argument
   if [n] is outside the range 0 to [(length a - 1)]. *)

val init : int -> (int -> 'a) -> 'a iarray
(** [init n f] returns a fresh immutable array of length [n],
   with element number [i] initialized to the result of [f i].
   In other terms, [init n f] tabulates the results of [f]
   applied to the integers [0] to [n-1].

   @raise Invalid_argument if [n < 0] or [n > Sys.max_array_length].
   If the return type of [f] is [float], then the maximum
   size is only [Sys.max_array_length / 2]. *)

val append : 'a iarray -> 'a iarray -> 'a iarray
(** [append v1 v2] returns a fresh immutable array containing the
   concatenation of the immutable arrays [v1] and [v2].
   @raise Invalid_argument if
   [length v1 + length v2 > Sys.max_array_length]. *)

val concat : 'a iarray list -> 'a iarray
(** Same as {!append}, but concatenates a list of immutable arrays. *)

val sub : 'a iarray -> pos:int -> len:int -> 'a iarray
(** [sub a ~pos ~len] returns a fresh immutable array of length [len],
   containing the elements number [pos] to [pos + len - 1]
   of immutable array [a].  This creates a copy of the selected
   portion of the immutable array.

   @raise Invalid_argument if [pos] and [len] do not
   designate a valid subarray of [a]; that is, if
   [pos < 0], or [len < 0], or [pos + len > length a]. *)

val to_list : 'a iarray -> 'a list
(** [to_list a] returns the list of all the elements of [a]. *)

val of_list : 'a list -> 'a iarray
(** [of_list l] returns a fresh immutable array containing the elements
   of [l].

   @raise Invalid_argument if the length of [l] is greater than
   [Sys.max_array_length]. *)

(** {1 Converting to and from mutable arrays} *)

val to_array : 'a iarray -> 'a array
(** [to_array a] returns a mutable copy of the immutable array [a]; that is, a
   fresh (mutable) array containing the same elements as [a] *)

val of_array : 'a array -> 'a iarray
(** [of_array ma] returns an immutable copy of the mutable array [ma]; that is,
   a fresh immutable array containing the same elements as [ma] *)

(** {1 Comparison} *)

val equal : ('a -> 'a -> bool) -> 'a iarray -> 'a iarray -> bool
(** [eq [|a1; ...; an|] [|b1; ..; bm|]] holds when the two input immutable
    arrays have the same length, and for each pair of elements [ai, bi] at the
    same position we have [eq ai bi]. *)

val compare : ('a -> 'a -> int) -> 'a iarray -> 'a iarray -> int
(** Provided the function [cmp] defines a preorder on elements,
    [compare cmp a b] compares first [a] and [b] by their length, and then, if
      equal, by their elements according to the lexicographic preorder.

    For more details on comparison functions, see {!Iarray.sort}. *)

(** {1 Iterators} *)

val iter : ('a -> unit) -> 'a iarray -> unit
(** [iter f a] applies function [f] in turn to all
   the elements of [a].  It is equivalent to
   [f (get a 0); f (get a 1); ...; f (get a (length a - 1)); ()]. *)

val iteri : (int -> 'a -> unit) -> 'a iarray -> unit
(** Same as {!iter}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)

val map : ('a -> 'b) -> 'a iarray -> 'b iarray
(** [map f a] applies function [f] to all the elements of [a],
   and builds an immutable array with the results returned by [f]:
   [[| f (get a 0); f (get a 1); ...; f (get a (length a - 1)) |]]. *)

val mapi : (int -> 'a -> 'b) -> 'a iarray -> 'b iarray
(** Same as {!map}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b iarray -> 'a
(** [fold_left f init a] computes
   [f (... (f (f init (get a 0)) (get a 1)) ...) (get a n-1)],
   where [n] is the length of the immutable array [a]. *)

val fold_left_map :
  ('a -> 'b -> 'a * 'c) -> 'a -> 'b iarray -> 'a * 'c iarray
(** [fold_left_map] is a combination of {!fold_left} and {!map} that threads an
    accumulator through calls to [f]. *)

val fold_right : ('b -> 'a -> 'a) -> 'b iarray -> 'a -> 'a
(** [fold_right f a init] computes
   [f (get a 0) (f (get a 1) ( ... (f (get a (n-1)) init) ...))],
   where [n] is the length of the immutable array [a]. *)


(** {1 Iterators on two arrays} *)


val iter2 : ('a -> 'b -> unit) -> 'a iarray -> 'b iarray -> unit
(** [iter2 f a b] applies function [f] to all the elements of [a]
   and [b].
   @raise Invalid_argument if the immutable arrays are not the same size.
   *)

val map2 : ('a -> 'b -> 'c) -> 'a iarray -> 'b iarray -> 'c iarray
(** [map2 f a b] applies function [f] to all the elements of [a]
   and [b], and builds an immutable array with the results returned by [f]:
   [[| f (get a 0) (get b 0);
       ...;
       f (get a (length a - 1)) (get b (length b - 1))|]].
   @raise Invalid_argument if the immutable arrays are not the same size. *)


(** {1 Array scanning} *)

val for_all : ('a -> bool) -> 'a iarray -> bool
(** [for_all f [|a1; ...; an|]] checks if all elements
   of the immutable array satisfy the predicate [f]. That is, it returns
   [(f a1) && (f a2) && ... && (f an)]. *)

val exists : ('a -> bool) -> 'a iarray -> bool
(** [exists f [|a1; ...; an|]] checks if at least one element of
    the immutable array satisfies the predicate [f]. That is, it returns
    [(f a1) || (f a2) || ... || (f an)]. *)

val for_all2 : ('a -> 'b -> bool) -> 'a iarray -> 'b iarray -> bool
(** Same as {!for_all}, but for a two-argument predicate.
   @raise Invalid_argument if the two immutable arrays have different
   lengths. *)

val exists2 : ('a -> 'b -> bool) -> 'a iarray -> 'b iarray -> bool
(** Same as {!exists}, but for a two-argument predicate.
   @raise Invalid_argument if the two immutable arrays have different
   lengths. *)

val mem : 'a -> 'a iarray -> bool
(** [mem a set] is true if and only if [a] is structurally equal
    to an element of [l] (i.e. there is an [x] in [l] such that
    [compare a x = 0]). *)

val memq : 'a -> 'a iarray -> bool
(** Same as {!mem}, but uses physical equality instead of structural equality
    to compare array elements. *)

val find_opt : ('a -> bool) -> 'a iarray -> 'a option
(** [find_opt f a] returns the first element of the immutable array [a] that
    satisfies the predicate [f], or [None] if there is no value that satisfies
    [f] in the array [a]. *)

val find_index : ('a -> bool) -> 'a iarray -> int option
(** [find_index f a] returns [Some i], where [i] is the index of the first
    element of the array [a] that satisfies [f x], if there is such an
    element.

    It returns [None] if there is no such element. *)

val find_map : ('a -> 'b option) -> 'a iarray -> 'b option
(** [find_map f a] applies [f] to the elements of [a] in order, and returns the
    first result of the form [Some v], or [None] if none exist. *)

val find_mapi : (int -> 'a -> 'b option) -> 'a iarray -> 'b option
(** Same as [find_map], but the predicate is applied to the index of
   the element as first argument (counting from 0), and the element
   itself as second argument. *)

(** {1 Arrays of pairs} *)

val split : ('a * 'b) iarray -> 'a iarray * 'b iarray
(** [split [|(a1,b1); ...; (an,bn)|]] is
    [([|a1; ...; an|], [|b1; ...; bn|])]. *)

val combine : 'a iarray -> 'b iarray -> ('a * 'b) iarray
(** [combine [|a1; ...; an|] [|b1; ...; bn|]] is [[|(a1,b1); ...; (an,bn)|]].
    Raise [Invalid_argument] if the two immutable arrays have different
    lengths. *)

(** {1 Sorting} *)

val sort : ('a -> 'a -> int) -> 'a iarray -> 'a iarray
(** Sort an immutable array in increasing order according to a comparison
   function.  The comparison function must return 0 if its arguments
   compare as equal, a positive integer if the first is greater,
   and a negative integer if the first is smaller (see below for a
   complete specification).  For example, {!Stdlib.compare} is
   a suitable comparison function. The result of calling [sort] is a fresh
   immutable array containing the same elements as the original sorted in
   increasing order. Other than this fresh array, [sort] is guaranteed to run in
   constant heap space and (at most) logarithmic stack space.

   The current implementation uses Heap Sort.  It runs in constant
   stack space.

   Specification of the comparison function:
   Let [a] be the immutable array and [cmp] the comparison function.  The
   following must be true for all [x], [y], [z] in [a] :
-   [cmp x y] > 0 if and only if [cmp y x] < 0
-   if [cmp x y] >= 0 and [cmp y z] >= 0 then [cmp x z] >= 0

   The result of [sort], which we'll call [a'], contains the same elements as
   [a], reordered in such a way that for all i and j valid indices of [a] (or
   equivalently, of [a']):
-   [cmp (get a' i) (get a' j)] >= 0 if and only if i >= j
*)

val stable_sort : ('a -> 'a -> int) -> 'a iarray -> 'a iarray
(** Same as {!sort}, but the sorting algorithm is stable (i.e.
   elements that compare equal are kept in their original order) and
   not guaranteed to run in constant heap space.

   The current implementation uses Merge Sort. It uses a temporary array of
   length [n/2], where [n] is the length of the immutable array.  It is usually
   faster than the current implementation of {!sort}.
*)

val fast_sort : ('a -> 'a -> int) -> 'a iarray -> 'a iarray
(** Same as {!sort} or {!stable_sort}, whichever is
    faster on typical input. *)

(** {1 Iterators} *)

val to_seq : 'a iarray -> 'a Seq.t
(** Iterate on the immutable array, in increasing order. *)

val to_seqi : 'a iarray -> (int * 'a) Seq.t
(** Iterate on the immutable array, in increasing order, yielding indices along
    elements. *)

val of_seq : 'a Seq.t -> 'a iarray
(** Create an immutable array from the generator *)

(**/**)

(** {1 Undocumented functions} *)

(* The following is for system use only. Do not call directly. *)

external unsafe_get : 'a iarray -> int -> 'a = "%array_unsafe_get"
