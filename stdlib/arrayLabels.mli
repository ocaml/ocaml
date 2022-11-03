(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* NOTE:
   If this file is arrayLabels.mli, run tools/sync_stdlib_docs after editing it
   to generate array.mli.

   If this file is array.mli, do not edit it directly -- edit
   arrayLabels.mli instead.
 *)

(** Array operations.

    The labeled version of this module can be used as described in the
    {!StdLabels} module.
*)

type 'a t = 'a array
(** An alias for the type of arrays. *)

external length : 'a array -> int = "%array_length"
(** Return the length (number of elements) of the given array. *)

external get : 'a array -> int -> 'a = "%array_safe_get"
(** [get a n] returns the element number [n] of array [a].
   The first element has number 0.
   The last element has number [length a - 1].
   You can also write [a.(n)] instead of [get a n].

   @raise Invalid_argument
   if [n] is outside the range 0 to [(length a - 1)]. *)

external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
(** [set a n x] modifies array [a] in place, replacing
   element number [n] with [x].
   You can also write [a.(n) <- x] instead of [set a n x].

   @raise Invalid_argument
   if [n] is outside the range 0 to [length a - 1]. *)

external make : int -> 'a -> 'a array = "caml_make_vect"
(** [make n x] returns a fresh array of length [n],
   initialized with [x].
   All the elements of this new array are initially
   physically equal to [x] (in the sense of the [==] predicate).
   Consequently, if [x] is mutable, it is shared among all elements
   of the array, and modifying [x] through one of the array entries
   will modify all other entries at the same time.

   @raise Invalid_argument if [n < 0] or [n > Sys.max_array_length].
   If the value of [x] is a floating-point number, then the maximum
   size is only [Sys.max_array_length / 2].*)

external create_float: int -> float array = "caml_make_float_vect"
(** [create_float n] returns a fresh float array of length [n],
    with uninitialized data.
    @since 4.03 *)

val init : int -> f:(int -> 'a) -> 'a array
(** [init n ~f] returns a fresh array of length [n],
   with element number [i] initialized to the result of [f i].
   In other terms, [init n ~f] tabulates the results of [f]
   applied in order to the integers [0] to [n-1].

   @raise Invalid_argument if [n < 0] or [n > Sys.max_array_length].
   If the return type of [f] is [float], then the maximum
   size is only [Sys.max_array_length / 2].*)

val make_matrix : dimx:int -> dimy:int -> 'a -> 'a array array
(** [make_matrix ~dimx ~dimy e] returns a two-dimensional array
   (an array of arrays) with first dimension [dimx] and
   second dimension [dimy]. All the elements of this new matrix
   are initially physically equal to [e].
   The element ([x,y]) of a matrix [m] is accessed
   with the notation [m.(x).(y)].

   @raise Invalid_argument if [dimx] or [dimy] is negative or
   greater than {!Sys.max_array_length}.
   If the value of [e] is a floating-point number, then the maximum
   size is only [Sys.max_array_length / 2]. *)

val append : 'a array -> 'a array -> 'a array
(** [append v1 v2] returns a fresh array containing the
   concatenation of the arrays [v1] and [v2].
   @raise Invalid_argument if
   [length v1 + length v2 > Sys.max_array_length]. *)

val concat : 'a array list -> 'a array
(** Same as {!append}, but concatenates a list of arrays. *)

val sub : 'a array -> pos:int -> len:int -> 'a array
(** [sub a ~pos ~len] returns a fresh array of length [len],
   containing the elements number [pos] to [pos + len - 1]
   of array [a].

   @raise Invalid_argument if [pos] and [len] do not
   designate a valid subarray of [a]; that is, if
   [pos < 0], or [len < 0], or [pos + len > length a]. *)

val copy : 'a array -> 'a array
(** [copy a] returns a copy of [a], that is, a fresh array
   containing the same elements as [a]. *)

val fill : 'a array -> pos:int -> len:int -> 'a -> unit
(** [fill a ~pos ~len x] modifies the array [a] in place,
   storing [x] in elements number [pos] to [pos + len - 1].

   @raise Invalid_argument if [pos] and [len] do not
   designate a valid subarray of [a]. *)

val blit :
  src:'a array -> src_pos:int -> dst:'a array -> dst_pos:int -> len:int ->
    unit
(** [blit ~src ~src_pos ~dst ~dst_pos ~len] copies [len] elements
   from array [src], starting at element number [src_pos], to array [dst],
   starting at element number [dst_pos]. It works correctly even if
   [src] and [dst] are the same array, and the source and
   destination chunks overlap.

   @raise Invalid_argument if [src_pos] and [len] do not
   designate a valid subarray of [src], or if [dst_pos] and [len] do not
   designate a valid subarray of [dst]. *)

val to_list : 'a array -> 'a list
(** [to_list a] returns the list of all the elements of [a]. *)

val of_list : 'a list -> 'a array
(** [of_list l] returns a fresh array containing the elements
   of [l].

   @raise Invalid_argument if the length of [l] is greater than
   [Sys.max_array_length]. *)

(** {1 Iterators} *)

val iter : f:('a -> unit) -> 'a array -> unit
(** [iter ~f a] applies function [f] in turn to all
   the elements of [a].  It is equivalent to
   [f a.(0); f a.(1); ...; f a.(length a - 1); ()]. *)

val iteri : f:(int -> 'a -> unit) -> 'a array -> unit
(** Same as {!iter}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)

val map : f:('a -> 'b) -> 'a array -> 'b array
(** [map ~f a] applies function [f] to all the elements of [a],
   and builds an array with the results returned by [f]:
   [[| f a.(0); f a.(1); ...; f a.(length a - 1) |]]. *)

val mapi : f:(int -> 'a -> 'b) -> 'a array -> 'b array
(** Same as {!map}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)

val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b array -> 'a
(** [fold_left ~f ~init a] computes
   [f (... (f (f init a.(0)) a.(1)) ...) a.(n-1)],
   where [n] is the length of the array [a]. *)

val fold_left_map :
  f:('a -> 'b -> 'a * 'c) -> init:'a -> 'b array -> 'a * 'c array
(** [fold_left_map] is a combination of {!fold_left} and {!map} that threads an
    accumulator through calls to [f].
    @since 4.13 *)

val fold_right : f:('b -> 'a -> 'a) -> 'b array -> init:'a -> 'a
(** [fold_right ~f a ~init] computes
   [f a.(0) (f a.(1) ( ... (f a.(n-1) init) ...))],
   where [n] is the length of the array [a]. *)


(** {1 Iterators on two arrays} *)


val iter2 : f:('a -> 'b -> unit) -> 'a array -> 'b array -> unit
(** [iter2 ~f a b] applies function [f] to all the elements of [a]
   and [b].
   @raise Invalid_argument if the arrays are not the same size.
   @since 4.03 (4.05 in ArrayLabels)
   *)

val map2 : f:('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
(** [map2 ~f a b] applies function [f] to all the elements of [a]
   and [b], and builds an array with the results returned by [f]:
   [[| f a.(0) b.(0); ...; f a.(length a - 1) b.(length b - 1)|]].
   @raise Invalid_argument if the arrays are not the same size.
   @since 4.03 (4.05 in ArrayLabels) *)


(** {1 Array scanning} *)

val for_all : f:('a -> bool) -> 'a array -> bool
(** [for_all ~f [|a1; ...; an|]] checks if all elements
   of the array satisfy the predicate [f]. That is, it returns
   [(f a1) && (f a2) && ... && (f an)].
   @since 4.03 *)

val exists : f:('a -> bool) -> 'a array -> bool
(** [exists ~f [|a1; ...; an|]] checks if at least one element of
    the array satisfies the predicate [f]. That is, it returns
    [(f a1) || (f a2) || ... || (f an)].
    @since 4.03 *)

val for_all2 : f:('a -> 'b -> bool) -> 'a array -> 'b array -> bool
(** Same as {!for_all}, but for a two-argument predicate.
   @raise Invalid_argument if the two arrays have different lengths.
   @since 4.11 *)

val exists2 : f:('a -> 'b -> bool) -> 'a array -> 'b array -> bool
(** Same as {!exists}, but for a two-argument predicate.
   @raise Invalid_argument if the two arrays have different lengths.
   @since 4.11 *)

val mem : 'a -> set:'a array -> bool
(** [mem a ~set] is true if and only if [a] is structurally equal
    to an element of [l] (i.e. there is an [x] in [l] such that
    [compare a x = 0]).
    @since 4.03 *)

val memq : 'a -> set:'a array -> bool
(** Same as {!mem}, but uses physical equality
   instead of structural equality to compare list elements.
   @since 4.03 *)

val find_opt : f:('a -> bool) -> 'a array -> 'a option
(** [find_opt ~f a] returns the first element of the array [a] that satisfies
    the predicate [f], or [None] if there is no value that satisfies [f] in the
    array [a].

    @since 4.13 *)

val find_map : f:('a -> 'b option) -> 'a array -> 'b option
(** [find_map ~f a] applies [f] to the elements of [a] in order, and returns the
    first result of the form [Some v], or [None] if none exist.

    @since 4.13 *)

(** {1 Arrays of pairs} *)

val split : ('a * 'b) array -> 'a array * 'b array
(** [split [|(a1,b1); ...; (an,bn)|]] is [([|a1; ...; an|], [|b1; ...; bn|])].

    @since 4.13 *)

val combine : 'a array -> 'b array -> ('a * 'b) array
(** [combine [|a1; ...; an|] [|b1; ...; bn|]] is [[|(a1,b1); ...; (an,bn)|]].
    Raise [Invalid_argument] if the two arrays have different lengths.

    @since 4.13 *)

(** {1 Sorting} *)

val sort : cmp:('a -> 'a -> int) -> 'a array -> unit
(** Sort an array in increasing order according to a comparison
   function.  The comparison function must return 0 if its arguments
   compare as equal, a positive integer if the first is greater,
   and a negative integer if the first is smaller (see below for a
   complete specification).  For example, {!Stdlib.compare} is
   a suitable comparison function. After calling [sort], the
   array is sorted in place in increasing order.
   [sort] is guaranteed to run in constant heap space
   and (at most) logarithmic stack space.

   The current implementation uses Heap Sort.  It runs in constant
   stack space.

   Specification of the comparison function:
   Let [a] be the array and [cmp] the comparison function.  The following
   must be true for all [x], [y], [z] in [a] :
-   [cmp x y] > 0 if and only if [cmp y x] < 0
-   if [cmp x y] >= 0 and [cmp y z] >= 0 then [cmp x z] >= 0

   When [sort] returns, [a] contains the same elements as before,
   reordered in such a way that for all i and j valid indices of [a] :
-   [cmp a.(i) a.(j)] >= 0 if and only if i >= j
*)

val stable_sort : cmp:('a -> 'a -> int) -> 'a array -> unit
(** Same as {!sort}, but the sorting algorithm is stable (i.e.
   elements that compare equal are kept in their original order) and
   not guaranteed to run in constant heap space.

   The current implementation uses Merge Sort. It uses a temporary array of
   length [n/2], where [n] is the length of the array.  It is usually faster
   than the current implementation of {!sort}.
*)

val fast_sort : cmp:('a -> 'a -> int) -> 'a array -> unit
(** Same as {!sort} or {!stable_sort}, whichever is
    faster on typical input. *)


(** {1 Arrays and Sequences} *)

val to_seq : 'a array -> 'a Seq.t
(** Iterate on the array, in increasing order. Modifications of the
    array during iteration will be reflected in the sequence.
    @since 4.07 *)

val to_seqi : 'a array -> (int * 'a) Seq.t
(** Iterate on the array, in increasing order, yielding indices along elements.
    Modifications of the array during iteration will be reflected in the
    sequence.
    @since 4.07 *)

val of_seq : 'a Seq.t -> 'a array
(** Create an array from the generator
    @since 4.07 *)

(**/**)

(** {1 Undocumented functions} *)

(* The following is for system use only. Do not call directly. *)

external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"

module Floatarray : sig
  external create : int -> floatarray = "caml_floatarray_create"
  external length : floatarray -> int = "%floatarray_length"
  external get : floatarray -> int -> float = "%floatarray_safe_get"
  external set : floatarray -> int -> float -> unit = "%floatarray_safe_set"
  external unsafe_get : floatarray -> int -> float = "%floatarray_unsafe_get"
  external unsafe_set : floatarray -> int -> float -> unit
      = "%floatarray_unsafe_set"
end
