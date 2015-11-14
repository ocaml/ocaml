(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Float array operations. *)

type t = [| mutable float |]
(** The type of float arrays *)

external length : t -> int = "%float_array_length"
(** Return the length (number of elements) of the given array. *)

external get : t -> int -> float = "%float_array_safe_get"
(** [FloatArray.get a n] returns the element number [n] of float array
   [a].  The first element has number 0.  The last element has number
   [FloatArray.length a - 1].

   Raise [Invalid_argument "index out of bounds"]
   if [n] is outside the range 0 to [(FloatArray.length a - 1)]. *)

external set : t -> int -> float -> unit = "%float_array_safe_set"
(** [FloatArray.set a n x] modifies array [a] in place, replacing
   element number [n] with [x].

   Raise [Invalid_argument "index out of bounds"]
   if [n] is outside the range 0 to [FloatArray.length a - 1]. *)

external create : int -> t = "caml_create_double_array"
(** [FloatArray.create n] returns a fresh float array of length [n],
   with uninitialized data.

   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length /
   2]. *)

external make : int -> float -> t = "caml_make_double_array"
(** [FloatArray.make n x] returns a fresh float array of length [n],
   initialized with [x].  All the elements of this new array are
   initially physically equal to [x] (in the sense of the [==]
   predicate).

   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length /
   2]. *)

val init : int -> (int -> float) -> t
(** [FloatArray.init n f] returns a fresh float array of length [n],
   with element number [i] initialized to the result of [f i].
   In other terms, [FloatArray.init n f] tabulates the results of [f]
   applied to the integers [0] to [n-1].

   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length /
    2]. *)

val make_matrix : int -> int -> float -> t array
(** [FloatArray.make_matrix dimx dimy e] returns a two-dimensional array
   (an array of float arrays) with first dimension [dimx] and
   second dimension [dimy]. All the elements of this new matrix
   are initially physically equal to [e].

   Raise [Invalid_argument] if [dimx] or [dimy] is negative or
   greater than [Sys.max_array_length / 2]. *)

val append : t -> t -> t
(** [FloatArray.append v1 v2] returns a fresh float array containing the
   concatenation of the float arrays [v1] and [v2]. *)

val concat : t list -> t
(** Same as [FloatArray.append], but concatenates a list of float
    arrays. *)

val sub : t -> int -> int -> t
(** [FloatArray.sub a start len] returns a fresh float array of length [len],
   containing the elements number [start] to [start + len - 1]
   of float array [a].

   Raise [Invalid_argument "FloatArray.sub"] if [start] and [len] do not
   designate a valid subarray of [a]; that is, if
   [start < 0], or [len < 0], or [start + len > FloatArray.length a]. *)

val copy : t -> t
(** [FloatArray.copy a] returns a copy of [a], that is, a fresh float
   array containing the same elements as [a]. *)

val fill : t -> int -> int -> float -> unit
(** [FloatArray.fill a ofs len x] modifies the float array [a] in place,
   storing [x] in elements number [ofs] to [ofs + len - 1].

   Raise [Invalid_argument "FloatArray.fill"] if [ofs] and [len] do not
   designate a valid subarray of [a]. *)

val blit : t -> int -> t -> int -> int -> unit
(** [FloatArray.blit v1 o1 v2 o2 len] copies [len] elements from float
   array [v1], starting at element number [o1], to float array [v2],
   starting at element number [o2]. It works correctly even if [v1] and
   [v2] are the same float array, and the source and destination chunks
   overlap.

   Raise [Invalid_argument "FloatArray.blit"] if [o1] and [len] do not
   designate a valid subarray of [v1], or if [o2] and [len] do not
   designate a valid subarray of [v2]. *)

val to_list : t -> float list
(** [FloatArray.to_list a] returns the list of all the elements of [a]. *)

val of_list : float list -> t
(** [FloatArray.of_list l] returns a fresh float array containing the
   elements of [l]. *)

val iter : (float -> unit) -> t -> unit
(** [FloatArray.iter f a] applies function [f] in turn to all
   the elements of [a]. *)

val map : (float -> float) -> t -> t
(** [FloatArray.map f a] applies function [f] to all the elements of [a],
   and builds an array with the results returned by [f]. *)

val iteri : (int -> float -> unit) -> t -> unit
(** Same as {!FloatArray.iter}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)

val mapi : (int -> float -> float) -> t -> t
(** Same as {!FloatArray.map}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)

val fold_left : ('a -> float -> 'a) -> 'a -> t -> 'a
(** [FloatArray.fold_left f x a] computes
   [f (... (f (f x (get a 0)) (get a 1)) ...) (get a (n-1))],
   where [n] is the length of the float array [a]. *)

val fold_right : (float -> 'a -> 'a) -> t -> 'a -> 'a
(** [FloatArray.fold_right f a x] computes
   [f (get a 0) (f (get a 1) ( ... (f (get a (n-1)) x) ...))],
   where [n] is the length of the float array [a]. *)


(** {6 Sorting} *)

val sort : (float -> float -> int) -> t -> unit
(** Sort a float array in increasing order according to a comparison
   function.  The comparison function must return 0 if its arguments
   compare as equal, a positive integer if the first is greater,
   and a negative integer if the first is smaller (see below for a
   complete specification).  For example, {!Pervasives.compare} is
   a suitable comparison function, provided there are no floating-point
   NaN values in the data.  After calling [FloatArray.sort], the
   float array is sorted in place in increasing order.
   [FloatArray.sort] is guaranteed to run in constant heap space
   and (at most) logarithmic stack space.

   The current implementation uses Heap Sort.  It runs in constant
   stack space.

   Specification of the comparison function:

   Let [a] be the float array and [cmp] the comparison function.  The
   following must be true for all x, y, z in a :
   - [cmp x y] > 0 if and only if [cmp y x] < 0
   - if [cmp x y] >= 0 and [cmp y z] >= 0 then [cmp x z] >= 0

   When [FloatArray.sort] returns, [a] contains the same elements as before,
   reordered in such a way that for all i and j valid indices of [a] :
   - [cmp (get a i) (get a j)] >= 0 if and only if i >= j
*)

val stable_sort : (float -> float -> int) -> t -> unit
(** Same as {!FloatArray.sort}, but the sorting algorithm is stable (i.e.
   elements that compare equal are kept in their original order) and
   not guaranteed to run in constant heap space.

   The current implementation uses Merge Sort. It uses [n/2] words of heap
   space, where [n] is the length of the array.  It is usually faster than the
   current implementation of {!FloatArray.sort}.
*)

val fast_sort : (float -> float -> int) -> t -> unit
(** Same as {!FloatArray.sort} or {!FloatArray.stable_sort}, whichever is
    faster on typical input.
*)


(**/**)
(** {6 Undocumented functions} *)

(* The following is for system use only. Do not call directly. *)

external unsafe_get : t -> int -> float = "%float_array_unsafe_get"
external unsafe_set : t -> int -> float -> unit = "%float_array_unsafe_set"
