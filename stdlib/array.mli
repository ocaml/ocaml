(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Array]: array operations *)

external length : 'a array -> int = "%array_length"
        (* Return the length (number of elements) of the given array. *)
external get: 'a array -> int -> 'a = "%array_safe_get"
        (* [Array.get a n] returns the element number [n] of array [a].
           The first element has number 0.
           The last element has number [Array.length a - 1].
           Raise [Invalid_argument "Array.get"]  if [n] is outside the range
           0 to [(Array.length a - 1)].
           You can also write [a.(n)] instead of [Array.get a n]. *)
external set: 'a array -> int -> 'a -> unit = "%array_safe_set"
        (* [Array.set a n x] modifies array [a] in place, replacing
           element number [n] with [x].
           Raise [Invalid_argument "Array.set"] if [n] is outside the range
           0 to [Array.length a - 1].
           You can also write [a.(n) <- x] instead of [Array.set a n x]. *)
external make: int -> 'a -> 'a array = "make_vect"
external create: int -> 'a -> 'a array = "make_vect"
        (* [Array.make n x] returns a fresh array of length [n],
           initialized with [x].
           All the elements of this new array are initially
           physically equal to [x] (in the sense of the [==] predicate).
           Consequently, if [x] is mutable, it is shared among all elements
           of the array, and modifying [x] through one of the array entries
           will modify all other entries at the same time. *)
val make_matrix: int -> int -> 'a -> 'a array array
val create_matrix: int -> int -> 'a -> 'a array array
        (* [Array.make_matrix dimx dimy e] returns a two-dimensional array
           (an array of arrays) with first dimension [dimx] and
           second dimension [dimy]. All the elements of this new matrix
           are initially physically equal to [e].
           The element ([x,y]) of a matrix [m] is accessed
           with the notation [m.(x).(y)]. *)
val append: 'a array -> 'a array -> 'a array
        (* [Array.append v1 v2] returns a fresh array containing the
           concatenation of arrays [v1] and [v2]. *)
val concat: 'a array list -> 'a array
        (* Same as [Array.append], but catenates a list of arrays. *)
val sub: 'a array -> int -> int -> 'a array
        (* [Array.sub a start len] returns a fresh array of length [len],
           containing the elements number [start] to [start + len - 1]
           of array [a].
           Raise [Invalid_argument "Array.sub"] if [start] and [len] do not
           designate a valid subarray of [a]; that is, if
           [start < 0], or [len < 0], or [start + len > Array.length a]. *)
val copy: 'a array -> 'a array
        (* [Array.copy a] returns a copy of [a], that is, a fresh array
           containing the same elements as [a]. *)
val fill: 'a array -> int -> int -> 'a -> unit
        (* [Array.fill a ofs len x] modifies the array [a] in place,
           storing [x] in elements number [ofs] to [ofs + len - 1].
           Raise [Invalid_argument "Array.fill"] if [ofs] and [len] do not
           designate a valid subarray of [a]. *)
val blit: 'a array -> int -> 'a array -> int -> int -> unit
        (* [Array.blit v1 o1 v2 o2 len] copies [len] elements
           from array [v1], starting at element number [o1], to array [v2],
           starting at element number [o2]. It works correctly even if
           [v1] and [v2] are the same array, and the source and
           destination chunks overlap.
           Raise [Invalid_argument "Array.blit"] if [o1] and [len] do not
           designate a valid subarray of [v1], or if [o2] and [len] do not
           designate a valid subarray of [v2]. *)
val to_list: 'a array -> 'a list
        (* [Array.to_list a] returns the list of all the elements of [a]. *)
val of_list: 'a list -> 'a array
        (* [Array.of_list l] returns a fresh array containing the elements
           of [l]. *)
val iter: ('a -> 'b) -> 'a array -> unit
        (* [Array.iter f a] applies function [f] in turn to all
           the elements of [a], discarding all the results:
           [f a.(0); f a.(1); ...; f a.(Array.length a - 1); ()]. *)
val map: ('a -> 'b) -> 'a array -> 'b array
        (* [Array.map f a] applies function [f] to all the elements of [a],
           and builds an array with the results returned by [f]:
           [[| f a.(0); f a.(1); ...; f a.(Array.length a - 1) |]]. *)
val iteri: (int -> 'a -> 'b) -> 'a array -> unit
val mapi: (int -> 'a -> 'b) -> 'a array -> 'b array
        (* Same as [Array.iter] and [Array.map] respectively, but the
           function is applied to the index of the element as first argument,
           and the element itself as second argument. *)
val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
        (* [Array.fold_left f x a] computes
           [f (... (f (f x a.(0)) a.(1)) ...) a.(n-1)],
           where [n] is the length of the array [a]. *)
val fold_right: ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a
        (* [Array.fold_right f a x] computes
           [f a.(0) (f a.(1) ( ... (f a.(n-1) x) ...))],
           where [n] is the length of the array [a]. *)
(*--*)

external unsafe_get: 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set: 'a array -> int -> 'a -> unit = "%array_unsafe_set"

