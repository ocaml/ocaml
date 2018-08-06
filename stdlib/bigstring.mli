(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Nicolas Ojeda Bar, LexiFi                        *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Large, one-dimensional, byte arrays.

    This module implements one-dimensional arrays of bytes, thereafter referred
    to as 'big strings'.  The implementation allows efficient sharing of large
    byte arrays between OCaml code and C libraries.

    Bigstrings support all the OCaml ad-hoc polymorphic operations:
    - comparisons ([=], [<>], [<=], etc, as well as {!Pervasives.compare});
    - hashing (module [Hash]);
    - and structured input-output (the functions from the
     {!Marshal} module, as well as {!Pervasives.output_value}
     and {!Pervasives.input_value}).
*)


(** @since 4.08.0 *)

type t = bigstring
(** The type of big strings. *)

val create : int -> t
(** [create n] returns a new big string with size [n]. *)

val init : int -> (int -> char) -> t
(** [init n f] returns a fresh big string of length [n] with
    element [i] initialized to the result of [f i] (in increasing index order).

    Raises [Invalid_argument] if [n < 0]. *)

val copy : t -> t
(** Returns a new big string containing the same bytes as the argument. *)

val length : t -> int
(** Return the length of the given big string. *)

val get : t -> int -> char
(** [get a x] returns the element of [a] at index [x].  [x] must be greater or
    equal than [0] and strictly less than [length a]. Otherwise,
    [Invalid_argument] is raised. *)

val set : t -> int -> char -> unit
(** [set a x v] stores the value [v] at index [x] in [a].  [x] must be inside
    the bounds of [a] as described in {!get}; otherwise, [Invalid_argument] is
    raised. *)

val unsafe_get : t -> int -> char
(** Like {!get}, but bounds checking is not always performed.
    Use with caution and only when the program logic guarantees that
    the access is within bounds. *)

val unsafe_set : t -> int -> char -> unit
(** Like {!set}, but bounds checking is not always performed.
    Use with caution and only when the program logic guarantees that
    the access is within bounds. *)

val sub : t -> int -> int -> t
(** [sub s start len] returns a new big string sequence of length [len],
    containing the subsequence of [s] that starts at position [start] and has
    length [len]. *)

val sub_nocopy : t -> int -> int -> t
(** Same as {!sub}, except that the resulting big string shares the same storage
    as the argument. *)

val sub_bytes : t -> int -> int -> bytes
(** Same as {!sub}, but returns [bytes]. *)

val sub_string : t -> int -> int -> string
(** Same as {!sub}, but returns a [string]. *)

val blit : t -> int -> t -> int -> int -> unit
(** [blit src srcoff dst dstoff len] copies [len] bytes from big string [src] at
    index [srcidx], to big string [dst], starting at index [dstoff]. It works
    correctly even if [src] and [dst] are the same big string, and the source
    and destination intervals overlap.

    Raises [Invalid_argument] if [srcoff] and [len] do not designate a valid
    range of [src], or if [dstoff] and [len] do not desginate a valid range of
    [dst]. *)

val blit_bytes : bytes -> int -> t -> int -> int -> unit
(** Same as {!blit}, but read data from a byte sequence. *)

val blit_string : string -> int -> t -> int -> int -> unit
(** Same as {!blit}, but read data from a [string]. *)

val fill : t -> char -> unit
(** Fill the given big string with the given value. *)

val of_string : string -> t
(** Return a new big string that contains the same bytes as the given string. *)

val to_string : t -> string
(** Return a new string that contains the same bytes as the given big string. *)

val of_bytes : bytes -> t
(** Return a new big string that contains the same bytes as the given byte
    sequence. *)

val to_bytes : t -> bytes
(** Return a new byte sequence that contains the same bytes as the given big
    string. *)

val to_seq : t -> char Seq.t
(** Iterate on the big string, in increasing index order. Modifications of the
    big string during iteration will be reflected in the iterator. *)

val get_uint8 : t -> int -> int
val get_int8 : t -> int -> int
val get_uint16_ne : t -> int -> int
val get_int16_ne : t -> int -> int
val get_int32_ne : t -> int -> int32
val get_int64_ne : t -> int -> int64
val set_int8 : t -> int -> int -> unit
val set_int16_ne : t -> int -> int -> unit
val set_int32_ne : t -> int -> int32 -> unit
val set_int64_ne : t -> int -> int64 -> unit
val get_int16_be : t -> int -> int
val get_int16_le : t -> int -> int
val get_uint16_be : t -> int -> int
val get_uint16_le : t -> int -> int
val get_int32_be : t -> int -> int32
val get_int32_le : t -> int -> int32
val get_int64_be : t -> int -> int64
val get_int64_le : t -> int -> int64
val set_int16_be : t -> int -> int -> unit
val set_int16_le : t -> int -> int -> unit
val set_int32_be : t -> int -> int32 -> unit
val set_int32_le : t -> int -> int32 -> unit
val set_int64_be : t -> int -> int64 -> unit
val set_int64_le : t -> int -> int64 -> unit

open Bigarray

val of_array1 : ('a, 'b, 'c) Array1.t -> t
(** Return a big string with the same bytes as the given one-dimensional big
    array. No copy is made: the resulting big string shares the same storage as
    the given big array. *)

val of_array2 : ('a, 'b, 'c) Array2.t -> t
(** Return a big string with the same bytes as the given two-dimensional big
    array. No copy is made: the resulting big string shares the same storage as
    the given big array. *)

val of_array3 : ('a, 'b, 'c) Array3.t -> t
(** Return a big string with the same bytes as the given three-dimensional big
    array. No copy is made: the resulting big string shares the same storage as
    the given big array. *)

val of_genarray : ('a, 'b, 'c) Genarray.t -> t
(** Return a big string with the same bytes as the given big array. No copy is
    made: the resulting big string shares the same storage as the given big
    array. *)

val to_array1 : t -> (char, int8_unsigned_elt, c_layout) Array1.t
(** Return a one-dimensional big array with the same bytes as the given big
    string. No copy is made: the resulting big array shares the same storage as
    the given big string. *)
