(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2025 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** 8-bit integers.

    This module provides operations on the type [t] of signed 8-bit integers.
    All arithmetic operations over [t] are taken modulo 2{^8}.

    Internally, values of type [t] are represented as [int].

    @since 5.4 *)

type t = private int
(** The type of 8-bit integers. *)

val zero : t
(** The 8-bit integer 0. *)

val one : t
(** The 8-bit integer 1. *)

val minus_one : t
(** The 8-bit integer -1. *)

val neg : t -> t
(** Unary negation. *)

val add : t -> t -> t
(** Addition. *)

val sub : t -> t -> t
(** Subtraction. *)

val mul : t -> t -> t
(** Multiplication. *)

val div : t -> t -> t
(** Integer division. This division rounds the real quotient of its arguments
    towards zero, as specified for {!Stdlib.(/)}. @raise Division_by_zero if the
    second argument is zero. *)

val unsigned_div : t -> t -> t
(** Same as {!div}, except that arguments and result are interpreted as {e
    unsigned} 8-bit integers. *)

val rem : t -> t -> t
(** Integer remainder. If [y] is not zero, the result of [Int8.rem x y]
    satisfies the following property:
    [x = Int8.add (Int8.mul (Int8.div x y) y) (Int8.rem x y)]. If [y = 0],
    [Int8.rem x y] raises [Division_by_zero]. *)

val unsigned_rem : t -> t -> t
(** Same as {!rem}, except that arguments and result are interpreted as {e
    unsigned} 8-bit integers. *)

val succ : t -> t
(** Successor. [Int8.succ x] is [Int8.add x Int8.one]. *)

val pred : t -> t
(** Predecessor. [Int8.pred x] is [Int8.sub x Int8.one]. *)

val abs : t -> t
(** [abs x] is the absolute value of [x]. On [min_int] this is [min_int] itself
    and thus remains negative. *)

val max_int : t
(** The greatest representable 8-bit integer, 2{^7} - 1. *)

val min_int : t
(** The smallest representable 8-bit integer, -2{^7}. *)

val logand : t -> t -> t
(** Bitwise logical and. *)

val logor : t -> t -> t
(** Bitwise logical or. *)

val logxor : t -> t -> t
(** Bitwise logical exclusive or. *)

val lognot : t -> t
(** Bitwise logical negation. *)

val shift_left : t -> int -> t
(** [Int8.shift_left x y] shifts [x] to the left by [y] bits. The result is
    unspecified if [y < 0] or [y >= 8]. *)

val shift_right : t -> int -> t
(** [Int8.shift_right x y] shifts [x] to the right by [y] bits. This is an
    arithmetic shift: the sign bit of [x] is replicated and inserted in the
    vacated bits. The result is unspecified if [y < 0] or [y >= 8]. *)

val shift_right_logical : t -> int -> t
(** [Int8.shift_right_logical x y] shifts [x] to the right by [y] bits. This is
    a logical shift: zeroes are inserted in the vacated bits regardless of the
    sign of [x]. The result is unspecified if [y < 0] or [y >= 8]. *)

val of_int : int -> t
(** Convert the given integer (type [int]) to a 8-bit integer (type [t]). The
    argument is taken modulo 2{^8}. *)

external to_int : t -> int = "%identity"
(** Convert the given 8-bit integer (type [t]) to an integer (type [int]). *)

val unsigned_to_int : t -> int option
(** Same as {!to_int}, but interprets the argument as an {e unsigned} integer.
    Returns [None] if the unsigned value of the argument cannot fit into an
    [int]. *)

val of_string : string -> t
(** Convert the given string to a 8-bit integer. The string is read in decimal
    (by default, or if the string begins with [0u]) or in hexadecimal, octal or
    binary if the string begins with [0x], [0o] or [0b] respectively.

    The [0u] prefix reads the input as an unsigned integer in the range
    [[0, 2*Int8.max_int+1]]. If the input exceeds {!Int8.max_int} it is
    converted to the signed integer [Int8.min_int + input - Int8.max_int - 1].

    The [_] (underscore) character can appear anywhere in the string and is
    ignored.

    @raise Failure if the given string is not a valid representation of an
    integer, or if the integer represented exceeds the range of integers
    representable in type [t]. *)

val of_string_opt : string -> t option
(** Same as [of_string], but return [None] instead of raising. *)

val to_string : t -> string
(** Return the string representation of its argument, in signed decimal. *)

val compare : t -> t -> int
(** The comparison function for 8-bit integers, with the same specification as
    {!Stdlib.compare}. Along with the type [t], this function [compare] allows
    the module [Int8] to be passed as argument to the functors {!Set.Make} and
    {!Map.Make}. *)

val unsigned_compare : t -> t -> int
(** Same as {!compare}, except that arguments are interpreted as {e unsigned}
    8-bit integers. *)

val equal : t -> t -> bool
(** [equal x y] is [true] if and only if [x = y]. *)

val min : t -> t -> t
(** Return the smaller of the two arguments. *)

val max : t -> t -> t
(** Return the greater of the two arguments. *)

val seeded_hash : int -> t -> int
(** A seeded hash function for 8-bit ints, with the same output value as
    {!Hashtbl.seeded_hash}. This function allows this module to be passed as
    argument to the functor {!Hashtbl.MakeSeeded}. *)

val hash : t -> int
(** An unseeded hash function for 8-bit ints, with the same output value as
    {!Hashtbl.hash}. This function allows this module to be passed as argument
    to the functor {!Hashtbl.Make}. *)
