(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                             Jeremy Yallop                              *)
(*                                                                        *)
(*   Copyright 2020 Jeremy Yallop                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Unsigned 64-bit integers.

    This module provides operations on the type [uint64] of unsigned
    64-bit integers, which is guaranteed to be exactly 64-bit wide on
    all platforms.  All arithmetic operations over [uint64] are taken
    modulo 2{^64}.

    @since 4.11.0
 *)

val zero : uint64
(** The unsigned 64-bit integer 0. *)

val one : uint64
(** The unsigned 64-bit integer 1. *)

val add : uint64 -> uint64 -> uint64
(** Addition. *)

val sub : uint64 -> uint64 -> uint64
(** Subtraction. *)

val mul : uint64 -> uint64 -> uint64
(** Multiplication. *)

val div : uint64 -> uint64 -> uint64
(** Division.  Raise {!Division_by_zero} if the second argument is zero. *)

val rem : uint64 -> uint64 -> uint64
(** Integer remainder.  Raise {!Division_by_zero} if the second argument is
    zero. *)

val succ : uint64 -> uint64
(** Successor. *)

val pred : uint64 -> uint64
(** Predecessor. *)

val max_int : uint64
(** The greatest representable integer. *)

val logand : uint64 -> uint64 -> uint64
(** Bitwise logical and. *)

val logor : uint64 -> uint64 -> uint64
(** Bitwise logical or. *)

val logxor : uint64 -> uint64 -> uint64
(** Bitwise logical exclusive or. *)

val lognot : uint64 -> uint64
(** Bitwise logical negation. *)

val shift_left : uint64 -> int -> uint64
(** [Uint64.shift_left x y] shifts [x] to the left by [y] bits.
    The result is unspecified if [y < 0] or [y >= 64]. *)

val shift_right : uint64 -> int -> uint64
(** [Uint64.shift_right x y] shifts [x] to the right by [y] bits.
    The result is unspecified if [y < 0] or [y >= 64]. *)

val of_int : int -> uint64
(** Convert the given int value to an unsigned 64-bit integer. *)

val to_int : uint64 -> int
(** Convert the given unsigned 64-bit integer value to an int. *)

val of_int64 : int64 -> uint64
(** Convert the given int64 value to an unsigned 64-bit integer. *)

val to_int64 : uint64 -> int64
(** Convert the given unsigned 64-bit integer value to an int64. *)

val of_nativeint : nativeint -> uint64
(** Convert the given native integer (type [nativeint]) to a 64-bit
    unsigned integer (type [uint64]). *)

val to_nativeint : uint64 -> nativeint
(** Convert the given 64-bit unsigned integer (type [uint64]) to a
    native integer. *)

val of_uint32 : uint32 -> uint64
(** Convert the given uint32 value to an unsigned 64-bit integer. *)

val to_uint32 : uint64 -> uint32
(** Convert the given unsigned 64-bit integer value to a uint32. *)

val of_int32 : int32 -> uint64
(** Convert the given int32 value to an unsigned 64-bit integer. *)

val to_int32 : uint64 -> int32
(** Convert the given unsigned 64-bit integer value to an int32. *)

val of_float : float -> uint64
(** Convert the given floating-point number to a 64-bit unsigned integer,
   discarding the fractional part (truncate towards 0).
   The result of the conversion is undefined if, after truncation,
   the number is outside the range \[0, {!Uint64.max_int}\]. *)

val to_float : uint64 -> float
(** Convert the given 64-bit unsigned integer to a floating-point
    number. *)

val of_string : string -> uint64
(** Convert the given string to an unsigned 64-bit integer.
   The string is read in decimal (by default, or if the string begins
   with [0u]) or in hexadecimal, octal or binary if the string begins
   with [0x], [0o] or [0b] respectively.
   Raise {!Failure} ["Uint64.of_string"] if the given string is not a
   valid representation of an unsigned 64-bit integer. *)

val of_string_opt: string -> uint64 option
(** Same as [of_string], but return [None] instead of raising. *)

val to_string : uint64 -> string
(** Return the string representation of its argument. *)

val compare : uint64 -> uint64 -> int
(** The comparison function for unsigned 64-bit integers, with the same
    specification as {!Pervasives.compare}.  Along with the type [t],
    this function [compare] allows the module [Int64] to be passed as
    argument to the functors {!Set.Make} and {!Map.Make}. *)

val equal : uint64 -> uint64 -> bool
(** The equal function for unsigned 64-bit integers. *)

type t = uint64
