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

(** Unsigned 32-bit integers.

    This module provides operations on the type [uint32] of unsigned
    32-bit integers, which is guaranteed to be exactly 32-bit wide on
    all platforms.  All arithmetic operations over [uint32] are taken
    modulo 2{^32}.

    @since 4.11.0
 *)

val zero : uint32
(** The unsigned 32-bit integer 0. *)

val one : uint32
(** The unsigned 32-bit integer 1. *)

val add : uint32 -> uint32 -> uint32
(** Addition. *)

val sub : uint32 -> uint32 -> uint32
(** Subtraction. *)

val mul : uint32 -> uint32 -> uint32
(** Multiplication. *)

val div : uint32 -> uint32 -> uint32
(** Division.  Raise {!Division_by_zero} if the second argument is zero. *)

val rem : uint32 -> uint32 -> uint32
(** Integer remainder.  Raise {!Division_by_zero} if the second argument is
    zero. *)

val succ : uint32 -> uint32
(** Successor. *)

val pred : uint32 -> uint32
(** Predecessor. *)

val max_int : uint32
(** The greatest representable integer. *)

val logand : uint32 -> uint32 -> uint32
(** Bitwise logical and. *)

val logor : uint32 -> uint32 -> uint32
(** Bitwise logical or. *)

val logxor : uint32 -> uint32 -> uint32
(** Bitwise logical exclusive or. *)

val lognot : uint32 -> uint32
(** Bitwise logical negation. *)

val shift_left : uint32 -> int -> uint32
(** [Uint32.shift_left x y] shifts [x] to the left by [y] bits.
    The result is unspecified if [y < 0] or [y >= 32]. *)

val shift_right : uint32 -> int -> uint32
(** [Uint32.shift_right x y] shifts [x] to the right by [y] bits.
    The result is unspecified if [y < 0] or [y >= 32]. *)

val of_int : int -> uint32
(** Convert the given int value to an unsigned 32-bit integer. *)

val to_int : uint32 -> int
(** Convert the given unsigned 32-bit integer value to an int. *)

val of_int32 : int32 -> uint32
(** Convert the given int32 value to an unsigned 32-bit integer. *)

val to_int32 : uint32 -> int32
(** Convert the given unsigned 32-bit integer value to an int32. *)

val of_float : float -> uint32
(** Convert the given floating-point number to a 32-bit unsigned integer,
   discarding the fractional part (truncate towards 0).
   The result of the conversion is undefined if, after truncation,
   the number is outside the range \[0, {!Uint32.max_int}\]. *)

val to_float : uint32 -> float
(** Convert the given 32-bit unsigned integer to a floating-point number. *)

val of_string : string -> uint32
(** Convert the given string to an unsigned 32-bit integer.
   The string is read in decimal (by default, or if the string begins
   with [0u]) or in hexadecimal, octal or binary if the string begins
   with [0x], [0o] or [0b] respectively.
   Raise {!Failure} ["Uint32.of_string"] if the given string is not a
   valid representation of an unsigned 32-bit integer. *)

val of_string_opt: string -> uint32 option
(** Same as [of_string], but return [None] instead of raising. *)

val to_string : uint32 -> string
(** Return the string representation of its argument. *)

val compare : uint32 -> uint32 -> int
(** The comparison function for unsigned 32-bit integers, with the same
    specification as {!Pervasives.compare}.  Along with the type [t],
    this function [compare] allows the module [Int32] to be passed as
    argument to the functors {!Set.Make} and {!Map.Make}. *)

val equal : uint32 -> uint32 -> bool
(** The equal function for unsigned 32-bit integers. *)

type t = uint32
