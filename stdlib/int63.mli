(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Mitchell Plamann, Jane Street Group, LLC               *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group, LLC                                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = int63

val zero : int63
(** The 63-bit integer 0. *)

val one : int63
(** The 63-bit integer 1. *)

val minus_one : int63
(** The 63-bit integer -1. *)

external neg : int63 -> int63 = "%int63_neg"
(** Unary negation. *)

external add : int63 -> int63 -> int63 = "%int63_add"
(** Addition. *)

external sub : int63 -> int63 -> int63 = "%int63_sub"
(** Subtraction. *)

external mul : int63 -> int63 -> int63 = "%int63_mul"
(** Multiplication. *)

external div : int63 -> int63 -> int63 = "%int63_div"
(** Integer division.  Raise [Division_by_zero] if the second
   argument is zero.  This division rounds the real quotient of
   its arguments towards zero, as specified for {!Pervasives.(/)}. *)

external rem : int63 -> int63 -> int63 = "%int63_mod"
(** Integer remainder.  If [y] is not zero, the result
   of [Int63.rem x y] satisfies the following property:
   [x = Int63.add (Int63.mul (Int63.div x y) y) (Int63.rem x y)].
   If [y = 0], [Int63.rem x y] raises [Division_by_zero]. *)

val succ : int63 -> int63
(** Successor.  [Int63.succ x] is [Int63.add x Int63.one]. *)

val pred : int63 -> int63
(** Predecessor.  [Int63.pred x] is [Int63.sub x Int63.one]. *)

val abs : int63 -> int63
(** Return the absolute value of its argument. *)

val max_int : int63
(** The greatest representable 63-bit integer, 2{^62} - 1. *)

val min_int : int63
(** The smallest representable 63-bit integer, -2{^62}. *)

external logand : int63 -> int63 -> int63 = "%int63_and"
(** Bitwise logical and. *)

external logor : int63 -> int63 -> int63 = "%int63_or"
(** Bitwise logical or. *)

external logxor : int63 -> int63 -> int63 = "%int63_xor"
(** Bitwise logical exclusive or. *)

val lognot : int63 -> int63
(** Bitwise logical negation *)

external shift_left : int63 -> int -> int63 = "%int63_lsl"
(** [Int63.shift_left x y] shifts [x] to the left by [y] bits.
   The result is unspecified if [y < 0] or [y >= 63]. *)

external shift_right : int63 -> int -> int63 = "%int63_asr"
(** [Int63.shift_right x y] shifts [x] to the right by [y] bits.
   This is an arithmetic shift: the sign bit of [x] is replicated
   and inserted in the vacated bits.
   The result is unspecified if [y < 0] or [y >= 63]. *)

external shift_right_logical : int63 -> int -> int63 = "%int63_lsr"
(** [Int63.shift_right_logical x y] shifts [x] to the right by [y] bits.
   This is a logical shift: zeroes are inserted in the vacated bits
   regardless of the sign of [x].
   The result is unspecified if [y < 0] or [y >= 63]. *)

external of_int : int -> int63 = "%int63_of_int"
(** Convert the given integer (type [int]) to a 63-bit integer
    (type [int63]). *)

external to_int : int63 -> int = "%int63_to_int"
(** Convert the given 63-bit integer (type [Int63.t]) to an
    integer (type [int]).  On 64-bit platforms, [int] is 63-bits,
    so no bits are lost. On 32-bit platforms, the 63-bit integer
    is taken modulo 2{^31}, i.e. the top 32 bits are lost
    during the conversion. *)

external of_int32 : int32 -> int63 = "%int63_of_int32"
(** Convert the given 32-bit integer (type [int32])
   to a 63-bit integer (type [Int63.t]). *)

external to_int32 : int63 -> int32 = "%int63_to_int32"
(** Convert the given 63-bit integer (type [Int63.t]) to a
   32-bit integer (type [int32]). The 63-bit integer
   is taken modulo 2{^32}, i.e. the top 31 bits are lost
   during the conversion.  *)

external of_int64 : int64 -> int63 = "%int63_of_int64"
(** Convert the given 64-bit intger (type [int64]) to a
   63-bit integer (type [Int63.t]). The 64-bit integer is
   taken modulo 2{^63}, i.e. the top bit is lost during
   the conversion. *)

external to_int64 : int63 -> int64 = "%int63_to_int64"
(** Convert the given 63-bit integer (type [Int63.t]) to
   a 64-bit integer (type [int64]). The conversion is exact. *)

external of_nativeint : nativeint -> int63 = "%int63_of_nativeint"
(** Convert the given native integer (type [nativeint])
   to a 63-bit integer (type [Int63.t]). *)

external to_nativeint : int63 -> nativeint = "%int63_to_nativeint"
(** Convert the given 63-bit integer (type [Int63.t]) to a
   native integer.  On 32-bit platforms, the 63-bit integer
   is taken modulo 2{^32}.  On 64-bit platforms,
   the conversion is exact. *)

external of_string : string -> int63 = "caml_int63_of_string"
(** Convert the given string to a 63-bit integer.
   The string is read in decimal (by default) or in hexadecimal,
   octal or binary if the string begins with [0x], [0o] or [0b]
   respectively.
   Raise [Failure "int_of_string"] if the given string is not
   a valid representation of an integer, or if the integer represented
   exceeds the range of integers representable in type [Int63.t]. *)

val to_string : int63 -> string
(** Return the string representation of its argument, in decimal. *)

val compare: int63 -> int63 -> int
(** The comparison function for 63-bit integers, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Int63] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal: int63 -> int63 -> bool
(** The equal function for int63s. *)

