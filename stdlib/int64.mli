(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Int64]: 64-bit integers *)

(* This module provides operations on the type [int64] of
   signed 64-bit integers.  Unlike the built-in [int] type,
   the type [int64] is guaranteed to be exactly 64-bit wide on all
   platforms.  All arithmetic operations over [int64] are taken
   modulo $2^{64}$.  

   The type [int64] is supported on all 64-bit platforms, as well as
   on all 32-bit platforms for which the C compiler supports 64-bit
   arithmetic.  On 32-bit platforms without support for 64-bit arithmetic,
   all functions in this module raise an [Invalid_argument] exception.
*)

val zero: int64
val one: int64
val minus_one: int64
      (* The 64-bit integers 0, 1, -1. *)

external neg: int64 -> int64 = "%int64_neg"
      (* Unary negation. *)
external add: int64 -> int64 -> int64 = "%int64_add"
      (* Addition. *)
external sub: int64 -> int64 -> int64 = "%int64_sub"
      (* Subtraction. *)
external mul: int64 -> int64 -> int64 = "%int64_mul"
      (* Multiplication. *)
external div: int64 -> int64 -> int64 = "%int64_div"
      (* Integer division.  Raise [Division_by_zero] if the second 
         argument is zero. *)
external rem: int64 -> int64 -> int64 = "%int64_mod"
      (* Integer remainder.  If [x >= 0] and [y > 0], the result
           of [Int64.rem x y] satisfies the following properties:
           [0 <= Int64.rem x y < y] and
           [x = Int64.add (Int64.mul (Int64.div x y) y) (Int64.rem x y)].
           If [y = 0], [Int64.rem x y] raises [Division_by_zero].
           If [x < 0] or [y < 0], the result of [Int64.rem x y] is
           not specified and depends on the platform. *)
val succ: int64 -> int64
      (* Successor.  [Int64.succ x] is [Int64.add x 1i]. *)
val pred: int64 -> int64
      (* Predecessor.  [Int64.pred x] is [Int64.sub x 1i]. *)
val abs: int64 -> int64
      (* Return the absolute value of its argument. *)
val max: int64
      (* The greatest representable 64-bit integer, $2^{63} - 1$. *)
val min: int64
      (* The smallest representable 64-bit integer, $-2^{63}$. *)

external logand: int64 -> int64 -> int64 = "%int64_and"
      (* Bitwise logical and. *)
external logor: int64 -> int64 -> int64 = "%int64_or"
      (* Bitwise logical or. *)
external logxor: int64 -> int64 -> int64 = "%int64_xor"
      (* Bitwise logical exclusive or. *)
val lognot: int64 -> int64
      (* Bitwise logical negation *)
external shift_left: int64 -> int -> int64 = "%int64_lsl"
      (* [Int64.shift_left x y] shifts [x] to the left by [y] bits. *)
external shift_right: int64 -> int -> int64 = "%int64_asr"
      (* [Int64.shift_right x y] shifts [x] to the right by [y] bits.
         This is an arithmetic shift: the sign bit of [x] is replicated
         and inserted in the vacated bits. *)
external shift_right_logical: int64 -> int -> int64 = "%int64_lsr"
      (* [Int64.shift_right_logical x y] shifts [x] to the right by [y] bits.
         This is a logical shift: zeroes are inserted in the vacated bits
         regardless of the sign of [x]. *)

external of_int: int -> int64 = "%int64_of_int"
      (* Convert the given integer (type [int]) to a 64-bit integer
         (type [Int64.int64]). *)
external to_int: int64 -> int = "%int64_to_int"
      (* Convert the given 64-bit integer (type [Int64.int64]) to an
         integer (type [int]).  On 64-bit platforms, the 64-bit integer
         is taken modulo $2^{63}$, i.e. the high-order bit is lost
         during the conversion.  On 32-bit platforms, the 64-bit integer
         is taken modulo $2^{31}$, i.e. the top 33 bits are lost
         during the conversion. *)

external of_int32: int32 -> int64 = "%int64_of_int32"
      (* Convert the given 32-bit integer (type [int32])
         to a 64-bit integer (type [int64]). *)
external to_int32: int64 -> int32 = "%int64_to_int32"
      (* Convert the given 64-bit integer (type [int64]) to a
         32-bit integer (type [int32]). The 64-bit integer
         is taken modulo $2^{32}$, i.e. the top 32 bits are lost
         during the conversion.  *)

external of_nativeint: nativeint -> int64 = "%int64_of_nativeint"
      (* Convert the given native integer (type [nativeint])
         to a 64-bit integer (type [int64]). *)
external to_nativeint: int64 -> nativeint = "%int64_to_nativeint"
      (* Convert the given 64-bit integer (type [int64]) to a
         native integer.  On 32-bit platforms, the 64-bit integer
         is taken modulo $2^{32}$.  On 64-bit platforms,
         the conversion is exact. *)

external of_string: string -> int64 = "int64_of_string"
      (* Convert the given string to a 64-bit integer.
         The string is read in decimal (by default) or in hexadecimal,
         octal or binary if the string begins with [0x], [0o] or [0b]
         respectively.
         Raise [Failure "int_of_string"] if the given string is not
         a valid representation of an integer. *)
val to_string: int64 -> string
      (* Return the string representation of its argument, in decimal. *)
external format : string -> int64 -> string = "int64_format"
      (* [Int64.format fmt n] return the string representation of the
         64-bit integer [n] in the format specified by [fmt].
         [fmt] is a [Printf]-style format containing exactly
         one [%d], [%i], [%u], [%x], [%X] or [%o] conversion specification.
         See the documentation of the [Printf] module for more information, *)

