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

(* This module provides the type [Int64.t] of signed 64-bit integers and its
   associated arithmetic operations.  Unlike the built-in [int] type,
   the type [Int64.t] is guaranteed to be exactly 64-bit wide on all
   platforms.  All arithmetic operations over [Int64.t] are taken
   modulo $2^{64}$.  

   The type [Int64.t] is available on all 64-bit platforms, as well as
   on all 32-bit platforms for which the C compiler supports 64-bit
   arithmetic.  On 32-bit platforms without support for 64-bit arithmetic,
   all functions in this module raise an [Invalid_argument] exception.
*)

type t
      (* The type of 64-bit integers. *)

val zero: t
val one: t
val minus_one: t
      (* The 64-bit integers 0, 1, -1. *)

external neg: t -> t = "int64_neg"
      (* Unary negation. *)
external add: t -> t -> t = "int64_add"
      (* Addition. *)
external sub: t -> t -> t = "int64_sub"
      (* Subtraction. *)
external mul: t -> t -> t = "int64_mul"
      (* Multiplication. *)
external div: t -> t -> t = "int64_div"
      (* Integer division.  Raise [Division_by_zero] if the second 
         argument is zero. *)
external rem: t -> t -> t = "int64_mod"
      (* Integer remainder.  If [x >= 0] and [y > 0], the result
           of [Int64.rem x y] satisfies the following properties:
           [0 <= Int64.rem x y < y] and
           [x = Int64.add (Int64.mul (Int64.div x y) y) (Int64.rem x y)].
           If [y = 0], [Int64.rem x y] raises [Division_by_zero].
           If [x < 0] or [y < 0], the result of [Int64.rem x y] is
           not specified and depends on the platform. *)
val succ: t -> t
      (* Successor.  [Int64.succ x] is [Int64.add x 1i]. *)
val pred: t -> t
      (* Predecessor.  [Int64.pred x] is [Int64.sub x 1i]. *)
val abs: t -> t
      (* Return the absolute value of its argument. *)
val max: t
      (* The greatest representable 64-bit integer, $2^{63} - 1$. *)
val min: t
      (* The smallest representable 64-bit integer, $-2^{63}$. *)

external logand: t -> t -> t = "int64_and"
      (* Bitwise logical and. *)
external logor: t -> t -> t = "int64_or"
      (* Bitwise logical or. *)
external logxor: t -> t -> t = "int64_xor"
      (* Bitwise logical exclusive or. *)
val lognot: t -> t
      (* Bitwise logical negation *)
external shift_left: t -> int -> t = "int64_shift_left"
      (* [Int64.shift_left x y] shifts [x] to the left by [y] bits. *)
external shift_right: t -> int -> t = "int64_shift_right"
      (* [Int64.shift_right x y] shifts [x] to the right by [y] bits.
         This is an arithmetic shift: the sign bit of [x] is replicated
         and inserted in the vacated bits. *)
external shift_right_logical: t -> int -> t = "int64_shift_right_unsigned"
      (* [Int64.shift_right_logical x y] shifts [x] to the right by [y] bits.
         This is a logical shift: zeroes are inserted in the vacated bits
         regardless of the sign of [x]. *)

external of_int: int -> t = "int64_of_int"
      (* Convert the given integer (type [int]) to a 64-bit integer
         (type [Int64.t]). *)
external to_int: t -> int = "int64_to_int"
      (* Convert the given 64-bit integer (type [Int64.t]) to an
         integer (type [int]).  On 64-bit platforms, the 64-bit integer
         is taken modulo $2^{63}$, i.e. the high-order bit is lost
         during the conversion.  On 32-bit platforms, the 64-bit integer
         is taken modulo $2^{31}$, i.e. the top 33 bits are lost
         during the conversion. *)

external of_int32: Int32.t -> t = "int64_of_int32"
      (* Convert the given 32-bit integer (type [Int32.t])
         to a 64-bit integer (type [Int64.t]). *)
external to_int32: Int32.t -> int = "int64_to_int32"
      (* Convert the given 64-bit integer (type [Int64.t]) to a
         32-bit integer (type [Int32.t]). The 64-bit integer
         is taken modulo $2^{32}$, i.e. the top 32 bits are lost
         during the conversion.  *)

external of_string: string -> t = "int64_of_string"
      (* Convert the given string to a 64-bit integer.
         The string is read in decimal (by default) or in hexadecimal,
         octal or binary if the string begins with [0x], [0o] or [0b]
         respectively.
         Raise [Failure "int_of_string"] if the given string is not
         a valid representation of an integer. *)
val to_string: t -> string
      (* Return the string representation of its argument, in decimal. *)
external format : string -> t -> string = "int64_format"
      (* [Int64.format fmt n] return the string representation of the
         64-bit integer [n] in the format specified by [fmt].
         [fmt] is a [Printf]-style format containing exactly
         one [%d], [%i], [%u], [%x], [%X] or [%o] conversion specification.
         See the documentation of the [Printf] module for more information, *)

