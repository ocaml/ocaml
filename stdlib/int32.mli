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

(* Module [Int32]: 32-bit integers *)

(* This module provides the type [Int32.t] of signed 32-bit integers and its
   associated arithmetic operations.  Unlike the built-in [int] type,
   the type [Int32.t] is guaranteed to be exactly 32-bit wide on all
   platforms.  All arithmetic operations over [Int32.t] are taken
   modulo $2^{32}$. *)

type t
      (* The type of 32-bit integers. *)

val zero: t
val one: t
val minus_one: t
      (* The 32-bit integers 0, 1, -1. *)

external neg: t -> t = "int32_neg"
      (* Unary negation. *)
external add: t -> t -> t = "int32_add"
      (* Addition. *)
external sub: t -> t -> t = "int32_sub"
      (* Subtraction. *)
external mul: t -> t -> t = "int32_mul"
      (* Multiplication. *)
external div: t -> t -> t = "int32_div"
      (* Integer division.  Raise [Division_by_zero] if the second 
         argument is zero. *)
external rem: t -> t -> t = "int32_mod"
      (* Integer remainder.  If [x >= 0] and [y > 0], the result
           of [Int32.rem x y] satisfies the following properties:
           [0 <= Int32.rem x y < y] and
           [x = Int32.add (Int32.mul (Int32.div x y) y) (Int32.rem x y)].
           If [y = 0], [Int32.rem x y] raises [Division_by_zero].
           If [x < 0] or [y < 0], the result of [Int32.rem x y] is
           not specified and depends on the platform. *)
val succ: t -> t
      (* Successor.  [Int32.succ x] is [Int32.add x 1i]. *)
val pred: t -> t
      (* Predecessor.  [Int32.pred x] is [Int32.sub x 1i]. *)
val abs: t -> t
      (* Return the absolute value of its argument. *)
val max: t
      (* The greatest representable 32-bit integer, $2^{31} - 1$. *)
val min: t
      (* The smallest representable 32-bit integer, $-2^{31}$. *)

external logand: t -> t -> t = "int32_and"
      (* Bitwise logical and. *)
external logor: t -> t -> t = "int32_or"
      (* Bitwise logical or. *)
external logxor: t -> t -> t = "int32_xor"
      (* Bitwise logical exclusive or. *)
val lognot: t -> t
      (* Bitwise logical negation *)
external shift_left: t -> int -> t = "int32_shift_left"
      (* [Int32.shift_left x y] shifts [x] to the left by [y] bits. *)
external shift_right: t -> int -> t = "int32_shift_right"
      (* [Int32.shift_right x y] shifts [x] to the right by [y] bits.
         This is an arithmetic shift: the sign bit of [x] is replicated
         and inserted in the vacated bits. *)
external shift_right_logical: t -> int -> t = "int32_shift_right_unsigned"
      (* [Int32.shift_right_logical x y] shifts [x] to the right by [y] bits.
         This is a logical shift: zeroes are inserted in the vacated bits
         regardless of the sign of [x]. *)

external of_int: int -> t = "int32_of_int"
      (* Convert the given integer (type [int]) to a 32-bit integer
         (type [Int32.t]). *)
external to_int: t -> int = "int32_to_int"
      (* Convert the given 32-bit integer (type [Int32.t]) to an
         integer (type [int]).  On 32-bit platforms, the 32-bit integer
         is taken modulo $2^{31}$, i.e. the high-order bit is lost
         during the conversion.  On 64-bit platforms, the conversion
         is exact. *)

external of_string: string -> t = "int32_of_string"
      (* Convert the given string to a 32-bit integer.
         The string is read in decimal (by default) or in hexadecimal,
         octal or binary if the string begins with [0x], [0o] or [0b]
         respectively.
         Raise [Failure "int_of_string"] if the given string is not
         a valid representation of an integer. *)
val to_string: t -> string
      (* Return the string representation of its argument,
         in signed decimal. *)
external format : string -> t -> string = "int32_format"
      (* [Int32.format fmt n] return the string representation of the
         32-bit integer [n] in the format specified by [fmt].
         [fmt] is a [Printf]-style format containing exactly
         one [%d], [%i], [%u], [%x], [%X] or [%o] conversion specification.
         See the documentation of the [Printf] module for more information, *)
