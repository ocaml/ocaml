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

(* Module [Nativeint]: processor-native integers *)

(* This module provides the type [Nativeint.t] of signed 32-bit integers
   (on 32-bit platforms) or signed 64-bit integers (on 64-bit platforms).
   This integer type has exactly the same width as that of a [long]
   integer type in the C compiler.  All arithmetic operations over
   [Nativeint.t] are taken modulo $2^{32}$ or $2^{64}$ depending
   on the word size of the architecture. *)

type t
      (* The type of native integers. *)

val zero: t
val one: t
val minus_one: t
      (* The native integers 0, 1, -1. *)

external neg: t -> t = "nativeint_neg"
      (* Unary negation. *)
external add: t -> t -> t = "nativeint_add"
      (* Addition. *)
external sub: t -> t -> t = "nativeint_sub"
      (* Subtraction. *)
external mul: t -> t -> t = "nativeint_mul"
      (* Multiplication. *)
external div: t -> t -> t = "nativeint_div"
      (* Integer division.  Raise [Division_by_zero] if the second 
         argument is zero. *)
external rem: t -> t -> t = "nativeint_mod"
      (* Integer remainder.  If [x >= 0] and [y > 0], the result
           of [Nativeint.rem x y] satisfies the following properties:
           [0 <= Nativeint.rem x y < y] and
           [x = Nativeint.add (Nativeint.mul (Nativeint.div x y) y) (Nativeint.rem x y)].
           If [y = 0], [Nativeint.rem x y] raises [Division_by_zero].
           If [x < 0] or [y < 0], the result of [Nativeint.rem x y] is
           not specified and depends on the platform. *)
val succ: t -> t
      (* Successor.  [Nativeint.succ x] is [Nativeint.add x 1n]. *)
val pred: t -> t
      (* Predecessor.  [Nativeint.pred x] is [Nativeint.sub x 1n]. *)
val abs: t -> t
      (* Return the absolute value of its argument. *)
val max: t
      (* The greatest representable native integer,
         either $2^{31} - 1$ on a 32-bit platform,
         or $2^{63} - 1$ on a 64-bit platform. *)
val min: t
      (* The greatest representable native integer,
         either $-2^{31}$ on a 32-bit platform,
         or $-2^{63}$ on a 64-bit platform. *)

external logand: t -> t -> t = "nativeint_and"
      (* Bitwise logical and. *)
external logor: t -> t -> t = "nativeint_or"
      (* Bitwise logical or. *)
external logxor: t -> t -> t = "nativeint_xor"
      (* Bitwise logical exclusive or. *)
val lognot: t -> t
      (* Bitwise logical negation *)
external shift_left: t -> int -> t = "nativeint_shift_left"
      (* [Nativeint.shift_left x y] shifts [x] to the left by [y] bits. *)
external shift_right: t -> int -> t = "nativeint_shift_right"
      (* [Nativeint.shift_right x y] shifts [x] to the right by [y] bits.
         This is an arithmetic shift: the sign bit of [x] is replicated
         and inserted in the vacated bits. *)
external shift_right_logical: t -> int -> t = "nativeint_shift_right_unsigned"
      (* [Nativeint.shift_right_logical x y] shifts [x] to the right
         by [y] bits.
         This is a logical shift: zeroes are inserted in the vacated bits
         regardless of the sign of [x]. *)

external of_int: int -> t = "nativeint_of_int"
      (* Convert the given integer (type [int]) to a native integer
         (type [Nativeint.t]). *)
external to_int: t -> int = "nativeint_to_int"
      (* Convert the given native integer (type [Nativeint.t]) to an
         integer (type [int]).  The high-order bit is lost during
         the conversion. *)

external of_string: string -> t = "nativeint_of_string"
      (* Convert the given string to a native integer.
         The string is read in decimal (by default) or in hexadecimal,
         octal or binary if the string begins with [0x], [0o] or [0b]
         respectively.
         Raise [Failure "int_of_string"] if the given string is not
         a valid representation of an integer. *)
val to_string: t -> string
      (* Return the string representation of its argument, in decimal. *)
external format : string -> t -> string = "nativeint_format"
      (* [Nativeint.format fmt n] return the string representation of the
         native integer [n] in the format specified by [fmt].
         [fmt] is a [Printf]-style format containing exactly
         one [%d], [%i], [%u], [%x], [%X] or [%o] conversion specification.
         See the documentation of the [Printf] module for more information, *)

