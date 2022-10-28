(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Processor-native integers.

   This module provides operations on the type [nativeint] of
   signed 32-bit integers (on 32-bit platforms) or
   signed 64-bit integers (on 64-bit platforms).
   This integer type has exactly the same width as that of a
   pointer type in the C compiler.  All arithmetic operations over
   [nativeint] are taken modulo 2{^32} or 2{^64} depending
   on the word size of the architecture.

   Performance notice: values of type [nativeint] occupy more memory
   space than values of type [int], and arithmetic operations on
   [nativeint] are generally slower than those on [int].  Use [nativeint]
   only when the application requires the extra bit of precision
   over the [int] type.

    Literals for native integers are suffixed by n:
    {[
     let zero: nativeint = 0n
     let one: nativeint = 1n
     let m_one: nativeint = -1n
    ]}
*)

val zero : nativeint
(** The native integer 0.*)

val one : nativeint
(** The native integer 1.*)

val minus_one : nativeint
(** The native integer -1.*)

external neg : nativeint -> nativeint = "%nativeint_neg"
(** Unary negation. *)

external add : nativeint -> nativeint -> nativeint = "%nativeint_add"
(** Addition. *)

external sub : nativeint -> nativeint -> nativeint = "%nativeint_sub"
(** Subtraction. *)

external mul : nativeint -> nativeint -> nativeint = "%nativeint_mul"
(** Multiplication. *)

external div : nativeint -> nativeint -> nativeint = "%nativeint_div"
(** Integer division. This division rounds the real quotient of
   its arguments towards zero, as specified for {!Stdlib.(/)}.

   @raise Division_by_zero if the second
   argument is zero. *)

val unsigned_div : nativeint -> nativeint -> nativeint
(** Same as {!div}, except that arguments and result are interpreted as {e
    unsigned} native integers.

    @since 4.08.0 *)

external rem : nativeint -> nativeint -> nativeint = "%nativeint_mod"
(** Integer remainder.  If [y] is not zero, the result
   of [Nativeint.rem x y] satisfies the following properties:
   [Nativeint.zero <= Nativeint.rem x y < Nativeint.abs y] and
   [x = Nativeint.add (Nativeint.mul (Nativeint.div x y) y)
                      (Nativeint.rem x y)].
   If [y = 0], [Nativeint.rem x y] raises [Division_by_zero]. *)

val unsigned_rem : nativeint -> nativeint -> nativeint
(** Same as {!rem}, except that arguments and result are interpreted as {e
    unsigned} native integers.

    @since 4.08.0 *)

val succ : nativeint -> nativeint
(** Successor.
   [Nativeint.succ x] is [Nativeint.add x Nativeint.one]. *)

val pred : nativeint -> nativeint
(** Predecessor.
   [Nativeint.pred x] is [Nativeint.sub x Nativeint.one]. *)

val abs : nativeint -> nativeint
(** [abs x] is the absolute value of [x]. On [min_int] this
   is [min_int] itself and thus remains negative. *)

val size : int
(** The size in bits of a native integer.  This is equal to [32]
   on a 32-bit platform and to [64] on a 64-bit platform. *)

val max_int : nativeint
(** The greatest representable native integer,
   either 2{^31} - 1 on a 32-bit platform,
   or 2{^63} - 1 on a 64-bit platform. *)

val min_int : nativeint
(** The smallest representable native integer,
   either -2{^31} on a 32-bit platform,
   or -2{^63} on a 64-bit platform. *)

external logand : nativeint -> nativeint -> nativeint = "%nativeint_and"
(** Bitwise logical and. *)

external logor : nativeint -> nativeint -> nativeint = "%nativeint_or"
(** Bitwise logical or. *)

external logxor : nativeint -> nativeint -> nativeint = "%nativeint_xor"
(** Bitwise logical exclusive or. *)

val lognot : nativeint -> nativeint
(** Bitwise logical negation. *)

external shift_left : nativeint -> int -> nativeint = "%nativeint_lsl"
(** [Nativeint.shift_left x y] shifts [x] to the left by [y] bits.
   The result is unspecified if [y < 0] or [y >= bitsize],
   where [bitsize] is [32] on a 32-bit platform and
   [64] on a 64-bit platform. *)

external shift_right : nativeint -> int -> nativeint = "%nativeint_asr"
(** [Nativeint.shift_right x y] shifts [x] to the right by [y] bits.
   This is an arithmetic shift: the sign bit of [x] is replicated
   and inserted in the vacated bits.
   The result is unspecified if [y < 0] or [y >= bitsize]. *)

external shift_right_logical :
  nativeint -> int -> nativeint = "%nativeint_lsr"
(** [Nativeint.shift_right_logical x y] shifts [x] to the right
   by [y] bits.
   This is a logical shift: zeroes are inserted in the vacated bits
   regardless of the sign of [x].
   The result is unspecified if [y < 0] or [y >= bitsize]. *)


external of_int : int -> nativeint = "%nativeint_of_int"
(** Convert the given integer (type [int]) to a native integer
   (type [nativeint]). *)

external to_int : nativeint -> int = "%nativeint_to_int"
(** Convert the given native integer (type [nativeint]) to an
   integer (type [int]).  The high-order bit is lost during
   the conversion. *)

val unsigned_to_int : nativeint -> int option
(** Same as {!to_int}, but interprets the argument as an {e unsigned} integer.
    Returns [None] if the unsigned value of the argument cannot fit into an
    [int].

    @since 4.08.0 *)

external of_float : float -> nativeint
  = "caml_nativeint_of_float" "caml_nativeint_of_float_unboxed"
  [@@unboxed] [@@noalloc]
(** Convert the given floating-point number to a native integer,
   discarding the fractional part (truncate towards 0).
   If the truncated floating-point number is outside the range
   \[{!Nativeint.min_int}, {!Nativeint.max_int}\], no exception is raised,
   and an unspecified, platform-dependent integer is returned. *)

external to_float : nativeint -> float
  = "caml_nativeint_to_float" "caml_nativeint_to_float_unboxed"
  [@@unboxed] [@@noalloc]
(** Convert the given native integer to a floating-point number. *)

external of_int32 : int32 -> nativeint = "%nativeint_of_int32"
(** Convert the given 32-bit integer (type [int32])
   to a native integer. *)

external to_int32 : nativeint -> int32 = "%nativeint_to_int32"
(** Convert the given native integer to a
   32-bit integer (type [int32]).  On 64-bit platforms,
   the 64-bit native integer is taken modulo 2{^32},
   i.e. the top 32 bits are lost.  On 32-bit platforms,
   the conversion is exact. *)

external of_string : string -> nativeint = "caml_nativeint_of_string"
(** Convert the given string to a native integer.
   The string is read in decimal (by default, or if the string
   begins with [0u]) or in hexadecimal, octal or binary if the
   string begins with [0x], [0o] or [0b] respectively.

   The [0u] prefix reads the input as an unsigned integer in the range
   [[0, 2*Nativeint.max_int+1]].  If the input exceeds {!Nativeint.max_int}
   it is converted to the signed integer
   [Int64.min_int + input - Nativeint.max_int - 1].

   @raise Failure if the given string is not
   a valid representation of an integer, or if the integer represented
   exceeds the range of integers representable in type [nativeint]. *)

val of_string_opt: string -> nativeint option
(** Same as [of_string], but return [None] instead of raising.
    @since 4.05 *)

val to_string : nativeint -> string
(** Return the string representation of its argument, in decimal. *)

type t = nativeint
(** An alias for the type of native integers. *)

val compare: t -> t -> int
(** The comparison function for native integers, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [Nativeint] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val unsigned_compare: t -> t -> int
(** Same as {!compare}, except that arguments are interpreted as {e unsigned}
    native integers.

    @since 4.08.0 *)

val equal: t -> t -> bool
(** The equal function for native ints.
    @since 4.03.0 *)

val min: t -> t -> t
(** Return the smaller of the two arguments.
    @since 4.13.0
*)

val max: t -> t -> t
(** Return the greater of the two arguments.
    @since 4.13.0
 *)

val seeded_hash : int -> t -> int
(** A seeded hash function for native ints, with the same output value as
    {!Hashtbl.seeded_hash}. This function allows this module to be passed as
    argument to the functor {!Hashtbl.MakeSeeded}.

    @since 5.1.0 *)

val hash : t -> int
(** An unseeded hash function for native ints, with the same output value as
    {!Hashtbl.hash}. This function allows this module to be passed as argument
    to the functor {!Hashtbl.Make}.

    @since 5.1.0 *)
