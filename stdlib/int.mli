(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Integer values.

    Integers are {!Sys.int_size} bits wide and use two's complement
    representation. All operations are taken modulo
    2{^[Sys.int_size]}. They do not fail on overflow.

    @since 4.08 *)

(** {1:ints Integers} *)

type t = int
(** The type for integer values. *)

val zero : int
(** [zero] is the integer [0]. *)

val one : int
(** [one] is the integer [1]. *)

val minus_one : int
(** [minus_one] is the integer [-1]. *)

external neg : int -> int = "%negint"
(** [neg x] is [~-x]. *)

external add : int -> int -> int = "%addint"
(** [add x y] is the addition [x + y]. *)

external sub : int -> int -> int = "%subint"
(** [sub x y] is the subtraction [x - y]. *)

external mul : int -> int -> int = "%mulint"
(** [mul x y] is the multiplication [x * y]. *)

external div : int -> int -> int = "%divint"
(** Rounding division.
    [div x y] is the real quotient [x / y] rounded towards zero to an integer.
    See {!Stdlib.( / )} for details.

    @raise Division_by_zero if the second argument is 0.
*)

external rem : int -> int -> int = "%modint"
(** [rem x y] is the remainder of the rounding division [div x y].
    We have [rem x y = x - div x y * y].
    See {!Stdlib.( mod )} for details.

    @raise Division_by_zero if the second argument is 0.
*)

val fdiv : int -> int -> int
(** Floor division.
    [fdiv x y] is the real quotient [x / y] rounded down to an integer.
    We have [fdiv x y <= div x y <= cdiv x y] and [cdiv x y - fdiv x y <= 1].

    @raise Division_by_zero if the second argument is 0.
    @since 5.5
*)

val cdiv : int -> int -> int
(** Ceil division.
    [cdiv x y] is the real quotient [x / y] rounded up to an integer.
    We have [fdiv x y <= div x y <= cdiv x y] and [cdiv x y - fdiv x y <= 1].

    @raise Division_by_zero if the second argument is 0.
    @since 5.5
*)

val ediv : int -> int -> int
(** Euclidean division.
    [ediv x y] is the real quotient [x / y] rounded down to an integer
    if [y > 0] and rounded up to an integer if [y < 0].
    The remainder [erem x y = x - ediv x y * y] is always non-negative.
    Moreover, [ediv x (-y)] = [- ediv x y].

    @raise Division_by_zero if the second argument is 0.
    @since 5.5
*)

val erem : int -> int -> int
(** Euclidean remainder.  If [y] is not zero, we have
    [x = ediv x y * y + erem x y] and [0 <= erem x y <= abs y - 1].
    The result of [erem x y] is always non-negative,
    unlike the result of [rem x y], which has the sign of [x].

    @raise Division_by_zero if the second argument is 0.
    @since 5.5
*)

external succ : int -> int = "%succint"
(** [succ x] is [add x 1]. *)

external pred : int -> int = "%predint"
(** [pred x] is [sub x 1]. *)

val abs : int -> int
(** [abs x] is the absolute value of [x]. That is [x] if [x] is positive
    and [neg x] if [x] is negative. {b Warning.} This may be negative if
    the argument is {!min_int}. *)

val max_int : int
(** [max_int] is the greatest representable integer,
    [2]{^[Sys.int_size - 1]}[-1]. *)

val min_int : int
(** [min_int] is the smallest representable integer,
    [-2]{^[Sys.int_size - 1]}. *)

external logand : int -> int -> int = "%andint"
(** [logand x y] is the bitwise logical and of [x] and [y]. *)

external logor : int -> int -> int = "%orint"
(** [logor x y] is the bitwise logical or of [x] and [y]. *)

external logxor : int -> int -> int = "%xorint"
(** [logxor x y] is the bitwise logical exclusive or of [x] and [y]. *)

val lognot : int -> int
(** [lognot x] is the bitwise logical negation of [x]. *)

external shift_left : int -> int -> int = "%lslint"
(** [shift_left x n] shifts [x] to the left by [n] bits. The result
    is unspecified if [n < 0] or [n > ]{!Sys.int_size}. *)

external shift_right : int -> int -> int = "%asrint"
(** [shift_right x n] shifts [x] to the right by [n] bits. This is an
    arithmetic shift: the sign bit of [x] is replicated and inserted
    in the vacated bits. The result is unspecified if [n < 0] or
    [n > ]{!Sys.int_size}. *)

external shift_right_logical : int -> int -> int = "%lsrint"
(** [shift_right_logical x n] shifts [x] to the right by [n] bits.
    This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of [x]. The result is unspecified if [n < 0]
    or [n > ]{!Sys.int_size}. *)

(** {1:preds Predicates and comparisons} *)

val equal : int -> int -> bool
(** [equal x y] is [true] if and only if [x = y]. *)

val compare : int -> int -> int
(** [compare x y] is {!Stdlib.compare}[ x y] but more efficient. *)

val min : int -> int -> int
(** Return the smaller of the two arguments.
    @since 4.13
*)

val max : int -> int -> int
(** Return the greater of the two arguments.
    @since 4.13
 *)

(** {1:bitcounting Bit counting} *)

val popcount: t -> int
(** Population count, also known as Hamming weight.
    [popcount n] is the number of 1 bits in the binary representation of [n].
    Negative [n] are represented in two's complement.

    @since 5.5 *)

val unsigned_bitsize: t -> int
(** [unsigned_bitsize n] is the minimal number of bits needed to represent
    [n] as an unsigned binary number.  It is the smallest integer [i]
    between 0 and {!Sys.int_size} inclusive such that
    [0 <= n < 2{^i}] (unsigned).

    @since 5.5 *)

val signed_bitsize: t -> int
(** [signed_bitsize n] is the minimal number of bits needed to represent
    [n] as a signed, two's complement binary number.
    It is the smallest integer [i] between 1 and {!Sys.int_size} inclusive
    such that [-2{^i-1} <= n < 2{^i-1}] (signed).

    @since 5.5 *)

val leading_zeros: t -> int
(** [leading_zeros n] is the number of leading (most significant) 0 bits in
    the binary representation of [n].
    It is an integer between 0 and {!Sys.int_size} inclusive.
    If [n] is negative, [leading_zeros n = 0] since the most significant
    bit of [n] is 1.
    [leading_zeros n = {!Sys.int_size}] if and only if [n = zero].
    Note that [leading_zeros n + unsigned_bitsize n = {!Sys.int_size}].

    @since 5.5 *)

val leading_sign_bits: t -> int
(** [leading_sign_bits n] is the number of leading (most significant)
    sign bits in the binary representation of [n],
    excluding the sign bit itself.
    It is an integer between 0 and [{!Sys.int_size} - 1] inclusive.
    For positive [n], it is the number of leading zero bits minus one.
    For negative [n], it is the number of leading one bits minus one.
    Note that [leading_sign_bits n + signed_bitsize n = {!Sys.int_size}].

    @since 5.5 *)

val trailing_zeros: t -> int
(** [trailing_zeros n] is the number of trailing (least significant) 0 bits in
    the binary representation of [n].
    It is an integer between 0 and {!Sys.int_size} inclusive.
    It is the largest integer [i <= {!Sys.int_size}]
    such that [2{^i}] divides [n] evenly.
    For example, [trailing_zeros n = 0] if and only if [n] is odd,
    and [trailing_zeros n = {!Sys.int_size}] if and only if [n = zero].

    @since 5.5 *)

(** {1:convert Converting} *)

external to_float : int -> float = "%floatofint"
(** [to_float x] is [x] as a floating point number. *)

external of_float : float -> int = "%intoffloat"
(** [of_float x] truncates [x] to an integer. The result is
    unspecified if the argument is [nan] or falls outside the range of
    representable integers. *)

(*
val of_string : string -> int option
(** [of_string s] is [Some s] if [s] can be parsed to an integer
    in the range representable by the type [int] (note that this
    depends on {!Sys.int_size}) and [None] otherwise.

    The string may start with an optional ['-'] or ['+'] sign, and may
    be followed by an optional prefix that specifies the base in which
    the number is expressed. If there is not prefix or if the prefix
    is [0u] or [0U] it is expressed in decimal. If the prefix is [0x]
    or [0X] it is expressed in hexadecimal. If the prefix is [0o] or
    [0O] it is expressed in octal. If the prefix is [0b] or [0B] it is
    expressed in binary.

    When the [0u] or [0U] prefix is used, the represented number may
    exceed {!max_int} or {!min_int} in which case it wraps around
    modulo 2{^[Sys.int_size]} like arithmetic operations do.

    The ['_'] (underscore) character can appear anywhere between two
    digits of the number. *)
*)

val to_string : int -> string
(** [to_string x] is the written representation of [x] in decimal. *)

val seeded_hash : int -> int -> int
(** A seeded hash function for ints, with the same output value as
    {!Hashtbl.seeded_hash}. This function allows this module to be passed as
    argument to the functor {!Hashtbl.MakeSeeded}.

    @since 5.1 *)

val hash : int -> int
(** An unseeded hash function for ints, with the same output value as
    {!Hashtbl.hash}. This function allows this module to be passed as argument
    to the functor {!Hashtbl.Make}.

    @since 5.1 *)
