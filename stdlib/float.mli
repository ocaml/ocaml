(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {1 Floating-point arithmetic}

    OCaml's floating-point numbers follow the
    IEEE 754 standard, using double precision (64 bits) numbers.
    Floating-point operations never raise an exception on overflow,
    underflow, division by zero, etc.  Instead, special IEEE numbers
    are returned as appropriate, such as [infinity] for [1.0 /. 0.0],
    [neg_infinity] for [-1.0 /. 0.0], and [nan] ('not a number')
    for [0.0 /. 0.0].  These special numbers then propagate through
    floating-point computations as expected: for instance,
    [1.0 /. infinity] is [0.0], and any arithmetic operation with [nan]
    as argument returns [nan] as result.

    @since 4.07.0
*)

external neg : float -> float = "%negfloat"
(** Unary negation. *)

external add : float -> float -> float = "%addfloat"
(** Floating-point addition. *)

external sub : float -> float -> float = "%subfloat"
(** Floating-point subtraction. *)

external mul : float -> float -> float = "%mulfloat"
(** Floating-point multiplication. *)

external div : float -> float -> float = "%divfloat"
(** Floating-point division. *)

external rem : float -> float -> float = "caml_fmod_float" "fmod"
[@@unboxed] [@@noalloc]
(** [rem a b] returns the remainder of [a] with respect to [b].  The returned
    value is [a -. n *. b], where [n] is the quotient [a /. b] rounded towards
    zero to an integer. *)

external abs : float -> float = "%absfloat"
(** [abs f] returns the absolute value of [f]. *)

val infinity : float
(** Positive infinity. *)

val neg_infinity : float
(** Negative infinity. *)

val nan : float
(** A special floating-point value denoting the result of an
    undefined operation such as [0.0 /. 0.0].  Stands for
    'not a number'.  Any floating-point operation with [nan] as
    argument returns [nan] as result.  As for floating-point comparisons,
    [=], [<], [<=], [>] and [>=] return [false] and [<>] returns [true]
    if one or both of their arguments is [nan]. *)

val pi : float
(** The constant pi. *)

val max_float : float
(** The largest positive finite value of type [float]. *)

val min_float : float
(** The smallest positive, non-zero, non-denormalized value of type [float]. *)

val epsilon : float
(** The difference between [1.0] and the smallest exactly representable
    floating-point number greater than [1.0]. *)

external of_int : int -> float = "%floatofint"
(** Convert an integer to floating-point. *)

external to_int : float -> int = "%intoffloat"
(** Truncate the given floating-point number to an integer.
    The result is unspecified if the argument is [nan] or falls outside the
    range of representable integers. *)

external of_string : string -> float = "caml_float_of_string"
(** Convert the given string to a float.  The string is read in decimal
    (by default) or in hexadecimal (marked by [0x] or [0X]).
    The format of decimal floating-point numbers is
    [ [-] dd.ddd (e|E) [+|-] dd ], where [d] stands for a decimal digit.
    The format of hexadecimal floating-point numbers is
    [ [-] 0(x|X) hh.hhh (p|P) [+|-] dd ], where [h] stands for an
    hexadecimal digit and [d] for a decimal digit.
    In both cases, at least one of the integer and fractional parts must be
    given; the exponent part is optional.
    The [_] (underscore) character can appear anywhere in the string
    and is ignored.
    Depending on the execution platforms, other representations of
    floating-point numbers can be accepted, but should not be relied upon.
    Raise [Failure "float_of_string"] if the given string is not a valid
    representation of a float. *)

val of_string_opt: string -> float option
(** Same as [of_string], but returns [None] instead of raising. *)

val to_string : float -> string
(** Return the string representation of a floating-point number. *)

type fpclass = Pervasives.fpclass =
    FP_normal           (** Normal number, none of the below *)
  | FP_subnormal        (** Number very close to 0.0, has reduced precision *)
  | FP_zero             (** Number is 0.0 or -0.0 *)
  | FP_infinite         (** Number is positive or negative infinity *)
  | FP_nan              (** Not a number: result of an undefined operation *)
(** The five classes of floating-point numbers, as determined by
    the {!classify_float} function. *)

external classify_float : (float [@unboxed]) -> fpclass =
  "caml_classify_float" "caml_classify_float_unboxed" [@@noalloc]
(** Return the class of the given floating-point number:
    normal, subnormal, zero, infinite, or not a number. *)

external pow : float -> float -> float = "caml_power_float" "pow"
[@@unboxed] [@@noalloc]
(** Exponentiation. *)

external sqrt : float -> float = "caml_sqrt_float" "sqrt"
[@@unboxed] [@@noalloc]
(** Square root. *)

external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
(** Exponential. *)

external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]
(** Natural logarithm. *)

external log10 : float -> float = "caml_log10_float" "log10"
[@@unboxed] [@@noalloc]
(** Base 10 logarithm. *)

external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
[@@unboxed] [@@noalloc]
(** [expm1 x] computes [exp x -. 1.0], giving numerically-accurate results
    even if [x] is close to [0.0]. *)

external log1p : float -> float = "caml_log1p_float" "caml_log1p"
[@@unboxed] [@@noalloc]
(** [log1p x] computes [log(1.0 +. x)] (natural logarithm),
    giving numerically-accurate results even if [x] is close to [0.0]. *)

external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]
(** Cosine.  Argument is in radians. *)

external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]
(** Sine.  Argument is in radians. *)

external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]
(** Tangent.  Argument is in radians. *)

external acos : float -> float = "caml_acos_float" "acos"
[@@unboxed] [@@noalloc]
(** Arc cosine.  The argument must fall within the range [[-1.0, 1.0]].
    Result is in radians and is between [0.0] and [pi]. *)

external asin : float -> float = "caml_asin_float" "asin"
[@@unboxed] [@@noalloc]
(** Arc sine.  The argument must fall within the range [[-1.0, 1.0]].
    Result is in radians and is between [-pi/2] and [pi/2]. *)

external atan : float -> float = "caml_atan_float" "atan"
[@@unboxed] [@@noalloc]
(** Arc tangent.
    Result is in radians and is between [-pi/2] and [pi/2]. *)

external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
[@@unboxed] [@@noalloc]
(** [atan2 y x] returns the arc tangent of [y /. x].  The signs of [x]
    and [y] are used to determine the quadrant of the result.
    Result is in radians and is between [-pi] and [pi]. *)

external hypot : float -> float -> float = "caml_hypot_float" "caml_hypot"
[@@unboxed] [@@noalloc]
(** [hypot x y] returns [sqrt(x *. x + y *. y)], that is, the length
    of the hypotenuse of a right-angled triangle with sides of length
    [x] and [y], or, equivalently, the distance of the point [(x,y)]
    to origin.  If one of [x] or [y] is infinite, returns [infinity]
    even if the other is [nan]. *)

external cosh : float -> float = "caml_cosh_float" "cosh"
[@@unboxed] [@@noalloc]
(** Hyperbolic cosine.  Argument is in radians. *)

external sinh : float -> float = "caml_sinh_float" "sinh"
[@@unboxed] [@@noalloc]
(** Hyperbolic sine.  Argument is in radians. *)

external tanh : float -> float = "caml_tanh_float" "tanh"
[@@unboxed] [@@noalloc]
(** Hyperbolic tangent.  Argument is in radians. *)

external ceil : float -> float = "caml_ceil_float" "ceil"
[@@unboxed] [@@noalloc]
(** Round above to an integer value.
    [ceil f] returns the least integer value greater than or equal to [f].
    The result is returned as a float. *)

external floor : float -> float = "caml_floor_float" "floor"
[@@unboxed] [@@noalloc]
(** Round below to an integer value.
    [floor f] returns the greatest integer value less than or
    equal to [f].
    The result is returned as a float. *)

external copysign : float -> float -> float
  = "caml_copysign_float" "caml_copysign"
[@@unboxed] [@@noalloc]
(** [copysign x y] returns a float whose absolute value is that of [x]
    and whose sign is that of [y].  If [x] is [nan], returns [nan].
    If [y] is [nan], returns either [x] or [-. x], but it is not
    specified which. *)

external frexp : float -> float * int = "caml_frexp_float"
(** [frexp f] returns the pair of the significant
    and the exponent of [f].  When [f] is zero, the
    significant [x] and the exponent [n] of [f] are equal to
    zero.  When [f] is non-zero, they are defined by
    [f = x *. 2 ** n] and [0.5 <= x < 1.0]. *)

external ldexp : (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed]) =
  "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
(** [ldexp x n] returns [x *. 2 ** n]. *)

external modf : float -> float * float = "caml_modf_float"
(** [modf f] returns the pair of the fractional and integral
    part of [f]. *)

type t = float
(** An alias for the type of floating-point numbers. *)

val compare: t -> t -> int
(** [compare x y] returns [0] if [x] is equal to [y], a negative integer if [x]
    is less than [y], and a positive integer if [x] is greater than
    [y]. [compare] treats [nan] as equal to itself and less than any other float
    value.  This treatment of [nan] ensures that [compare] defines a total
    ordering relation.  *)

val equal: t -> t -> bool
(** The equal function for floating-point numbers, compared using {!compare}. *)

val hash: t -> int
(** The hash function for floating-point numbers. *)

module Array : sig
  type t = floatarray
  external create : int -> t = "caml_floatarray_create"
  external length : t -> int = "%floatarray_length"
  external get : t -> int -> float = "%floatarray_safe_get"
  external set : t -> int -> float -> unit = "%floatarray_safe_set"
  external unsafe_get : t -> int -> float = "%floatarray_unsafe_get"
  external unsafe_set : t -> int -> float -> unit = "%floatarray_unsafe_set"
end
