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

(* NOTE:
   If this file is float.template.mli, run tools/sync_stdlib_docs after editing
   it to generate float.mli.

   If this file is float.mli, do not edit it directly -- edit
   templates/float.template.mli instead.
 *)

(** Floating-point arithmetic.

    OCaml's floating-point numbers follow the
    IEEE 754 standard, using double precision (64 bits) numbers.
    Floating-point operations never raise an exception on overflow,
    underflow, division by zero, etc.  Instead, special IEEE numbers
    are returned as appropriate, such as [infinity] for [1.0 /. 0.0],
    [neg_infinity] for [-1.0 /. 0.0], and [nan] ('not a number')
    for [0.0 /. 0.0].  These special numbers then propagate through
    floating-point computations as expected: for instance,
    [1.0 /. infinity] is [0.0], basic arithmetic operations
    ([+.], [-.], [*.], [/.]) with [nan] as an argument return [nan], ...

    @since 4.07.0
*)

val zero : float
(** The floating point 0.
   @since 4.08.0 *)

val one : float
(** The floating-point 1.
   @since 4.08.0 *)

val minus_one : float
(** The floating-point -1.
   @since 4.08.0 *)

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

external fma : float -> float -> float -> float =
  "caml_fma_float" "caml_fma" [@@unboxed] [@@noalloc]
(** [fma x y z] returns [x * y + z], with a best effort for computing
   this expression with a single rounding, using either hardware
   instructions (providing full IEEE compliance) or a software
   emulation.

   On 64-bit Cygwin, 64-bit mingw-w64 and MSVC 2017 and earlier, this function
   may be emulated owing to known bugs on limitations on these platforms.
   Note: since software emulation of the fma is costly, make sure that you are
   using hardware fma support if performance matters.

   @since 4.08.0 *)

external rem : float -> float -> float = "caml_fmod_float" "fmod"
[@@unboxed] [@@noalloc]
(** [rem a b] returns the remainder of [a] with respect to [b].  The returned
    value is [a -. n *. b], where [n] is the quotient [a /. b] rounded towards
    zero to an integer. *)

val succ : float -> float
(** [succ x] returns the floating point number right after [x] i.e.,
   the smallest floating-point number greater than [x].  See also
   {!next_after}.
   @since 4.08.0 *)

val pred : float -> float
(** [pred x] returns the floating-point number right before [x] i.e.,
   the greatest floating-point number smaller than [x].  See also
   {!next_after}.
   @since 4.08.0 *)

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
    argument returns [nan] as result, unless otherwise specified in
    IEEE 754 standard.  As for floating-point comparisons,
    [=], [<], [<=], [>] and [>=] return [false] and [<>] returns [true]
    if one or both of their arguments is [nan].

    [nan] is [quiet_nan] since 5.1; it was a signaling NaN before. *)

val signaling_nan : float
(** Signaling NaN. The corresponding signals do not raise OCaml exception,
    but the value can be useful for interoperability with C libraries.

    @since 5.1 *)

val quiet_nan : float
(** Quiet NaN.

    @since 5.1 *)

val pi : float
(** The constant pi. *)

val max_float : float
(** The largest positive finite value of type [float]. *)

val min_float : float
(** The smallest positive, non-zero, non-denormalized value of type [float]. *)

val epsilon : float
(** The difference between [1.0] and the smallest exactly representable
    floating-point number greater than [1.0]. *)

val is_finite : float -> bool
(** [is_finite x] is [true] if and only if [x] is finite i.e., not infinite and
   not {!nan}.

   @since 4.08.0 *)

val is_infinite : float -> bool
(** [is_infinite x] is [true] if and only if [x] is {!infinity} or
    {!neg_infinity}.

   @since 4.08.0 *)

val is_nan : float -> bool
(** [is_nan x] is [true] if and only if [x] is not a number (see {!nan}).

   @since 4.08.0 *)

val is_integer : float -> bool
(** [is_integer x] is [true] if and only if [x] is an integer.

   @since 4.08.0 *)

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
    @raise Failure if the given string is not a valid
    representation of a float. *)

val of_string_opt: string -> float option
(** Same as [of_string], but returns [None] instead of raising. *)

val to_string : float -> string
(** Return a string representation of a floating-point number.

    This conversion can involve a loss of precision. For greater control over
    the manner in which the number is printed, see {!Printf}.

    This function is an alias for {!Stdlib.string_of_float}. *)

type fpclass = Stdlib.fpclass =
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

external cbrt : float -> float = "caml_cbrt_float" "caml_cbrt"
  [@@unboxed] [@@noalloc]
(** Cube root.

    @since 4.13.0
*)

external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
(** Exponential. *)

external exp2 : float -> float = "caml_exp2_float" "caml_exp2"
  [@@unboxed] [@@noalloc]
(** Base 2 exponential function.

    @since 4.13.0
*)

external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]
(** Natural logarithm. *)

external log10 : float -> float = "caml_log10_float" "log10"
[@@unboxed] [@@noalloc]
(** Base 10 logarithm. *)

external log2 : float -> float = "caml_log2_float" "caml_log2"
  [@@unboxed] [@@noalloc]
(** Base 2 logarithm.

    @since 4.13.0
*)

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

external acosh : float -> float = "caml_acosh_float" "caml_acosh"
  [@@unboxed] [@@noalloc]
(** Hyperbolic arc cosine.  The argument must fall within the range
    [[1.0, inf]].
    Result is in radians and is between [0.0] and [inf].

    @since 4.13.0
*)

external asinh : float -> float = "caml_asinh_float" "caml_asinh"
  [@@unboxed] [@@noalloc]
(** Hyperbolic arc sine.  The argument and result range over the entire
    real line.
    Result is in radians.

    @since 4.13.0
*)

external atanh : float -> float = "caml_atanh_float" "caml_atanh"
  [@@unboxed] [@@noalloc]
(** Hyperbolic arc tangent.  The argument must fall within the range
    [[-1.0, 1.0]].
    Result is in radians and ranges over the entire real line.

    @since 4.13.0
*)

external erf : float -> float = "caml_erf_float" "caml_erf"
  [@@unboxed] [@@noalloc]
(** Error function.  The argument ranges over the entire real line.
    The result is always within [[-1.0, 1.0]].

    @since 4.13.0
*)

external erfc : float -> float = "caml_erfc_float" "caml_erfc"
  [@@unboxed] [@@noalloc]
(** Complementary error function ([erfc x = 1 - erf x]).
    The argument ranges over the entire real line.
    The result is always within [[-1.0, 1.0]].

    @since 4.13.0
*)

external trunc : float -> float = "caml_trunc_float" "caml_trunc"
                                    [@@unboxed] [@@noalloc]
(** [trunc x] rounds [x] to the nearest integer whose absolute value is
   less than or equal to [x].

   @since 4.08.0 *)

external round : float -> float = "caml_round_float" "caml_round"
                                    [@@unboxed] [@@noalloc]
(** [round x] rounds [x] to the nearest integer with ties (fractional
   values of 0.5) rounded away from zero, regardless of the current
   rounding direction.  If [x] is an integer, [+0.], [-0.], [nan], or
   infinite, [x] itself is returned.

   On 64-bit mingw-w64, this function may be emulated owing to a bug in the
   C runtime library (CRT) on this platform.

   @since 4.08.0 *)

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

external next_after : float -> float -> float
  = "caml_nextafter_float" "caml_nextafter" [@@unboxed] [@@noalloc]
(** [next_after x y] returns the next representable floating-point
   value following [x] in the direction of [y].  More precisely, if
   [y] is greater (resp. less) than [x], it returns the smallest
   (resp. largest) representable number greater (resp. less) than [x].
   If [x] equals [y], the function returns [y].  If [x] or [y] is
   [nan], a [nan] is returned.
   Note that [next_after max_float infinity = infinity] and that
   [next_after 0. infinity] is the smallest denormalized positive number.
   If [x] is the smallest denormalized positive number,
   [next_after x 0. = 0.]

   @since 4.08.0 *)

external copy_sign : float -> float -> float
  = "caml_copysign_float" "caml_copysign"
[@@unboxed] [@@noalloc]
(** [copy_sign x y] returns a float whose absolute value is that of [x]
    and whose sign is that of [y].  If [x] is [nan], returns [nan].
    If [y] is [nan], returns either [x] or [-. x], but it is not
    specified which. *)

external sign_bit : (float [@unboxed]) -> bool
  = "caml_signbit_float" "caml_signbit" [@@noalloc]
(** [sign_bit x] is [true] if and only if the sign bit of [x] is set.
    For example [sign_bit 1.] and [signbit 0.] are [false] while
    [sign_bit (-1.)] and [sign_bit (-0.)] are [true].

    @since 4.08.0 *)

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

val min : t -> t -> t
(** [min x y] returns the minimum of [x] and [y].  It returns [nan]
   when [x] or [y] is [nan].  Moreover [min (-0.) (+0.) = -0.]

   @since 4.08.0 *)

val max : float -> float -> float
(** [max x y] returns the maximum of [x] and [y].  It returns [nan]
   when [x] or [y] is [nan].  Moreover [max (-0.) (+0.) = +0.]

   @since 4.08.0 *)

val min_max : float -> float -> float * float
(** [min_max x y] is [(min x y, max x y)], just more efficient.

   @since 4.08.0 *)

val min_num : t -> t -> t
(** [min_num x y] returns the minimum of [x] and [y] treating [nan] as
   missing values.  If both [x] and [y] are [nan], [nan] is returned.
   Moreover [min_num (-0.) (+0.) = -0.]

   @since 4.08.0 *)

val max_num : t -> t -> t
(** [max_num x y] returns the maximum of [x] and [y] treating [nan] as
   missing values.  If both [x] and [y] are [nan] [nan] is returned.
   Moreover [max_num (-0.) (+0.) = +0.]

   @since 4.08.0 *)

val min_max_num : float -> float -> float * float
(** [min_max_num x y] is [(min_num x y, max_num x y)], just more
   efficient.  Note that in particular [min_max_num x nan = (x, x)]
   and [min_max_num nan y = (y, y)].

   @since 4.08.0 *)


val hash: t -> int
(** The hash function for floating-point numbers. *)

module Array : sig
FLOATARRAY
end
(** Float arrays with packed representation. *)

module ArrayLabels : sig
FLOATARRAYLAB
end
(** Float arrays with packed representation (labeled functions). *)
