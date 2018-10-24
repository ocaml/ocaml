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

external neg : float -> float = "%negfloat"
external add : float -> float -> float = "%addfloat"
external sub : float -> float -> float = "%subfloat"
external mul : float -> float -> float = "%mulfloat"
external div : float -> float -> float = "%divfloat"
external rem : float -> float -> float = "caml_fmod_float" "fmod"
  [@@unboxed] [@@noalloc]
external fma : float -> float -> float -> float = "caml_fma_float" "caml_fma"
  [@@unboxed] [@@noalloc]
external abs : float -> float = "%absfloat"

let zero = 0.
let one = 1.
let minus_one = -1.
let infinity = Stdlib.infinity
let neg_infinity = Stdlib.neg_infinity
let nan = Stdlib.nan
let is_finite (x: float) = x -. x = 0.
let is_infinite (x: float) = 1. /. x = 0.
let is_nan (x: float) = x <> x

let pi = 0x1.921fb54442d18p+1
let max_float = Stdlib.max_float
let min_float = Stdlib.min_float
let epsilon = Stdlib.epsilon_float
external of_int : int -> float = "%floatofint"
external to_int : float -> int = "%intoffloat"
external of_string : string -> float = "caml_float_of_string"
let of_string_opt = Stdlib.float_of_string_opt
let to_string = Stdlib.string_of_float
type fpclass = Stdlib.fpclass =
    FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
external classify_float : (float [@unboxed]) -> fpclass =
  "caml_classify_float" "caml_classify_float_unboxed" [@@noalloc]
external pow : float -> float -> float = "caml_power_float" "pow"
  [@@unboxed] [@@noalloc]
external sqrt : float -> float = "caml_sqrt_float" "sqrt"
  [@@unboxed] [@@noalloc]
external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]
external log10 : float -> float = "caml_log10_float" "log10"
  [@@unboxed] [@@noalloc]
external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
  [@@unboxed] [@@noalloc]
external log1p : float -> float = "caml_log1p_float" "caml_log1p"
  [@@unboxed] [@@noalloc]
external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]
external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]
external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]
external acos : float -> float = "caml_acos_float" "acos"
  [@@unboxed] [@@noalloc]
external asin : float -> float = "caml_asin_float" "asin"
  [@@unboxed] [@@noalloc]
external atan : float -> float = "caml_atan_float" "atan"
  [@@unboxed] [@@noalloc]
external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
  [@@unboxed] [@@noalloc]
external hypot : float -> float -> float
               = "caml_hypot_float" "caml_hypot" [@@unboxed] [@@noalloc]
external cosh : float -> float = "caml_cosh_float" "cosh"
  [@@unboxed] [@@noalloc]
external sinh : float -> float = "caml_sinh_float" "sinh"
  [@@unboxed] [@@noalloc]
external tanh : float -> float = "caml_tanh_float" "tanh"
  [@@unboxed] [@@noalloc]
external trunc : float -> float = "caml_trunc_float" "caml_trunc"
  [@@unboxed] [@@noalloc]
external round : float -> float = "caml_round_float" "caml_round"
  [@@unboxed] [@@noalloc]
external ceil : float -> float = "caml_ceil_float" "ceil"
  [@@unboxed] [@@noalloc]
external floor : float -> float = "caml_floor_float" "floor"
[@@unboxed] [@@noalloc]

let is_integer x = x = trunc x && is_finite x

external next_after : float -> float -> float
  = "caml_nextafter_float" "caml_nextafter" [@@unboxed] [@@noalloc]

let succ x = next_after x infinity
let pred x = next_after x neg_infinity

external copy_sign : float -> float -> float
                  = "caml_copysign_float" "caml_copysign"
                  [@@unboxed] [@@noalloc]
external sign_bit : (float [@unboxed]) -> bool
  = "caml_signbit_float" "caml_signbit" [@@noalloc]

external frexp : float -> float * int = "caml_frexp_float"
external ldexp : (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed]) =
  "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
external modf : float -> float * float = "caml_modf_float"
type t = float
external compare : float -> float -> int = "%compare"
let equal x y = compare x y = 0

let[@inline] min (x: float) (y: float) =
  if y > x || (not(sign_bit y) && sign_bit x) then
    if is_nan y then y else x
  else if is_nan x then x else y

let[@inline] max (x: float) (y: float) =
  if y > x || (not(sign_bit y) && sign_bit x) then
    if is_nan x then x else y
  else if is_nan y then y else x

let[@inline] min_max (x: float) (y: float) =
  if is_nan x || is_nan y then (nan, nan)
  else if y > x || (not(sign_bit y) && sign_bit x) then (x, y) else (y, x)

let[@inline] min_num (x: float) (y: float) =
  if y > x || (not(sign_bit y) && sign_bit x) then
    if is_nan x then y else x
  else if is_nan y then x else y

let[@inline] max_num (x: float) (y: float) =
  if y > x || (not(sign_bit y) && sign_bit x) then
    if is_nan y then x else y
  else if is_nan x then y else x

let[@inline] min_max_num (x: float) (y: float) =
  if is_nan x then (y,y)
  else if is_nan y then (x,x)
  else if y > x || (not(sign_bit y) && sign_bit x) then (x,y) else (y,x)

external seeded_hash_param : int -> int -> int -> float -> int
                           = "caml_hash" [@@noalloc]
let hash x = seeded_hash_param 10 100 0 x

module Array = struct
  type t = floatarray
  external create : int -> t = "caml_floatarray_create"
  external length : t -> int = "%floatarray_length"
  external get : t -> int -> float = "%floatarray_safe_get"
  external set : t -> int -> float -> unit = "%floatarray_safe_set"
  external unsafe_get : t -> int -> float = "%floatarray_unsafe_get"
  external unsafe_set : t -> int -> float -> unit = "%floatarray_unsafe_set"
end
