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

(* Module [Nativeint]: processor-native integers *)

external neg: nativeint -> nativeint = "%nativeint_neg"
external add: nativeint -> nativeint -> nativeint = "%nativeint_add"
external sub: nativeint -> nativeint -> nativeint = "%nativeint_sub"
external mul: nativeint -> nativeint -> nativeint = "%nativeint_mul"
external div: nativeint -> nativeint -> nativeint = "%nativeint_div"
external rem: nativeint -> nativeint -> nativeint = "%nativeint_mod"
external logand: nativeint -> nativeint -> nativeint = "%nativeint_and"
external logor: nativeint -> nativeint -> nativeint = "%nativeint_or"
external logxor: nativeint -> nativeint -> nativeint = "%nativeint_xor"
external shift_left: nativeint -> int -> nativeint = "%nativeint_lsl"
external shift_right: nativeint -> int -> nativeint = "%nativeint_asr"
external shift_right_logical: nativeint -> int -> nativeint = "%nativeint_lsr"
external of_int: int -> nativeint = "%nativeint_of_int"
external to_int: nativeint -> int = "%nativeint_to_int"
external of_float : float -> nativeint
  = "caml_nativeint_of_float" "caml_nativeint_of_float_unboxed"
  [@@unboxed] [@@noalloc]
external to_float : nativeint -> float
  = "caml_nativeint_to_float" "caml_nativeint_to_float_unboxed"
  [@@unboxed] [@@noalloc]
external of_int32: int32 -> nativeint = "%nativeint_of_int32"
external to_int32: nativeint -> int32 = "%nativeint_to_int32"

let zero = 0n
let one = 1n
let minus_one = -1n
let succ n = add n 1n
let pred n = sub n 1n
let abs n = if n >= 0n then n else neg n
let size = Sys.word_size
let min_int = shift_left 1n (size - 1)
let max_int = sub min_int 1n
let lognot n = logxor n (-1n)

let unsigned_to_int =
  let max_int = of_int Stdlib.max_int in
  fun n ->
    if n >= 0n && n <= max_int then
      Some (to_int n)
    else
      None

external format : string -> nativeint -> string = "caml_nativeint_format"
let to_string n = format "%d" n

external of_string: string -> nativeint = "caml_nativeint_of_string"

let of_string_opt s =
  try Some (of_string s)
  with Failure _ -> None

type t = nativeint

let compare (x: t) (y: t) = Stdlib.compare x y
let equal (x: t) (y: t) = compare x y = 0

let unsigned_compare n m =
  compare (sub n min_int) (sub m min_int)

let unsigned_lt n m =
  sub n min_int < sub m min_int

let min x y : t = if x <= y then x else y
let max x y : t = if x >= y then x else y

(* Unsigned division from signed division of the same bitness.
   See Warren Jr., Henry S. (2013). Hacker's Delight (2 ed.), Sec 9-3.
*)
let unsigned_div n d =
  if d < zero then
    if unsigned_lt n d then zero else one
  else
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if unsigned_lt r d then q else succ q

let unsigned_rem n d =
  sub n (mul (unsigned_div n d) d)

(* Floor division, ceil division *)

let fdiv n d =
  let q = div n d in
  if logxor n d >= 0n (* n and d have same sign *) || n = mul q d
  then q else pred q

let cdiv n d =
  let q = div n d in
  if logxor n d < 0n (* n and d have different signs *) || n = mul q d
  then q else succ q

(* Euclidean division and remainder *)

let erem n d =
  let r = rem n d in
  if r >= 0n then r else if d >= 0n then add r d else sub r d

let ediv n d =
  let q = div n d in
  let r = sub n (mul q d) in
  if r >= 0n then q else if d >= 0n then pred q else succ q

(* Bit counting functions *)

let leading_zeros =
  if size = 64
  then fun x -> Int64.(leading_zeros (of_nativeint x))
  else fun x -> Int32.(leading_zeros (to_int32 x))

let unsigned_bitsize x =
  size - leading_zeros x

let leading_sign_bits x =
  if x >= 0n then leading_zeros x - 1 else leading_zeros (lognot x) - 1

let signed_bitsize x =
  size - leading_sign_bits x

let trailing_zeros =
  if size = 64
  then fun x -> Int64.(trailing_zeros (of_nativeint x))
  else fun x -> Int32.(trailing_zeros (to_int32 x))

let popcount =
  if size = 64
  then fun x -> Int64.(popcount (of_nativeint x))
  else fun x -> Int32.(popcount (to_int32 x))

external seeded_hash_param :
  int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]
let seeded_hash seed x = seeded_hash_param 10 100 seed x
let hash x = seeded_hash_param 10 100 0 x
