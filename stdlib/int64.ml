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

(* Module [Int64]: 64-bit integers *)

external neg : int64 -> int64 = "%int64_neg"
external add : int64 -> int64 -> int64 = "%int64_add"
external sub : int64 -> int64 -> int64 = "%int64_sub"
external mul : int64 -> int64 -> int64 = "%int64_mul"
external div : int64 -> int64 -> int64 = "%int64_div"
external rem : int64 -> int64 -> int64 = "%int64_mod"
external logand : int64 -> int64 -> int64 = "%int64_and"
external logor : int64 -> int64 -> int64 = "%int64_or"
external logxor : int64 -> int64 -> int64 = "%int64_xor"
external shift_left : int64 -> int -> int64 = "%int64_lsl"
external shift_right : int64 -> int -> int64 = "%int64_asr"
external shift_right_logical : int64 -> int -> int64 = "%int64_lsr"
external of_int : int -> int64 = "%int64_of_int"
external to_int : int64 -> int = "%int64_to_int"
external of_float : float -> int64
  = "caml_int64_of_float" "caml_int64_of_float_unboxed"
  [@@unboxed] [@@noalloc]
external to_float : int64 -> float
  = "caml_int64_to_float" "caml_int64_to_float_unboxed"
  [@@unboxed] [@@noalloc]
external of_int32 : int32 -> int64 = "%int64_of_int32"
external to_int32 : int64 -> int32 = "%int64_to_int32"
external of_nativeint : nativeint -> int64 = "%int64_of_nativeint"
external to_nativeint : int64 -> nativeint = "%int64_to_nativeint"

let zero = 0L
let one = 1L
let minus_one = -1L
let succ n = add n 1L
let pred n = sub n 1L
let abs n = if n >= 0L then n else neg n
let min_int = 0x8000000000000000L
let max_int = 0x7FFFFFFFFFFFFFFFL
let lognot n = logxor n (-1L)

let unsigned_to_int =
  let max_int = of_int Stdlib.max_int in
  fun n ->
    if n >= 0L && n <= max_int then
      Some (to_int n)
    else
      None

external format : string -> int64 -> string = "caml_int64_format"
let to_string n = format "%d" n

external of_string : string -> int64 = "caml_int64_of_string"

let of_string_opt s =
  try Some (of_string s)
  with Failure _ -> None

external bits_of_float : float -> int64
  = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
  [@@unboxed] [@@noalloc]
external float_of_bits : int64 -> float
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]

type t = int64

let compare (x: t) (y: t) = Stdlib.compare x y
let equal (x: t) (y: t) = x = y

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
  if logxor n d >= 0L (* n and d have same sign *) || n = mul q d
  then q else pred q

let cdiv n d =
  let q = div n d in
  if logxor n d < 0L (* n and d have different signs *) || n = mul q d
  then q else succ q

(* Euclidean division and remainder *)

let erem n d =
  let r = rem n d in
  if r >= 0L then r else if d >= 0L then add r d else sub r d

let ediv n d =
  let q = div n d in
  let r = sub n (mul q d) in
  if r >= 0L then q else if d >= 0L then pred q else succ q

(* Number of leading zeros.  Hacker's Delight (2 ed.), algorithm 5.12 *)

let leading_zeros x =
  let x = ref x and n = ref 64 in
  let y = shift_right_logical !x 32 in
  if y <> 0L then (n := !n - 32; x := y);
  let y = shift_right_logical !x 16 in
  if y <> 0L then (n := !n - 16; x := y);
  let y = shift_right_logical !x  8 in
  if y <> 0L then (n := !n -  8; x := y);
  let y = shift_right_logical !x  4 in
  if y <> 0L then (n := !n -  4; x := y);
  let y = shift_right_logical !x 2 in
  if y <> 0L then (n := !n -  2; x := y);
  let y = shift_right_logical !x 1 in
  if y <> 0L then !n - 2 else !n - to_int !x

let unsigned_bitsize x =
  64 - leading_zeros x

(* Number of leading sign bits. *)

let leading_sign_bits x =
  if x >= 0L then leading_zeros x - 1 else leading_zeros (lognot x) - 1

let signed_bitsize x =
  64 - leading_sign_bits x

(* Number of trailing zeros.  Hacker's Delight (2 ed.), algorithm 5.21 *)

let trailing_zeros x =
  if x = 0L then 64 else begin
    let x = ref x and n = ref 63 in
    let y = shift_left !x 32 in
    if y <> 0L then (n := !n - 32; x := y);
    let y = shift_left !x 16 in
    if y <> 0L then (n := !n - 16; x := y);
    let y = shift_left !x  8 in
    if y <> 0L then (n := !n -  8; x := y);
    let y = shift_left !x  4 in
    if y <> 0L then (n := !n -  4; x := y);
    let y = shift_left !x  2 in
    if y <> 0L then (n := !n -  2; x := y);
    let y = shift_left !x  1 in
    if y <> 0L then !n - 1 else !n
  end

(* Population count.  Hacker's Delight (2 ed.), algorithm 5.2 *)

let popcount x =
  let x = sub x (logand (shift_right_logical x 1) 0x5555_5555_5555_5555L) in
  let x = add (logand x 0x3333_3333_3333_3333L)
              (logand (shift_right_logical x 2) 0x3333_3333_3333_3333L) in
  let x = logand (add x (shift_right_logical x 4)) 0x0F0F_0F0F_0F0F_0F0FL in
  let x = add x (shift_right_logical x 8) in
  let x = add x (shift_right_logical x 16) in
  let x = add x (shift_right_logical x 32) in
  to_int x land 0x7F

external seeded_hash_param :
  int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]
let seeded_hash seed x = seeded_hash_param 10 100 seed x
let hash x = seeded_hash_param 10 100 0 x
