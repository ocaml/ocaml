(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Mitchell Plamann, Jane Street Group, LLC               *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group, LLC                                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Int63]: 63-bit integers *)

type t = int63

external neg : int63 -> int63 = "%int63_neg"
external add : int63 -> int63 -> int63 = "%int63_add"
external sub : int63 -> int63 -> int63 = "%int63_sub"
external mul : int63 -> int63 -> int63 = "%int63_mul"
external div : int63 -> int63 -> int63 = "%int63_div"
external rem : int63 -> int63 -> int63 = "%int63_mod"
external logand : int63 -> int63 -> int63 = "%int63_and"
external logor : int63 -> int63 -> int63 = "%int63_or"
external logxor : int63 -> int63 -> int63 = "%int63_xor"
external shift_left : int63 -> int -> int63 = "%int63_lsl"
external shift_right : int63 -> int -> int63 = "%int63_asr"
external shift_right_logical : int63 -> int -> int63 = "%int63_lsr"
external of_int : int -> int63 = "%int63_of_int"
external to_int : int63 -> int = "%int63_to_int"

external of_int32 : int32 -> int63 = "%int63_of_int32"
external to_int32 : int63 -> int32 = "%int63_to_int32"
external of_int64 : int64 -> int63 = "%int63_of_int64"
external to_int64 : int63 -> int64 = "%int63_to_int64"
external of_nativeint : nativeint -> int63 = "%int63_of_nativeint"
external to_nativeint : int63 -> nativeint = "%int63_to_nativeint"

external format : string -> int63 -> string = "caml_int63_format"
let to_string n = format "%d" n

external of_string : string -> int63 = "caml_int63_of_string"

let zero = of_int 0
let one = of_int 1
let minus_one = of_int (-1)
let succ n = add n one
let pred n = sub n one
let abs n = if n >= zero then n else neg n
let lognot n = logxor n minus_one
let max_int = of_string "0x3FFFFFFFFFFFFFFF"
let min_int = of_string "0x4000000000000000"

let compare (x: int63) (y: int63) = Pervasives.compare x y
let equal (x: int63) (y: int63) = compare x y = 0
