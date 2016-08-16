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

(* Module [Int63]: 63-bit integers *)

type t

external neg : t -> t = "%int63_neg"
external add : t -> t -> t = "%int63_add"
external sub : t -> t -> t = "%int63_sub"
external mul : t -> t -> t = "%int63_mul"
external div : t -> t -> t = "%int63_div"
external rem : t -> t -> t = "%int63_mod"
external logand : t -> t -> t = "%int63_and"
external logor : t -> t -> t = "%int63_or"
external logxor : t -> t -> t = "%int63_xor"
external shift_left : t -> int -> t = "%int63_lsl"
external shift_right : t -> int -> t = "%int63_asr"
external shift_right_logical : t -> int -> t = "%int63_lsr"
external of_int : int -> t = "%int63_of_int"
external to_int : t -> int = "%int63_to_int"

external of_int32 : int32 -> t = "%int63_of_int32"
external to_int32 : t -> int32 = "%int63_to_int32"
external of_int64 : int64 -> t = "%int63_of_int64"
external to_int64 : t -> int64 = "%int63_to_int64"
external of_nativeint : nativeint -> t = "%int63_of_nativeint"
external to_nativeint : t -> nativeint = "%int63_to_nativeint"

external format : string -> t -> string = "caml_int63_format"
let to_string n = format "%d" n

external of_string : string -> t = "caml_int63_of_string"

let zero = of_int 0
let one = of_int 1
let minus_one = of_int (-1)
let succ n = add n one
let pred n = sub n one
let abs n = if n >= zero then n else neg n
let lognot n = logxor n minus_one
let max_int = of_string "0x3FFFFFFFFFFFFFFF"
let min_int = of_string "0x4000000000000000"

let compare (x: t) (y: t) = Pervasives.compare x y
let equal (x: t) (y: t) = compare x y = 0

