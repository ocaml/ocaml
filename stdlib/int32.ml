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

(* Module [Int32]: 32-bit integers *)

external neg: int32 -> int32 = "%int32_neg"
external add: int32 -> int32 -> int32 = "%int32_add"
external sub: int32 -> int32 -> int32 = "%int32_sub"
external mul: int32 -> int32 -> int32 = "%int32_mul"
external div: int32 -> int32 -> int32 = "%int32_div"
external rem: int32 -> int32 -> int32 = "%int32_mod"
external logand: int32 -> int32 -> int32 = "%int32_and"
external logor: int32 -> int32 -> int32 = "%int32_or"
external logxor: int32 -> int32 -> int32 = "%int32_xor"
external shift_left: int32 -> int -> int32 = "%int32_lsl"
external shift_right: int32 -> int -> int32 = "%int32_asr"
external shift_right_logical: int32 -> int -> int32 = "%int32_lsr"
external of_int: int -> int32 = "%int32_of_int"
external to_int: int32 -> int = "%int32_to_int"

let zero = of_int 0
let one = of_int 1
let minus_one = of_int (-1)
let succ n = add n one
let pred n = sub n one
let abs n = if n >= zero then n else neg n
let min = shift_left one 31
let max = sub min one
let lognot n = logxor n minus_one

external format : string -> int32 -> string = "int32_format"
let to_string n = format "%d" n

external of_string: string -> int32 = "int32_of_string"
