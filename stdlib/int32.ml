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

type t

external neg: t -> t = "int32_neg"
external add: t -> t -> t = "int32_add"
external sub: t -> t -> t = "int32_sub"
external mul: t -> t -> t = "int32_mul"
external div: t -> t -> t = "int32_div"
external rem: t -> t -> t = "int32_mod"
external logand: t -> t -> t = "int32_and"
external logor: t -> t -> t = "int32_or"
external logxor: t -> t -> t = "int32_xor"
external shift_left: t -> int -> t = "int32_shift_left"
external shift_right: t -> int -> t = "int32_shift_right"
external shift_right_logical: t -> int -> t = "int32_shift_right_unsigned"
external of_int: int -> t = "int32_of_int"
external to_int: t -> int = "int32_to_int"

let zero = of_int 0
let one = of_int 1
let minus_one = of_int (-1)
let succ n = add n one
let pred n = sub n one
let abs n = if n >= zero then n else neg n
let min = shift_left one 31
let max = add min one
let lognot n = logxor n minus_one

external format : string -> t -> string = "int32_format"
let to_string n = format "%d" n

external of_string: string -> t = "int32_of_string"
