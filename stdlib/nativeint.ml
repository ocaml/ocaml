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

(* Module [Nativeint]: processor-native integers *)

type t

external neg: t -> t = "nativeint_neg"
external add: t -> t -> t = "nativeint_add"
external sub: t -> t -> t = "nativeint_sub"
external mul: t -> t -> t = "nativeint_mul"
external div: t -> t -> t = "nativeint_div"
external rem: t -> t -> t = "nativeint_mod"
external logand: t -> t -> t = "nativeint_and"
external logor: t -> t -> t = "nativeint_or"
external logxor: t -> t -> t = "nativeint_xor"
external shift_left: t -> int -> t = "nativeint_shift_left"
external shift_right: t -> int -> t = "nativeint_shift_right"
external shift_right_logical: t -> int -> t = "nativeint_shift_right_unsigned"
external of_int: int -> t = "nativeint_of_int"
external to_int: t -> int = "nativeint_to_int"

let zero = of_int 0
let one = of_int 1
let minus_one = of_int (-1)
let succ n = add n one
let pred n = sub n one
let abs n = if n >= zero then n else neg n
let min = shift_left one (Sys.word_size - 1)
let max = add min one
let lognot n = logxor n minus_one

external format : string -> t -> string = "nativeint_format"
let to_string n = format "%d" n

external of_string: string -> t = "nativeint_of_string"
