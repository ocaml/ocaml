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

(* Module [Int64]: 64-bit integers *)

type t

external neg: t -> t = "int64_neg"
external add: t -> t -> t = "int64_add"
external sub: t -> t -> t = "int64_sub"
external mul: t -> t -> t = "int64_mul"
external div: t -> t -> t = "int64_div"
external rem: t -> t -> t = "int64_mod"
external logand: t -> t -> t = "int64_and"
external logor: t -> t -> t = "int64_or"
external logxor: t -> t -> t = "int64_xor"
external shift_left: t -> int -> t = "int64_shift_left"
external shift_right: t -> int -> t = "int64_shift_right"
external shift_right_logical: t -> int -> t = "int64_shift_right_unsigned"
external of_int: int -> t = "int64_of_int"
external to_int: t -> int = "int64_to_int"
external of_int32: Int32.t -> t = "int64_of_int32"
external to_int32: Int32.t -> int = "int64_to_int32"

let zero = try of_int 0 with Invalid_argument _ -> Obj.magic Int32.zero
let one = try of_int 1 with Invalid_argument _ -> Obj.magic Int32.one
let minus_one = try of_int (-1) with Invalid_argument _ -> Obj.magic Int32.minus_one
let succ n = add n one
let pred n = sub n one
let abs n = if n >= zero then n else neg n
let min = try shift_left one 63 with Invalid_argument _ -> Obj.magic Int32.min
let max = try add min one with Invalid_argument _ -> Obj.magic Int32.max
let lognot n = logxor n minus_one

external format : string -> t -> string = "int64_format"
let to_string n = format "%d" n

external of_string: string -> t = "int64_of_string"
