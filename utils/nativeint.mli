(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Operations on native integers (32 or 64 bits). *)

type t

val from: int -> t
        (* Turn an integer into a native integer *)
val to_int : t -> int
        (* Return the integer value of a native integer, dropping the
           most significant bit *)
val add: t -> t -> t
val sub: t -> t -> t
        (* Addition, subtraction. *)
val shift: t -> int -> t
        (* [shift n s] shifts [n] left by [s] bits if [s] > 0
           and right by [-s] bits if [s] < 0. *)
val logand: t -> t -> t
val logor: t -> t -> t
val logxor: t -> t -> t
	(* Bitwise and, or, xor *)
val sign: t -> int
        (* [sign n1] returns [0] if [n1] is zero, a positive
           integer if [n] is positive, and a negative integer if
           [n] is negative. *)
val compare: t -> t -> int
        (* [compare n1 n2] returns [0] if [n1 = n2], a positive
           integer if [n1] > [n2], and a negative integer if
           [n1] < [n2]. *)
val cmp: t -> int -> int
        (* [cmp n1 i2] is [compare n1 (from i2)]. *)
val to_string: t -> string
        (* Return the signed decimal representation of a native integer. *)
val to_hexa_string: t -> string
        (* Return the signed hexadecimal representation of a native integer,
           in 0x notation. *)
