(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*    Valerie Menissier-Morain, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Big_int]: operations on big integers *)

(* Big integers (type [big_int]) are signed integers of arbitrary size. *)
  
open Nat
 
type big_int

val sign_big_int : big_int -> int
val zero_big_int : big_int
val unit_big_int : big_int
val num_digits_big_int : big_int -> int
val minus_big_int : big_int -> big_int
val abs_big_int : big_int -> big_int
val compare_big_int : big_int -> big_int -> int
val eq_big_int : big_int -> big_int -> bool
val le_big_int : big_int -> big_int -> bool
val ge_big_int : big_int -> big_int -> bool
val lt_big_int : big_int -> big_int -> bool
val gt_big_int : big_int -> big_int -> bool
val max_big_int : big_int -> big_int -> big_int
val min_big_int : big_int -> big_int -> big_int
val pred_big_int : big_int -> big_int
val succ_big_int : big_int -> big_int
val add_big_int : big_int -> big_int -> big_int
val big_int_of_int : int -> big_int
val add_int_big_int : int -> big_int -> big_int
val sub_big_int : big_int -> big_int -> big_int
val mult_int_big_int : int -> big_int -> big_int
val mult_big_int : big_int -> big_int -> big_int
val quomod_big_int : big_int -> big_int -> big_int * big_int
val div_big_int : big_int -> big_int -> big_int
val mod_big_int : big_int -> big_int -> big_int
val gcd_big_int : big_int -> big_int -> big_int
val int_of_big_int : big_int -> int
val is_int_big_int : big_int -> bool
val nat_of_big_int : big_int -> nat
val big_int_of_nat : nat -> big_int
val string_of_big_int : big_int -> string
val big_int_of_string : string -> big_int
val float_of_big_int : big_int -> float
val square_big_int: big_int -> big_int
val sqrt_big_int: big_int -> big_int
val base_power_big_int: int -> int -> big_int -> big_int
val sys_big_int_of_string: string -> int -> int -> big_int
val power_int_positive_int: int -> int -> big_int
val power_big_int_positive_int: big_int -> int -> big_int
val power_int_positive_big_int: int -> big_int -> big_int
val power_big_int_positive_big_int: big_int -> big_int -> big_int
val round_futur_last_digit : string -> int -> int -> bool
val approx_big_int: int -> big_int -> string
