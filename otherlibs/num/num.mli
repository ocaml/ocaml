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

(* Module [Num]: operation on arbitrary-precision numbers *)

open Nat
open Big_int
open Ratio

(* Numbers (type [num]) are arbitrary-precision rational numbers,
   plus the special elements [1/0] (infinity) and [0/0] (undefined). *)

type num = Int of int | Big_int of big_int | Ratio of ratio
        (* The type of numbers. *)

(* Arithmetic operations *)

val (+/) : num -> num -> num
val add_num : num -> num -> num
        (* Addition *)
val minus_num : num -> num
        (* Unary negation. *)
val (-/) : num -> num -> num
val sub_num : num -> num -> num
        (* Subtraction *)
val ( */ ) : num -> num -> num
val mult_num : num -> num -> num
        (* Multiplication *)
val square_num : num -> num
        (* Squaring *)
val (//) : num -> num -> num
val div_num : num -> num -> num
        (* Division *)
val quo_num : num -> num -> num
val mod_num : num -> num -> num
        (* Euclidean division: quotient and remainder *)
val ( **/ ) : num -> num -> num
val power_num : num -> num -> num
        (* Exponentiation *)
val is_integer_num : num -> bool
        (* Test if a number is an integer *)
val integer_num : num -> num
val floor_num : num -> num
val round_num : num -> num
val ceiling_num : num -> num
        (* Approximate a number by an integer.
           [floor_num n] returns the largest integer smaller or equal to [n].
           [ceiling_num n] returns the smallest integer bigger or equal to [n].
           [integer_num n] returns the integer closest to [n]. In case of ties,
           rounds towards zero.
           [round_num n] returns the integer closest to [n]. In case of ties,
           rounds off zero. *)
val sign_num : num -> int
        (* Return [-1], [0] or [1] according to the sign of the argument. *)
val (=/) : num -> num -> bool
val (</) : num -> num -> bool
val (>/) : num -> num -> bool
val (<=/) : num -> num -> bool
val (>=/) : num -> num -> bool
val (<>/) : num -> num -> bool
val eq_num : num -> num -> bool
val lt_num : num -> num -> bool
val le_num : num -> num -> bool
val gt_num : num -> num -> bool
val ge_num : num -> num -> bool
        (* Usual comparisons between numbers *)
val compare_num : num -> num -> int
        (* Return [-1], [0] or [1] if the first argument is less than,
           equal to, or greater than the second argument. *)
val max_num : num -> num -> num
val min_num : num -> num -> num
        (* Return the greater (resp. the smaller) of the two arguments. *)
val abs_num : num -> num
        (* Absolute value. *)
val succ_num: num -> num
        (* [succ n] is [n+1] *)
val pred_num: num -> num
        (* [pred n] is [n-1] *)
val incr_num: num ref -> unit
        (* [incr r] is [r:=!r+1], where [r] is a reference to a number. *)
val decr_num: num ref -> unit
        (* [decr r] is [r:=!r-1], where [r] is a reference to a number. *)

(* Coercions with strings *)

val string_of_num : num -> string
        (* Convert a number to a string, using fractional notation. *)
val approx_num_fix : int -> num -> string
val approx_num_exp : int -> num -> string
        (* Approximate a number by a decimal. The first argument is the
           required precision. The second argument is the number to
           approximate. [approx_fix] uses decimal notation; the first
           argument is the number of digits after the decimal point.
           [approx_exp] uses scientific (exponential) notation; the
           first argument is the number of digits in the mantissa. *)
val num_of_string : string -> num
        (* Convert a string to a number. *)

(* Coercions between numerical types *)

val int_of_num : num -> int
val num_of_int : int -> num
val nat_of_num : num -> nat
val num_of_nat : nat -> num
val num_of_big_int : big_int -> num
val big_int_of_num : num -> big_int
val ratio_of_num : num -> ratio
val num_of_ratio : ratio -> num
val float_of_num : num -> float

