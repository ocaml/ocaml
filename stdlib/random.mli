(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Random]: pseudo-random number generator *)

val init : int -> unit
  (* Initialize the generator, using the argument as a seed.
     The same seed will always yield the same sequence of numbers. *)
val full_init : int array -> unit
  (* Same as [init] but takes more data as seed. *)

val bits : unit -> int
  (* Return 30 random bits in a nonnegative integer. *)
val int : int -> int
  (* [Random.int bound] returns a random number between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be positive and smaller
     than $2^{30}$. *)
val float : float -> float
  (* [Random.float bound] returns a random number between 0 (inclusive)
     and [bound] (exclusive). *)
