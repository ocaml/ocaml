(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Pseudo-random number generator (PRNG). *)

(** Initialize the generator, using the argument as a seed.
     The same seed will always yield the same sequence of numbers. *)
val init : int -> unit

(** Same as {!Random.init} but takes more data as seed. *)
val full_init : int array -> unit

(** Initialize the generator with a more-or-less random seed chosen
   in a system-dependent way. *)
val self_init : unit -> unit

(** Return 30 random bits in a nonnegative integer. *)
val bits : unit -> int

(** [Random.int bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be more than 0 and less
     than 2{^30}. *)
val int : int -> int

(** [Random.float bound] returns a random floating-point number
   between 0 (inclusive) and [bound] (exclusive).  If [bound] is
   negative, the result is negative.  If [bound] is 0, the result
   is 0. *)
val float : float -> float

(** Values of this type are used to store the current state of the
   generator. *)
type state;;

(** Returns the current state of the generator.  This is useful for
   checkpointing computations that use the PRNG. *)
val get_state : unit -> state;;

(** Resets the state of the generator to some previous state returned by
   {!Random.get_state}. *)
val set_state : state -> unit;;

