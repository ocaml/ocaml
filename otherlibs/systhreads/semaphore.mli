(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Semaphore]: semaphores to synchronize between threads *)

(* Semaphores are an alternative to mutexes and conditions for
   synchronizing the execution of several threads.  Semaphores
   are integer counters with atomic increment (post) and decrement (wait)
   operations. *)

type t
        (* The type of semaphores *)
val create: int -> t
        (* Return a new semaphore, initialized to the given integer. *)
val post: t -> unit
        (* Atomically increment the value of the semaphore.
           If some threads were waiting for the semaphore to become non-zero,
           one of them is restarted. *)
val wait: t -> unit
        (* Atomically decrement the value of the semaphore.
           If the semaphore was initially zero, block until it becomes non-zero
           via a [Semaphore.post] operation, then decrement it and return. *)
val getvalue: t -> int
        (* Return the current value of the semaphore. *)
