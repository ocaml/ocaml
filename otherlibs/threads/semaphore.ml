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
   synchronizing the execution of several threads. *)

type t = 
  { mut: Mutex.t;
    notzero: Condition.t;
    mutable status: int }

let create n =
  { mut = Mutex.create(); notzero = Condition.create(); status = n }

let post s =
  Mutex.lock s.mut;
  s.status <- s.status + 1;
  if s.status = 1 then Condition.signal s.notzero;
  Mutex.unlock s.mut

let wait s =
  Mutex.lock s.mut;
  while s.status = 0 do Condition.wait s.notzero s.mut done;
  s.status <- s.status - 1;
  Mutex.unlock s.mut
    
let getvalue s = s.status
