(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy and Pascal Cuoq, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Mutex]: locks for mutual exclusion *)

(* Mutexes (mutual-exclusion locks) are used to implement critical sections
   and protect shared mutable data structures against concurrent accesses.
   The typical use is (if [m] is the mutex associated with the data structure
   [D]):
   [
     Mutex.lock m;
     (* Critical section that operates over D *);
     Mutex.unlock m
   ]
*)

type t
        (* The type of mutexes. *)
external create: unit -> t = "caml_mutex_new"
        (* Return a new mutex. *)
external lock: t -> unit = "caml_mutex_lock"
        (* Lock the given mutex. Only one thread can have the mutex locked
           at any time. A thread that attempts to lock a mutex already locked
           by another thread will suspend until the other thread unlocks
           the mutex. *)
external try_lock: t -> bool = "caml_mutex_try_lock"
        (* Same as [try_lock], but does not suspend the calling thread if
           the mutex is already locked: just return [false] immediately
           in that case. If the mutex is unlocked, lock it and
           return [true]. *)
external unlock: t -> unit = "caml_mutex_unlock"
        (* Unlock the given mutex. Other threads suspended trying to lock
           the mutex will restart. *)
