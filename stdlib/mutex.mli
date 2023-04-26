(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Locks for mutual exclusion.

   Mutexes (mutual-exclusion locks) are used to implement critical sections
   and protect shared mutable data structures against concurrent accesses.
   The typical use is (if [m] is the mutex associated with the data structure
   [D]):
   {[
     Mutex.lock m;
     (* Critical section that operates over D *);
     Mutex.unlock m
   ]}
*)

type t
(** The type of mutexes. *)

val create : unit -> t
(** Return a new mutex. *)

val lock : t -> unit
(** Lock the given mutex. Only one thread can have the mutex locked
   at any time. A thread that attempts to lock a mutex already locked
   by another thread will suspend until the other thread unlocks
   the mutex.

   @raise Sys_error if the mutex is already locked by the thread calling
   {!Mutex.lock}.

   @before 4.12 {!Sys_error} was not raised for recursive locking
   (platform-dependent behaviour) *)

val try_lock : t -> bool
(** Same as {!Mutex.lock}, but does not suspend the calling thread if
   the mutex is already locked: just return [false] immediately
   in that case. If the mutex is unlocked, lock it and
   return [true]. *)

val unlock : t -> unit
(** Unlock the given mutex. Other threads suspended trying to lock
   the mutex will restart.  The mutex must have been previously locked
   by the thread that calls {!Mutex.unlock}.
   @raise Sys_error if the mutex is unlocked or was locked by another thread.

   @before 4.12 {!Sys_error} was not raised when unlocking an unlocked mutex
   or when unlocking a mutex from a different thread. *)

val protect : t -> (unit -> 'a) -> 'a
(** [protect mutex f] runs [f()] in a critical section where [mutex]
    is locked (using {!lock}); it then takes care of releasing [mutex],
    whether [f()] returned a value or raised an exception.

    The unlocking operation is guaranteed to always takes place,
    even in the event an asynchronous exception (e.g. {!Sys.Break}) is raised
    in some signal handler.

    @since 5.1 *)
