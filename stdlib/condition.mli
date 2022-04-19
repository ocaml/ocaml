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

(**Condition variables.

   Condition variables are useful when several threads wish to access a
   shared data structure that is protected by a mutex (a mutual exclusion
   lock).

   A condition variable is a {i communication channel}. On the receiver
   side, one or more threads can indicate that they wish to {i wait}
   for a certain property to become true. On the sender side, a thread
   can {i signal} that this property has become true, causing one (or
   more) waiting threads to be woken up.

   For instance, in the implementation of a queue data structure, if a
   thread that wishes to extract an element finds that the queue is
   currently empty, then this thread waits for the queue to become
   nonempty. A thread that inserts an element into the queue signals
   that the queue is nonempty. A condition variable is used for this
   purpose. This communication channel conveys the information that
   the property "the queue is nonempty" is true, or more accurately,
   may be true. (We explain below why the receiver of a signal cannot
   be certain that the property holds.)

   In short, a condition variable [c] is used to convey the information
   that a certain property [P] about a shared data structure [D],
   protected by a mutex [m], may be true.

   With a condition variable [c], exactly one mutex [m] is associated.
   This association is implicit: the mutex [m] is not explicitly passed
   as an argument to {!create}. It is up to the programmer to know, for
   each condition variable [c], which is the associated mutex [m].

   With a mutex [m], several condition variables can be associated.
   For instance, in a bounded queue, one condition variable could
   be used to indicate that the queue is nonempty, and another condition
   variable could be used to indicate that the queue is not full.

   With a condition variable [c], exactly one logical property [P]
   should be associated. Examples of such properties
   include "the queue is nonempty" and "the queue is not full".
   It is up to the programmer to keep track, for each condition
   variable, of the corresponding property [P].
   A signal is sent on the condition variable [c]
   as an indication that the property [P] is true, or may be true.
   On the receiving end, however, a thread that is woken up
   cannot assume that [P] is true;
   after a call to {!wait} terminates,
   one must explicitly test whether [P] is true.
   There are several reasons why this is so.
   One reason is that,
   between the moment when the signal is sent
   and the moment when a waiting thread receives the signal
   and is scheduled,
   the property [P] may be falsified by some other thread
   that is able to acquire the mutex [m] and alter the data structure [D].
   Another reason is that {i spurious wakeups} may occur:
   a waiting thread can be woken up even if no signal was sent.

   Assuming that [D] is a shared data structure
   protected by the mutex [m],
   and assuming that [c] is a condition variable
   associated with the mutex [m]
   and with the property [P],
   a typical usage scenario is as follows:
   {[
     Mutex.lock m;
     while (* the property P over D is not satisfied *) do
       Condition.wait c m
     done;
     (* Modify D *)
     if (* the property P over D is now satisfied *) then Condition.signal c;
     Mutex.unlock m
   ]}
*)

type t
(** The type of condition variables. *)

val create : unit -> t
(**[create()] creates and returns a new condition variable.
   This condition variable should be associated (in the programmer's mind)
   with a certain mutex [m], and should be used only when the mutex [m] is
   locked. *)

val wait : t -> Mutex.t -> unit
(**The call [wait c m] is permitted only if [m] is the mutex associated
   with the condition variable [c], and only if [m] is currently locked.
   This call atomically unlocks the mutex [m] and suspends the
   current thread on the condition variable [c]. This thread can
   later be woken up after the condition variable [c] has been signaled
   via {!signal} or {!broadcast}; however, it can also be woken up for
   no reason. The mutex [m] is locked again before [wait] returns. One
   cannot assume that the property [P] associated with the condition
   variable [c] holds when [wait] returns; one must explicitly test
   whether [P] holds after calling [wait]. *)

val signal : t -> unit
(**[signal c] wakes up one of the threads waiting on the condition
   variable [c], if there is one. If there is none, this call has
   no effect.

   It is recommended to call [signal c] inside a critical section,
   that is, while the mutex [m] associated with [c] is held. *)

val broadcast : t -> unit
(**[broadcast c] wakes up all threads waiting on the condition
   variable [c]. If there are none, this call has no effect.

   It is recommended to call [broadcast c] inside a critical section,
   that is, while the mutex [m] associated with [c] is held. *)
