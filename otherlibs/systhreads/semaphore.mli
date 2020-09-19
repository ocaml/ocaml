(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Xavier Leroy, Collège de France and INRIA Paris               *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Semaphores

  A counting semaphore is a counter that can be accessed concurrently
  by several threads.  The typical use is to synchronize producers and
  consumers of a resource by counting how many units of the resource
  are available.

  The two basic operations on semaphores are:
- "post" (also called "V", "up", "signal", and "release"), which
  increments the value of the counter;
- "wait" (also called "P", "down", "pend", and "acquire"), which
  waits until the counter is greater than zero and decrements it.

  A variant of the counting semaphore is the binary semaphore,
  which has only two values, 0 and 1.  "post" sets the value to 1
  and "wait" waits until the value is 1 and sets it to 0.

  @since 4.12
*)

type t
(** The type of semaphores. *)

val make : ?max: int -> int -> t
(** [make n] returns a new counted semaphore, with initial value [n].
    The initial value [n] must be nonnegative.

    The [max] optional argument specifies the maximal value of
    the semaphore: once the value of the semaphore reaches [max],
    subsequent {!post} operations will not increase it
    above [max].  If provided, [max] must be positive and
    greater than or equal to [n].

    @raise Sys_error if [n < 0] or [max <= 0] or [n > max]
*)

val make_binary : bool -> t
(** [make_binary b] is equivalent to [{!make} ~max:1 (if b then 1 else 0)].
    It returns a new binary semaphore, with initial value [b]
    ([false] means [0], [true] means [1]) and maximal value [1].
*)

val post : t -> unit
(** [post s] increments the value of semaphore [s].
    If other threads are waiting on [s], one of them is restarted.
    If semaphore [s] was created with a maximal value, and the current
    value is equal to this maximal value, the value of the semaphore
    is unchanged and [post s] has no effect.
    If semaphore [s] was created without a maximal value, and the
    current value is equal to [max_int], the value of the semaphore
    is unchanged and a [Sys_error] exception is raised to signal overflow.

    @raise Sys_error if the value of the semaphore would overflow [max_int]
*)

val wait : t -> unit
(** [wait s] blocks the calling thread until the value of semaphore [s]
    is not zero, then atomically decrements the value of [s] and returns.
*)

val trywait : t -> bool
(** [trywait s] immediately returns [false] if the value of semaphore [s]
    is zero.  Otherwise, the value of [s] is atomically decremented
    and [trywait s] returns [true].
*)
