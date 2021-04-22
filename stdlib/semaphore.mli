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

  A semaphore is a thread synchronization device that can be used to
  control access to a shared resource.

  Two flavors of semaphores are provided: counting semaphores and
  binary semaphores.

  @since 4.12 *)

(** {2 Counting semaphores} *)

(**
  A counting semaphore is a counter that can be accessed concurrently
  by several threads.  The typical use is to synchronize producers and
  consumers of a resource by counting how many units of the resource
  are available.

  The two basic operations on semaphores are:
- "release" (also called "V", "post", "up", and "signal"), which
  increments the value of the counter.  This corresponds to producing
  one more unit of the shared resource and making it available to others.
- "acquire" (also called "P", "wait", "down", and "pend"), which
  waits until the counter is greater than zero and decrements it.
  This corresponds to consuming one unit of the shared resource.

  @since 4.12 *)

module Counting : sig

type t
(** The type of counting semaphores. *)

val make : int -> t
(** [make n] returns a new counting semaphore, with initial value [n].
    The initial value [n] must be nonnegative.

    @raise Invalid_argument if [n < 0]
*)

val release : t -> unit
(** [release s] increments the value of semaphore [s].
    If other threads are waiting on [s], one of them is restarted.
    If the current value of [s] is equal to [max_int], the value of
    the semaphore is unchanged and a [Sys_error] exception is raised
    to signal overflow.

    @raise Sys_error if the value of the semaphore would overflow [max_int]
*)

val acquire : t -> unit
(** [acquire s] blocks the calling thread until the value of semaphore [s]
    is not zero, then atomically decrements the value of [s] and returns.
*)

val try_acquire : t -> bool
(** [try_acquire s] immediately returns [false] if the value of semaphore [s]
    is zero.  Otherwise, the value of [s] is atomically decremented
    and [try_acquire s] returns [true].
*)

val get_value : t -> int
(** [get_value s] returns the current value of semaphore [s].
    The current value can be modified at any time by concurrent
    {!release} and {!acquire} operations.  Hence, the [get_value]
    operation is racy, and its result should only be used for debugging
    or informational messages.
*)

end

(** {2 Binary semaphores} *)

(** Binary semaphores are a variant of counting semaphores
    where semaphores can only take two values, 0 and 1.

    A binary semaphore can be used to control access to a single
    shared resource, with value 1 meaning "resource is available" and
    value 0 meaning "resource is unavailable".

    The "release" operation of a binary semaphore sets its value to 1,
    and "acquire" waits until the value is 1 and sets it to 0.

    A binary semaphore can be used instead of a mutex (see module
    {!Mutex}) when the mutex discipline (of unlocking the mutex from the
    thread that locked it) is too restrictive.  The "acquire" operation
    corresponds to locking the mutex, and the "release" operation to
    unlocking it, but "release" can be performed in a thread different
    than the one that performed the "acquire".  Likewise, it is safe
    to release a binary semaphore that is already available.

    @since 4.12
*)

module Binary : sig

type t
(** The type of binary semaphores. *)

val make : bool -> t
(** [make b] returns a new binary semaphore.
    If [b] is [true], the initial value of the semaphore is 1, meaning
    "available".  If [b] is [false], the initial value of the
    semaphore is 0, meaning "unavailable".
*)

val release : t -> unit
(** [release s] sets the value of semaphore [s] to 1, putting it in the
    "available" state.  If other threads are waiting on [s], one of them is
    restarted.
*)

val acquire : t -> unit
(** [acquire s] blocks the calling thread until the semaphore [s]
    has value 1 (is available), then atomically sets it to 0
    and returns.
*)

val try_acquire : t -> bool
(** [try_acquire s] immediately returns [false] if the semaphore [s]
    has value 0.  If [s] has value 1, its value is atomically set to 0
    and [try_acquire s] returns [true].
*)

end
