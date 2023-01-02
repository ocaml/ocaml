(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     *)
(*                                                                        *)
(*   Copyright 1995 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Lightweight threads for Posix [1003.1c] and Win32. *)

type t
(** The type of thread handles. *)

(** {1 Thread creation and termination} *)

val create : ('a -> 'b) -> 'a -> t
(** [Thread.create funct arg] creates a new thread of control,
   in which the function application [funct arg]
   is executed concurrently with the other threads of the domain.
   The application of [Thread.create]
   returns the handle of the newly created thread.
   The new thread terminates when the application [funct arg]
   returns, either normally or by raising the {!Thread.Exit} exception
   or by raising any other uncaught exception.
   In the last case, the uncaught exception is printed on standard error,
   but not propagated back to the parent thread. Similarly, the
   result of the application [funct arg] is discarded and not
   directly accessible to the parent thread.

   See also {!Domain.spawn} if you want parallel execution instead.
   *)

val self : unit -> t
(** Return the handle for the thread currently executing. *)

val id : t -> int
(** Return the identifier of the given thread. A thread identifier
   is an integer that identifies uniquely the thread.
   It can be used to build data structures indexed by threads. *)

exception Exit
(** Exception raised by user code to initiate termination of the
    current thread.
    In a thread created by {!Thread.create} [funct] [arg], if the
    {!Thread.Exit} exception reaches the top of the application
    [funct arg], it has the effect of terminating the current thread
    silently.  In other contexts, there is no implicit handling of the
    {!Thread.Exit} exception. *)

val exit : unit -> unit
[@@ocaml.deprecated "Use 'raise Thread.Exit' instead."]
(** Raise the {!Thread.Exit} exception.
    In a thread created by {!Thread.create}, this will cause the thread
    to terminate prematurely, unless the thread function handles the
    exception itself.  {!Fun.protect} finalizers and catch-all
    exception handlers will be executed.

    To make it clear that an exception is raised and will trigger
    finalizers and catch-all exception handlers, it is recommended
    to write [raise Thread.Exit] instead of [Thread.exit ()].

    @before 5.0 A different implementation was used, not based on raising
        an exception, and not running finalizers and catch-all handlers.
        The previous implementation had a different behavior when called
        outside of a thread created by {!Thread.create}. *)

(** {1 Suspending threads} *)

val delay: float -> unit
(** [delay d] suspends the execution of the calling thread for
   [d] seconds. The other program threads continue to run during
   this time. *)

val join : t -> unit
(** [join th] suspends the execution of the calling thread
   until the thread [th] has terminated. *)

val yield : unit -> unit
(** Re-schedule the calling thread without suspending it.
   This function can be used to give scheduling hints,
   telling the scheduler that now is a good time to
   switch to other threads. *)

(** {1 Waiting for file descriptors or processes} *)

(** The functions below are leftovers from an earlier, VM-based threading
    system.  The {!Unix} module provides equivalent functionality, in
    a more general and more standard-conformant manner.  It is recommended
    to use {!Unix} functions directly. *)

val wait_timed_read : Unix.file_descr -> float -> bool
[@@ocaml.deprecated "Use Unix.select instead."]
(** See {!Thread.wait_timed_write}.*)

val wait_timed_write : Unix.file_descr -> float -> bool
[@@ocaml.deprecated "Use Unix.select instead."]
(** Suspend the execution of the calling thread until at least
   one character or EOF is available for reading ([wait_timed_read]) or
   one character can be written without blocking ([wait_timed_write])
   on the given Unix file descriptor. Wait for at most
   the amount of time given as second argument (in seconds).
   Return [true] if the file descriptor is ready for input/output
   and [false] if the timeout expired.
   The same functionality can be achieved with {!Unix.select}.
*)

val select :
  Unix.file_descr list -> Unix.file_descr list ->
  Unix.file_descr list -> float ->
    Unix.file_descr list * Unix.file_descr list * Unix.file_descr list
[@@ocaml.deprecated "Use Unix.select instead."]
(** Same function as {!Unix.select}.
   Suspend the execution of the calling thread until input/output
   becomes possible on the given Unix file descriptors.
   The arguments and results have the same meaning as for
   {!Unix.select}. *)

val wait_pid : int -> int * Unix.process_status
[@@ocaml.deprecated "Use Unix.waitpid instead."]
(** Same function as {!Unix.waitpid}.
   [wait_pid p] suspends the execution of the calling thread
   until the process specified by the process identifier [p]
   terminates. Returns the pid of the child caught and
   its termination status, as per {!Unix.wait}. *)

(** {1 Management of signals} *)

(** Signal handling follows the POSIX thread model: signals generated
  by a thread are delivered to that thread; signals generated externally
  are delivered to one of the threads that does not block it.
  Each thread possesses a set of blocked signals, which can be modified
  using {!Thread.sigmask}.  This set is inherited at thread creation time.
  Per-thread signal masks are supported only by the system thread library
  under Unix, but not under Win32, nor by the VM thread library. *)

val sigmask : Unix.sigprocmask_command -> int list -> int list
(** [sigmask cmd sigs] changes the set of blocked signals for the
   calling thread.
   If [cmd] is [SIG_SETMASK], blocked signals are set to those in
   the list [sigs].
   If [cmd] is [SIG_BLOCK], the signals in [sigs] are added to
   the set of blocked signals.
   If [cmd] is [SIG_UNBLOCK], the signals in [sigs] are removed
   from the set of blocked signals.
   [sigmask] returns the set of previously blocked signals for the thread. *)


val wait_signal : int list -> int
(** [wait_signal sigs] suspends the execution of the calling thread
   until the process receives one of the signals specified in the
   list [sigs].  It then returns the number of the signal received.
   Signal handlers attached to the signals in [sigs] will not
   be invoked.  The signals [sigs] are expected to be blocked before
   calling [wait_signal]. *)

(** {1 Uncaught exceptions} *)

val default_uncaught_exception_handler : exn -> unit
(** [Thread.default_uncaught_exception_handler] will print the thread's id,
    exception and backtrace (if available). *)

val set_uncaught_exception_handler : (exn -> unit) -> unit
(** [Thread.set_uncaught_exception_handler fn] registers [fn] as the handler
    for uncaught exceptions.

    If the newly set uncaught exception handler raise an exception,
    {!default_uncaught_exception_handler} will be called. *)
