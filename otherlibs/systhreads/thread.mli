(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Thread]: lightweight threads for Win32 *)

type t
        (* The type of thread handles. *)

(** Thread creation and termination *)

val create : ('a -> 'b) -> 'a -> t
        (* [new funct arg] creates a new thread of control, in which the
           function application [funct arg] is executed concurrently
           with the other threads of the program. The application of [new]
           returns the handle of the newly created thread.
           The new thread terminates when the application [funct arg]
           returns, either normally or by raising an uncaught exception.
           In the latter case, the exception is printed on standard error,
           but not propagated back to the parent thread. Similarly, the
           result of the application [funct arg] is discarded and not
           directly accessible to the parent thread. *)
external self : unit -> t = "csl_thread_self"
        (* Return the thread currently executing. *)
external id : t -> int = "csl_thread_id"
        (* Return the identifier of the given thread. A thread identifier
           is an integer that identifies uniquely the thread.
           It can be used to build data structures indexed by threads. *)
external exit : unit -> unit = "csl_thread_exit"
        (* Terminate prematurely the currently executing thread. *)
external kill : t -> unit = "csl_thread_kill"
        (* Terminate prematurely the thread whose handle is given. *)

(** Thread synchronization *)

external join : t -> unit = "csl_thread_join"
        (* [join th] suspends the execution of the calling thread
           until the thread [th] has terminated. *)

(** Suspending threads *)

val delay: float -> unit
        (* [delay d] suspends the execution of the calling thread for
           [d] seconds. The other program threads continue to run during
           this time. *)
val join : t -> unit
        (* [join th] suspends the execution of the calling thread
           until the thread [th] has terminated. *)
val wait_read : Unix.file_descr -> unit
val wait_write : Unix.file_descr -> unit
        (* Does nothing in this Win32 implementation. *)
external wait_timed_read : Unix.file_descr -> float -> bool = "csl_wait_file"
external wait_timed_write : Unix.file_descr -> float -> bool = "csl_wait_file"
        (* Suspend the calling thread until
           one character is available for reading ([wait_read]) or
           one character can be written without blocking ([wait_write])
           on the given Unix file descriptor. Wait for at most the amount
           of time given as second argument (in seconds).
           Return [true] if the file descriptor is ready for input/output
           and [false] if the timeout expired. *)
val wait_pid : int -> int * Unix.process_status
        (* [wait_pid p] suspends the execution of the calling thread
           until the process specified by the process identifier [p]
           terminates. Returns the pid of the child caught and
           its termination status, as per [Unix.wait]. *)
(*--*)

(* The following primitives provide the basis for implementing 
   synchronization functions between threads. Their direct use is
   discouraged, as they are very low-level and prone to race conditions
   and deadlocks. The modules [Mutex], [Condition] and [Event]
   provide higher-level synchronization primitives. *)

external sleep : unit -> unit = "csl_thread_sleep"
        (* Suspend the calling thread until another thread reactivates it
           using [wakeup]. Just before suspending the thread,
           [critical_section] is reset to [false]. Resetting
           [critical_section] and suspending the calling thread is an
           atomic operation. *)
external wakeup : t -> unit = "csl_thread_wakeup"
        (* Reactivate the given thread. After the call to [wakeup],
           the suspended thread will resume execution at some future time. *)

