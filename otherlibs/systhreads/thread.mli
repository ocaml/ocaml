(***********************************************************************)
(*                                                                     *)
(*                         Objective Caml                              *)
(*                                                                     *)
(*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Thread]: lightweight threads for Posix 1003.1c and Win32 *)

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
external self : unit -> t = "caml_thread_self"
        (* Return the thread currently executing. *)
external id : t -> int = "caml_thread_id"
        (* Return the identifier of the given thread. A thread identifier
           is an integer that identifies uniquely the thread.
           It can be used to build data structures indexed by threads. *)
external exit : unit -> unit = "caml_thread_exit"
        (* Terminate prematurely the currently executing thread. *)
external kill : t -> unit = "caml_thread_kill"
        (* Terminate prematurely the thread whose handle is given. *)

(** Suspending threads *)

val delay: float -> unit
        (* [delay d] suspends the execution of the calling thread for
           [d] seconds. The other program threads continue to run during
           this time. *)
external join : t -> unit = "caml_thread_join"
        (* [join th] suspends the execution of the calling thread
           until the thread [th] has terminated. *)
val wait_read : Unix.file_descr -> unit
val wait_write : Unix.file_descr -> unit
        (* These functions do nothing in this implementation. *)
val wait_timed_read : Unix.file_descr -> float -> bool
val wait_timed_write : Unix.file_descr -> float -> bool
        (* Suspend the execution of the calling thread until at least
           one character is available for reading ([wait_read]) or
           one character can be written without blocking ([wait_write])
           on the given Unix file descriptor. Wait for at most
           the amount of time given as second argument (in seconds).
           Return [true] if the file descriptor is ready for input/output
           and [false] if the timeout expired. *)
        (* These functions return immediately [true] in the Win32
           implementation. *)
val select :
  Unix.file_descr list -> Unix.file_descr list ->
  Unix.file_descr list -> float ->
    Unix.file_descr list * Unix.file_descr list * Unix.file_descr list
        (* Suspend the execution of the calling thead until input/output
           becomes possible on the given Unix file descriptors.
           The arguments and results have the same meaning as for
           [Unix.select]. *)
        (* This function is not implemented yet under Win32. *)
val wait_pid : int -> int * Unix.process_status
        (* [wait_pid p] suspends the execution of the calling thread
           until the process specified by the process identifier [p]
           terminates. Returns the pid of the child caught and
           its termination status, as per [Unix.wait]. *)

