(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Thread]: user-level lightweight threads *)

type t
        (* The type of thread handles. *)

(** Thread creation and termination *)

val new : ('a -> 'b) -> 'a -> t
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

(** Thread synchronization *)

external join : t -> unit = "csl_thread_join"
        (* [join th] suspends the execution of the calling thread
           until the thread [th] has terminated. *)
external detach : t -> unit = "csl_thread_detach"
        (* [detach th] indicates that the thread [th] will never
           be joined, and that its resources can be freed as soon
           as it terminates. *)

(** Thread scheduling *)

external yield : unit -> unit = "csl_thread_yield"
        (* [yield()] suggests the scheduler that this is a good point
           to suspend the current thread and reactivate another one.
           This is just a hint: the scheduler preempts periodically
           long-running threads even if they never execute [yield()].
           Using [yield()] may improve the responsiveness of the program,
           though. *)
