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

(* User-level threads *)

type t

let critical_section = ref false

type resumption_status =
    Resumed_wakeup
  | Resumed_io
  | Resumed_delay
  | Resumed_join
  | Resumed_wait of int * Unix.process_status

(* It is mucho important that the primitives that reschedule are called 
   through an ML function call, not directly. That's because when such a
   primitive returns, the bytecode interpreter is only semi-obedient:
   it takes sp from the new thread, but keeps pc from the old thread.
   But that's OK if all calls to rescheduling primitives are immediately
   followed by a RETURN operation, which will restore the correct pc
   from the stack. *)

external thread_initialize : unit -> unit = "thread_initialize"
external thread_new : (unit -> unit) -> t = "thread_new"
external thread_yield : unit -> unit = "thread_yield"
external thread_sleep : unit -> unit = "thread_sleep"
external thread_wait_read : Unix.file_descr -> unit = "thread_wait_read"
external thread_wait_write : Unix.file_descr -> unit = "thread_wait_write"
external thread_wait_timed_read
            : Unix.file_descr -> float -> resumption_status
            = "thread_wait_timed_read"
external thread_wait_timed_write
            : Unix.file_descr -> float -> resumption_status
            = "thread_wait_timed_write"
external thread_join : t -> unit = "thread_join"
external thread_delay : float -> unit = "thread_delay"
external thread_wait_pid : int -> int * Unix.process_status
            = "thread_wait_pid"
external thread_wakeup : t -> unit = "thread_wakeup"
external thread_self : unit -> t = "thread_self"
external thread_kill : t -> unit = "thread_kill"

external id : t -> int = "thread_id"

(* In sleep() below, we rely on the fact that signals are detected
   only at function applications and beginning of loops,
   making all other operations atomic. *)

let sleep () = critical_section := false; thread_sleep()
let wait_read fd = thread_wait_read fd
let wait_write fd = thread_wait_write fd
let delay duration = thread_delay duration
let join th = thread_join th
let wait_pid pid = thread_wait_pid pid
let wakeup pid = thread_wakeup pid
let self () = thread_self()
let kill pid = thread_kill pid
let exit () = thread_kill(thread_self())

let wait_timed_read_aux fd d = thread_wait_timed_read fd d
let wait_timed_write_aux fd d = thread_wait_timed_write fd d

let wait_timed_read fd d = wait_timed_read_aux fd d = Resumed_io
let wait_timed_write fd d = wait_timed_write_aux fd d = Resumed_io

(* For new, make sure the function passed to thread_new always terminates
   by calling exit. *)

let create fn arg =
  thread_new
    (fun () ->
      try
        Printexc.print fn arg; exit()
      with x ->
        flush stdout; flush stderr; exit())

(* Preemption *)

let preempt signal =
  if !critical_section then () else thread_yield()

(* Initialization of the scheduler *)

let _ =
  Sys.signal Sys.sigvtalrm (Sys.Signal_handle preempt);
  thread_initialize()
