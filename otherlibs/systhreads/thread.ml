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

(* User-level threads *)

open ThreadIO

type t

external thread_initialize : unit -> unit = "caml_thread_initialize"
external thread_new : (unit -> unit) -> t = "caml_thread_new"

external yield : unit -> unit = "caml_thread_yield"
external self : unit -> t = "caml_thread_self"
external id : t -> int = "caml_thread_id"
external exit : unit -> unit = "caml_thread_exit"
external join : t -> unit = "caml_thread_join"
external detach : t -> unit = "caml_thread_detach"
external delay: float -> unit = "caml_thread_delay"
external kill : t -> unit = "caml_thread_kill"

(* For new, make sure the function passed to thread_new never
   raises an exception. *)

let create fn arg =
  thread_new
    (fun () ->
      try
        ThreadPrintexc.print fn arg; exit()
      with x ->
        flush stdout; flush stderr; exit())

(* Preemption *)

let preempt signal = yield()

(* Initialization of the scheduler *)

let _ =
  Sys.signal 1 (Sys.Signal_handle preempt);
  thread_initialize()

let wait_read fd = ()
let wait_write fd = ()
let wait_timed_read fd delay = true
let wait_timed_write fd delay = true

let wait_pid p = Unix.waitpid [] p
