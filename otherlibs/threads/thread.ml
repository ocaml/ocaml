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

external thread_initialize : unit -> unit = "csl_thread_initialize"
external thread_new : (unit -> unit) -> t = "csl_thread_new"

external yield : unit -> unit = "csl_thread_yield"
external self : unit -> t = "csl_thread_self"
external id : t -> int = "csl_thread_id"
external exit : unit -> unit = "csl_thread_exit"
external join : t -> unit = "csl_thread_join"
external detach : t -> unit = "csl_thread_detach"

(* For new, make sure the function passed to thread_new never
   raises an exception. *)

let new fn arg =
  thread_new
    (fun () ->
      try
        Printexc.print fn arg; ()
      with x ->
        flush stdout; flush stderr)

(* Preemption *)

let preempt signal = yield()

(* Initialization of the scheduler *)

let _ =
  Sys.signal Sys.sigvtalrm (Sys.Signal_handle preempt);
  thread_initialize()
