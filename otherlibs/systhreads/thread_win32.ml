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

type t

external thread_initialize : unit -> unit = "caml_thread_initialize"
external thread_new : (unit -> unit) -> t = "caml_thread_new"

external yield : unit -> unit = "caml_thread_yield"
external self : unit -> t = "caml_thread_self"
external id : t -> int = "caml_thread_id"
external join : t -> unit = "caml_thread_join"

(* For new, make sure the function passed to thread_new never
   raises an exception. *)

exception Thread_exit

let create fn arg =
  thread_new
    (fun () ->
      try
        fn arg; ()
      with Thread_exit -> ()
         | exn ->
             Printf.eprintf "Uncaught exception in thread %d: %s\n"
                            (id(self())) (Printexc.to_string exn);
             flush stderr)

let exit () = raise Thread_exit

(* Thread.kill is currently not implemented because there is no way
   to do correct cleanup under Win32. *)

let kill th = invalid_arg "Thread.kill: not implemented"

(* Preemption *)

let preempt signal = yield()

(* Initialization of the scheduler *)

let _ =
  Sys.signal 1 (Sys.Signal_handle preempt);
  thread_initialize()

(* Wait functions *)

external delay: float -> unit = "caml_thread_delay"

let wait_read fd = ()
let wait_write fd = ()

let wait_timed_read fd delay = true
let wait_timed_write fd delay = true
let select rd wr ex delay = invalid_arg "Thread.select: not implemented"

let wait_pid p = Unix.waitpid [] p

