(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
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
external thread_uncaught_exception : exn -> unit = 
            "caml_thread_uncaught_exception"

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
             flush stdout; flush stderr;
             thread_uncaught_exception exn)

let exit () = raise Thread_exit

(* Thread.kill is currently not implemented because there is no way
   to do correct cleanup under Win32. *)

let kill th = invalid_arg "Thread.kill: not implemented"

(* Preemption *)

let preempt signal = yield()

(* Initialization of the scheduler *)

let _ =
  ignore(Sys.signal Sys.sigterm (Sys.Signal_handle preempt));
  thread_initialize()

(* Wait functions *)

external delay: float -> unit = "caml_thread_delay"

let wait_read fd = ()
let wait_write fd = ()

let wait_timed_read fd delay = true
let wait_timed_write fd delay = true
let select rd wr ex delay = invalid_arg "Thread.select: not implemented"

let wait_pid p = Unix.waitpid [] p

let sigmask cmd set = invalid_arg "Thread.sigmask: not implemented"
let wait_signal set = invalid_arg "Thread.wait_signal: not implemented"

