(***********************************************************************)
(*                                                                     *)
(*                         Objective Caml                              *)
(*                                                                     *)
(*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
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
external thread_uncaught_exception : exn -> unit = 
            "caml_thread_uncaught_exception"

external yield : unit -> unit = "caml_thread_yield"
external self : unit -> t = "caml_thread_self"
external id : t -> int = "caml_thread_id"
external join : t -> unit = "caml_thread_join"
external exit : unit -> unit = "caml_thread_exit"

(* For new, make sure the function passed to thread_new never
   raises an exception. *)

let create fn arg =
  thread_new
    (fun () ->
      try
        fn arg; ()
      with exn ->
             flush stdout; flush stderr;
             thread_uncaught_exception exn)

(* Thread.kill is currently not implemented due to problems with
   cleanup handlers on several platforms *)

let kill th = invalid_arg "Thread.kill: not implemented"

(* Preemption *)

let preempt signal = yield()

(* Initialization of the scheduler *)

let _ =
  ignore(Sys.signal Sys.sigvtalrm (Sys.Signal_handle preempt));
  thread_initialize()

(* Wait functions *)

let delay time = ignore(Unix.select [] [] [] time)

let wait_read fd = ()
let wait_write fd = ()

let wait_timed_read fd d =
  match Unix.select [fd] [] [] d with ([], _, _) -> false | (_, _, _) -> true
let wait_timed_write fd d =
  match Unix.select [] [fd] [] d with (_, [], _) -> false | (_, _, _) -> true
let select = Unix.select

let wait_pid p = Unix.waitpid [] p

external wait_signal : int list -> int = "caml_wait_signal"
