(***********************************************************************)
(*                                                                     *)
(*                         Objective Caml                              *)
(*                                                                     *)
(*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
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

(* Thread.kill is currently not implemented due to problems with
   cleanup handlers on several platforms *)

let kill th = invalid_arg "Thread.kill: not implemented"

(* Preemption *)

let preempt signal = yield()

(* Initialization of the scheduler *)

let _ =
  Sys.signal Sys.sigvtalrm (Sys.Signal_handle preempt);
  thread_initialize()

(* Wait functions *)

let delay time = Unix.select [] [] [] time; ()

let wait_read fd = ()
let wait_write fd = ()

let wait_timed_read fd d =
  match Unix.select [fd] [] [] d with ([], _, _) -> false | (_, _, _) -> true
let wait_timed_write fd d =
  match Unix.select [] [fd] [] d with (_, [], _) -> false | (_, _, _) -> true
let select = Unix.select

let wait_pid p = Unix.waitpid [] p

let wait_signal sigs =
  let gotsig = ref 0 in
  let sem = Semaphore.create 0 in
  let sighandler s = gotsig := s; Semaphore.post sem in
  let oldhdlrs =
    List.map (fun s -> Sys.signal s (Sys.Signal_handle sighandler)) sigs in
  Semaphore.wait sem;
  List.iter2 (fun s act -> Sys.signal s act; ()) sigs oldhdlrs;
  !gotsig
