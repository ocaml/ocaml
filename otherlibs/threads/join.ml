(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Printf

let verbose = true

let debug source msg =
  if verbose then begin
    eprintf "%s: %s\n" source msg ;
    flush stderr
  end

let nthreads_mutex = Mutex.create ()
and nthreads_condition = Condition.create ()
and nthreads = ref 1

let exit_hook () =
  Mutex.lock nthreads_mutex ;
  decr nthreads ;
  let r = !nthreads in
  Mutex.unlock nthreads_mutex ;  
  if r > 0 then
    Condition.wait nthreads_condition nthreads_mutex

let _ = at_exit exit_hook

external thread_new : (unit -> unit) -> Thread.t = "thread_new"
external thread_uncaught_exception : exn -> unit =
  "thread_uncaught_exception"

(* To be called whenever a join-thread exits *)
let exit_thread () =
  Mutex.lock nthreads_mutex ;
  decr nthreads ;
  if !nthreads = 0 then Condition.signal nthreads_condition ;
  Mutex.unlock nthreads_mutex ;
  Thread.exit ()
  
let create_process f =
(* One new join thread *)
  Mutex.lock nthreads_mutex ;
  incr nthreads ;
  Mutex.unlock nthreads_mutex ;

(* Be sure to call my exit_thread *)
  let g () = 
    try f () ; exit_thread ()
    with e ->
      flush stdout; flush stderr;
      thread_uncaught_exception e;
      exit_thread () in

(* use thread_new, to short-circuit handling of exceptions by Thread *)
  ignore (thread_new g) ;
  

type argument

type queue = argument list

type status = int

type automaton = {
  mutable status : status ;
  mutex : Mutex.t ;
  queues : queue array ;
  mutable matches : reaction array ;
} 

and reaction = status * (automaton -> unit)


let put_queue auto idx a =
  auto.queues.(idx) <- a :: auto.queues.(idx)

let get_queue auto idx = match auto.queues.(idx) with
| [] -> assert false
| a::rem ->
    auto.queues.(idx) <- rem ;
    begin match rem with
    | [] -> auto.status <- auto.status land (lnot (1 lsl idx))
    | _  -> ()
    end ;
    a

let id x = ()

let create_automaton nchans nmatches =
  {
    status = 0 ;
    mutex = Mutex.create () ;
    queues = Array.create nchans [] ;
    matches = [| |] ;
  } 


let patch_table auto t = auto.matches <- t

(* Can be called by compiled code, just before firing guarded process *)
let unlock_automaton a = Mutex.unlock a.mutex

let rec attempt_match auto reactions i =
  if i >= Obj.size reactions then Mutex.unlock auto.mutex
  else begin
    let (ipat, f) = Obj.magic (Obj.field reactions i) in
    if ipat=auto.status then f auto (* f will unlock auto.mutex *)
    else attempt_match auto reactions (i+1)
  end

let send_async auto idx a =
  debug "SEND_ASYNC" (sprintf "channel %i" idx) ;
(* Acknowledge new message by altering queue and status *)
  Mutex.lock auto.mutex ;
  let old_status = auto.status in
  let new_status = old_status lor (1 lsl idx) in
  put_queue auto idx a ;
  auto.status <- new_status ;
  if old_status = new_status then begin
    debug "SEND_ASYNC" (sprintf "Return: %i" auto.status) ;
    Mutex.unlock auto.mutex
  end else begin
    attempt_match auto (Obj.magic auto.matches) 0
  end

let send_sync auto idx a = failwith "not_implemented"
  
