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
  ignore (thread_new g)

type queue = Obj.t list

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

type kval = Start | Go of (unit -> Obj.t) | Ret of Obj.t

type continuation =
  { kmutex : Mutex.t ;
    kcondition : Condition.t ;
    mutable kval : kval }

let kont_create mtx =
  {kmutex = mtx ;
   kcondition = Condition.create () ;
   kval = Start}

(* mutex is locked *)
let kont_suspend k =
  Condition.wait k.kcondition k.kmutex ;
  Mutex.unlock k.kmutex ;
  match k.kval with
  | Go f -> f ()
  | Ret v -> v
  | Start -> assert false

(* Those function are called from compiled code,
   in all cases auto's mutex is locked *)

(* Transfert control to frozen principal thread,
   called from asynchronous sends when one principal exists *)
let kont_go k f =
  k.kval <- Go f ;
  Condition.signal k.kcondition ;
  Mutex.unlock k.kmutex

(* Spawn new process
   called from asynchronous sends when no principal exists *)
let fire_go auto f =
  create_process f ;
  Mutex.unlock auto.mutex

(* Transfert control to frozen principal thread and suspend current thread *)
let kont_go_suspend kme k f =
(* awake principal *)
  k.kval <- Go f ;
  Condition.signal k.kcondition ;
(* suspend current, which will normally be signaled by guarded executed
   on principal *)
  Condition.wait kme.kcondition k.kmutex ;
  Mutex.unlock kme.kmutex ;
  match kme.kval with
  | Ret v -> v
  | Start|Go _ -> assert false

(* Transfer control to current thread
    called when current thread is principal *)
let just_go k f =
  Mutex.unlock k.kmutex ;
  f ()

(* Fire process and suspend
   called from synchronous, when there is no principal name *)
let fire_suspend k auto f =
  create_process f ;
  Condition.wait k.kcondition k.kmutex ;
  Mutex.unlock k.kmutex ;
  match k.kval with
  | Ret v -> v
  | Start|Go _ -> assert false
    

let rec attempt_match auto reactions i =
  if i >= Obj.size reactions then Mutex.unlock auto.mutex
  else begin
    let (ipat, iprim, f) = Obj.magic (Obj.field reactions i) in
    if ipat land auto.status = ipat then
      if iprim < 0 then
        f auto fire_go (* f will unlock auto's mutex *)
      else
        f auto kont_go
    else
      attempt_match auto reactions (i+1)
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

  
let reply_to v k =
  debug "REPLY" (sprintf "%i" (Obj.magic v)) ;
  Mutex.lock k.kmutex ;
  k.kval <- Ret v ;
  Condition.signal k.kcondition ;
  Mutex.unlock k.kmutex 



let rec attempt_match_sync idx kont auto reactions i =
  if i >= Obj.size reactions then
    kont_suspend kont
  else begin
    let (ipat, ipri, f) = Obj.magic (Obj.field reactions i) in
    if ipat land auto.status = ipat then begin
      if ipri < 0 then
        f auto (fire_suspend kont)   (* will create other thread *)
      else if ipri = idx then
        f auto just_go                (* will continue evaluation *)
      else begin
        f auto (kont_go_suspend kont) (* will awake principal thread *)
      end
    end else attempt_match_sync idx kont auto reactions (i+1)
  end

let send_sync auto idx a =
  debug "SEND_SYNC" (sprintf "channel %i" idx) ;
(* Acknowledge new message by altering queue and status *)
  Mutex.lock auto.mutex ;
  let old_status = auto.status in
  let new_status = old_status lor (1 lsl idx) in
  let kont = kont_create auto.mutex in
  put_queue auto idx (Obj.magic (kont,a)) ;
  auto.status <- new_status ;
  if old_status = new_status then begin
    debug "SEND_SYNC" (sprintf "Return: %i" auto.status) ;
    kont_suspend kont
  end else begin
    attempt_match_sync idx kont auto (Obj.magic auto.matches) 0
  end

