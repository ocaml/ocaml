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


let verbose =
  try
    int_of_string (Sys.getenv "VERBOSE")
  with
  | _ -> 0


let debug_mutex = Mutex.create ()

let debug lvl source msg =
  if verbose >= lvl then begin
    Mutex.lock debug_mutex ;
    eprintf "%s: %s\n" source msg ;
    flush stderr ;
    Mutex.unlock debug_mutex
  end

let debug1 = debug 1
and debug2 = debug 2
and debug3 = debug 3

let nthreads_mutex = Mutex.create ()
and nthreads_condition = Condition.create ()
and nthreads = ref 1 (* Real threads devoted to join *)
and in_pool = ref 0  (* Count of such waiting in pool *)
and pool_kont = ref [] (* work available for pool processes *)

let exit_hook () =
  Mutex.lock nthreads_mutex ;
  decr nthreads ;
  if !nthreads > !in_pool then
    Condition.wait nthreads_condition nthreads_mutex
  else
    Mutex.unlock nthreads_mutex

let _ = at_exit exit_hook

external thread_new : (unit -> unit) -> Thread.t = "caml_thread_new"
external thread_uncaught_exception : exn -> unit = "caml_thread_uncaught_exception"

(***************)
(* Thread pool *)
(***************)


(* To be called whenever a join-thread exits *)
let really_exit_thread () =
  Mutex.lock nthreads_mutex ;
  decr nthreads ;
  debug1 "REAL EXIT"
    (sprintf "%i nthread=%i pool=%i"
       (Thread.id (Thread.self ()))
       !nthreads !in_pool) ;
  if !nthreads <= !in_pool && !pool_kont = [] then
    Condition.signal nthreads_condition ;
  Mutex.unlock nthreads_mutex ;
  Thread.exit ()

(* Note: really_create_process
   uses thread_new, to short-circuit handling of exceptions by Thread *)  

let really_create_process f =
  Mutex.lock nthreads_mutex ;
  incr nthreads ;
  Mutex.unlock nthreads_mutex ;
  let t = Thread.id (thread_new f) in
  debug1 "REAL FORK"
    (sprintf "%i nthread=%i pool=%i" t !nthreads !in_pool)



(****************)
(* Thread cache *)
(****************)

let pool_condition = Condition.create ()
and pool_size =
  try
    int_of_string (Sys.getenv "POOLSIZE")
  with
  | _ -> 10

(* nthreads_mutex  is now locked *)
let rec do_pool_enter () =
  match !pool_kont with
  | f::rem ->
      pool_kont := rem ;
      debug2 "POOL RUN" (sprintf "%i" (Thread.id (Thread.self()))) ;
      Mutex.unlock nthreads_mutex ;
      f ()
  | [] ->
      incr in_pool ;
      if !nthreads <= !in_pool then Condition.signal nthreads_condition ;
      debug2 "POOL SLEEP" (sprintf "%i" (Thread.id (Thread.self()))) ;
      Condition.wait pool_condition nthreads_mutex ;
      debug2 "POOL AWAKE" (sprintf "%i" (Thread.id (Thread.self()))) ;
      decr in_pool ;
      do_pool_enter ()

let pool_enter () =
  Mutex.lock nthreads_mutex ;
  debug2 "POOL ENTER" (sprintf "%i" (Thread.id (Thread.self()))) ;
  do_pool_enter ()

let exit_thread () =
  if !in_pool >= pool_size then
    really_exit_thread ()
  else
    pool_enter ()

let create_process f =
(* Wapper around f, to be sure to call my exit_thread *)  
  let g () = 
    begin try f ()
    with e ->
        flush stdout; flush stderr;
        thread_uncaught_exception e
    end ;
    exit_thread () in

  Mutex.lock nthreads_mutex ;
  if !in_pool = 0 then begin
    Mutex.unlock nthreads_mutex ;
    really_create_process g
  end else begin
    pool_kont := g :: !pool_kont ;
    Condition.signal pool_condition ;
    Mutex.unlock nthreads_mutex
  end

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
  debug3 "SEND_ASYNC" (sprintf "channel %i" idx) ;
(* Acknowledge new message by altering queue and status *)
  Mutex.lock auto.mutex ;
  let old_status = auto.status in
  let new_status = old_status lor (1 lsl idx) in
  put_queue auto idx a ;
  auto.status <- new_status ;
  if old_status = new_status then begin
    debug3 "SEND_ASYNC" (sprintf "Return: %i" auto.status) ;
    Mutex.unlock auto.mutex
  end else begin
    attempt_match auto (Obj.magic auto.matches) 0
  end

  
let reply_to v k =
  debug3 "REPLY" (sprintf "%i" (Obj.magic v)) ;
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
  debug3 "SEND_SYNC" (sprintf "channel %i" idx) ;
(* Acknowledge new message by altering queue and status *)
  Mutex.lock auto.mutex ;
  let old_status = auto.status in
  let new_status = old_status lor (1 lsl idx) in
  let kont = kont_create auto.mutex in
  put_queue auto idx (Obj.magic (kont,a)) ;
  auto.status <- new_status ;
  if old_status = new_status then begin
    debug3 "SEND_SYNC" (sprintf "Return: %i" auto.status) ;
    kont_suspend kont
  end else begin
    attempt_match_sync idx kont auto (Obj.magic auto.matches) 0
  end

