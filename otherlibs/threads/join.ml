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

(* DEBUG *)let verbose =
(* DEBUG *)  try int_of_string (Sys.getenv "VERBOSE") with | _ -> 0

(*DEBUG*)let debug_mutex = Mutex.create ()

(*DEBUG*)let debug lvl source msg =
(*DEBUG*)  if verbose >= lvl then begin
(*DEBUG*)   Mutex.lock debug_mutex ;
(*DEBUG*)    eprintf "%s: %s\n" source msg ;
(*DEBUG*)    flush stderr ;
(*DEBUG*)    Mutex.unlock debug_mutex
(*DEBUG*)  end
(*DEBUG*)
(*DEBUG*)let debug1 = debug 1
(*DEBUG*)and debug2 = debug 2
(*DEBUG*)and debug3 = debug 3

let nthreads_mutex = Mutex.create ()
and nthreads_condition = Condition.create ()
and nthreads = ref 1 (* Real threads devoted to join *)
and in_pool = ref 0  (* Count of such waiting in pool *)
and suspended = ref 0 (* Count of such being suspended *)
and pool_kont = ref [] (* work available for pool processes *)

let incr_locked r =
  Mutex.lock nthreads_mutex ;
  incr r ;
  Mutex.unlock nthreads_mutex

and decr_locked r =
  Mutex.lock nthreads_mutex ;
  decr r ;
  Mutex.unlock nthreads_mutex

let something_running () =
  let r =
    !nthreads > !in_pool + !suspended || !pool_kont <> [] in
(*DEBUG*)debug2 "SOMETHING" (sprintf "%b" r) ;
  r

let check_something () =
  if not (something_running ()) then
    Condition.signal nthreads_condition

let exit_hook () =
  Mutex.lock nthreads_mutex ;
  decr nthreads ;
  begin if something_running () then
    Condition.wait nthreads_condition nthreads_mutex
  else
    Mutex.unlock nthreads_mutex
  end ;
(*DEBUG*)  debug1 "EXIT HOOK" "over" ;
  ()

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
(*DEBUG*)debug1 "REAL EXIT"
(*DEBUG*)    (sprintf "%i nthread=%i pool=%i"
(*DEBUG*)      (Thread.id (Thread.self ())) !nthreads !in_pool) ;
  if not (something_running ()) then
    Condition.signal nthreads_condition ;
  Mutex.unlock nthreads_mutex ;
  Thread.exit ()

(* Note: really_create_process
   uses thread_new, to short-circuit handling of exceptions by Thread *)  

let really_create_process f =
  incr_locked nthreads ;
  try
    let t = Thread.id (thread_new f) in
(*DEBUG*)debug1 "REAL FORK"
(*DEBUG*) (sprintf "%i nthread=%i suspended=%i pool=%i"
(*DEBUG*)   t !nthreads !suspended !in_pool) in
    ignore(t)
  with
  | e ->
      prerr_string "Cannot create process: " ;
      prerr_endline (Printexc.to_string e) ;
      decr_locked nthreads ;
      if not (something_running ()) then
        Condition.signal nthreads_condition
      


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
(*DEBUG*)debug2 "POOL RUN" (sprintf "%i" (Thread.id (Thread.self()))) ;
      Mutex.unlock nthreads_mutex ;
      f ()
  | [] ->
      incr in_pool ;
      if not (something_running ()) then Condition.signal nthreads_condition ;
(*DEBUG*)      debug2 "POOL SLEEP"
(*DEBUG*)        (sprintf "%i, nthread=%i suspended=%i pool=%i"
(*DEBUG*)           (Thread.id (Thread.self())) !nthreads !suspended !in_pool) ;
      Condition.wait pool_condition nthreads_mutex ;
(*DEBUG*)      debug2 "POOL AWAKE" (sprintf "%i" (Thread.id (Thread.self()))) ;
      decr in_pool ;
      do_pool_enter ()

let pool_enter () =
  Mutex.lock nthreads_mutex ;
(*DEBUG*)  debug2 "POOL ENTER" (sprintf "%i" (Thread.id (Thread.self()))) ;
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
(*DEBUG*)  debug1 "KONT_SUSPEND" (sprintf "%i" (Thread.id (Thread.self ()))) ;
  incr_locked suspended ;
  check_something () ;
  Condition.wait k.kcondition k.kmutex ;
  decr_locked suspended ;
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
  Mutex.unlock auto.mutex ;
  create_process f


(* Transfert control to frozen principal thread and suspend current thread *)
let kont_go_suspend kme k f =
(* awake principal *)
  k.kval <- Go f ;
  Condition.signal k.kcondition ;
(* suspend current, which will normally be signaled by guarded executed
   on principal *)
(*DEBUG*)debug1 "KONT_GO_SUSPEND" (sprintf "%i" (Thread.id (Thread.self ()))) ;
  incr_locked suspended ;
  check_something () ;
  Condition.wait kme.kcondition k.kmutex ;
  decr_locked suspended ;
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
(*DEBUG*)  debug1 "FIRE_SUSPEND" (sprintf "%i" (Thread.id (Thread.self ()))) ;
  incr_locked suspended ;
  check_something () ;
  Condition.wait k.kcondition k.kmutex ;
  decr_locked suspended ;
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
        f fire_go (* f will unlock auto's mutex *)
      else
        f kont_go
    else
      attempt_match auto reactions (i+1)
  end

let send_async auto idx a =
(*DEBUG*)  debug3 "SEND_ASYNC" (sprintf "channel %i" idx) ;
(* Acknowledge new message by altering queue and status *)
  Mutex.lock auto.mutex ;
  let old_status = auto.status in
  let new_status = old_status lor (1 lsl idx) in
  put_queue auto idx a ;
  auto.status <- new_status ;
  if old_status = new_status then begin
(*DEBUG*)    debug3 "SEND_ASYNC" (sprintf "Return: %i" auto.status) ;
    Mutex.unlock auto.mutex
  end else begin
    attempt_match auto (Obj.magic auto.matches) 0
  end

and send_async_alone auto g a =
(*DEBUG*)  debug3 "SEND_ASYNC_ALONE" (sprintf "match %i" g) ;
  let _,_,f = Obj.magic (Obj.field (Obj.magic auto.matches) g) in
  create_process (fun () -> f a)
  
let reply_to v k =
(*DEBUG*)  debug3 "REPLY" (sprintf "%i" (Obj.magic v)) ;
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
        f (fire_suspend kont)   (* will create other thread *)
      else if ipri = idx then
        f just_go               (* will continue evaluation *)
      else begin
        f (kont_go_suspend kont) (* will awake principal thread *)
      end
    end else attempt_match_sync idx kont auto reactions (i+1)
  end

let send_sync auto idx a =
(*DEBUG*)  debug3 "SEND_SYNC" (sprintf "channel %i" idx) ;
(* Acknowledge new message by altering queue and status *)
  Mutex.lock auto.mutex ;
  let old_status = auto.status in
  let new_status = old_status lor (1 lsl idx) in
  let kont = kont_create auto.mutex in
  put_queue auto idx (Obj.magic (kont,a)) ;
  auto.status <- new_status ;
  if old_status = new_status then begin
(*DEBUG*)    debug3 "SEND_SYNC" (sprintf "Return: %i" auto.status) ;
    kont_suspend kont
  end else begin
    attempt_match_sync idx kont auto (Obj.magic auto.matches) 0
  end

and send_sync_alone auto g a =
(*DEBUG*)  debug3 "SEND_SYNC_ALONE" (sprintf "match %i" g) ;
  let _,ipri,f = Obj.magic (Obj.field (Obj.magic auto.matches) g) in
  if ipri >= 0 then begin
(*DEBUG*)    debug3 "SEND_SYNC_ALONE" "direct" ;
    f a    
  end else begin
(*DEBUG*)    debug3 "SEND_SYNC_ALONE" "fire" ;
    Mutex.lock auto.mutex ;
    let k = kont_create auto.mutex in
    fire_suspend k auto (fun () -> f (k,a))
  end
