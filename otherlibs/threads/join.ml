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

(*DEBUG*)let verbose =
(*DEBUG*)  try int_of_string (Sys.getenv "VERBOSE") with | _ -> 0
(*DEBUG*)
(*DEBUG*)let debug_mutex = Mutex.create ()
(*DEBUG*)
(*DEBUG*)let debug lvl source msg =
(*DEBUG*)  if verbose >= lvl then begin
(*DEBUG*)   Mutex.lock debug_mutex ;
(*DEBUG*)    eprintf "%s[%i]: %s\n" source (Thread.id (Thread.self ())) msg ;
(*DEBUG*)    flush stderr ;
(*DEBUG*)    Mutex.unlock debug_mutex
(*DEBUG*)  end
(*DEBUG*)
(*DEBUG*)let debug1 = debug 1
(*DEBUG*)and debug2 = debug 2
(*DEBUG*)and debug3 = debug 3


(*
   Active tasks.
     A task is active when :
      * running compiled code
      * or in lib code, doomed to become inactive and not to create
        other tasks.

     A task is inactive when :
      * suspended, awaiting synchronous reply
      * simply finished (ie exit_thread is performed)
*)

let active_mutex = Mutex.create ()
and active_condition = Condition.create ()
and active = ref 1
and in_pool = ref 0
(* Number of threads devoted to join *)
(*DEBUG*)and nthreads = ref 1
(*DEBUG*)and suspended = ref 0
(*DEBUG*)and nthreads_mutex = Mutex.create()

let incr_locked r =
  Mutex.lock nthreads_mutex ;
  incr r ;
  Mutex.unlock nthreads_mutex

and decr_locked r =
  Mutex.lock nthreads_mutex ;
  decr r ;
  Mutex.unlock nthreads_mutex


let check_active () =
(*DEBUG*)debug2 "CHECK"
(*DEBUG*) (sprintf "active=%i, nthreads=%i, suspended=%i[%i]"
(*DEBUG*)   !active !nthreads !suspended !in_pool) ;
  if !active <= 0 then Condition.signal active_condition

let become_inactive () =
  Mutex.lock active_mutex ;
  decr active ;
  Mutex.unlock active_mutex ;
 (* if active reaches 0, this cannot change, so we unlock now *)
  check_active () ;

(* incr_active is performed by task creator or awaker *)
and incr_active () =
  Mutex.lock active_mutex ;
  incr active ;
  Mutex.unlock active_mutex

let exit_hook () =
(*DEBUG*)debug1 "EXIT HOOK" "enter" ;
(*DEBUG*)decr_locked nthreads ;
  Mutex.lock active_mutex ;
  decr active ;
  begin if !active > 0 then begin
(*DEBUG*)debug1 "EXIT HOOK" "suspend" ;
    Condition.wait active_condition active_mutex
  end else
    Mutex.unlock active_mutex
  end ;
(*DEBUG*)debug1 "EXIT HOOK" "over" ;
  ()

let _ = at_exit exit_hook

external thread_new : (unit -> unit) -> Thread.t = "caml_thread_new"
external thread_uncaught_exception : exn -> unit = "caml_thread_uncaught_exception"

(*************************************)
(* Real threads creation/destruction *)
(*************************************)


let really_exit_thread () =
  decr_locked nthreads ;
(*DEBUG*)debug1 "REAL EXIT" (sprintf "nthreads=%i" !nthreads);
  Thread.exit ()

(* Note: really_create_process
   uses thread_new, to short-circuit handling of exceptions by Thread *)  

let really_create_process f =
  incr_locked nthreads ;
  try
    let t = Thread.id (thread_new f) in
(*DEBUG*)debug1 "REAL FORK"
(*DEBUG*) (sprintf "%i nthread=%i suspended=%i[%i]"
(*DEBUG*)   t !nthreads !suspended !in_pool) ;
    ignore(t)
  with
  | e ->
      prerr_string "Cannot create process: " ;
      prerr_endline (Printexc.to_string e) ;
      decr_locked nthreads ;
      become_inactive ()
      


(****************)
(* Thread cache *)
(****************)

let pool_condition = Condition.create ()
and pool_mutex = Mutex.create ()
and pool_kont = ref [] 
and pool_size =
  try
    int_of_string (Sys.getenv "POOLSIZE")
  with
  | _ -> 10

let rec do_pool () =
  incr in_pool ;
(*DEBUG*)incr_locked suspended ;
(*DEBUG*)debug2 "POOL SLEEP"
(*DEBUG*)  (sprintf "%i, nthread=%i suspended=%i pool=%i"
(*DEBUG*)     (Thread.id (Thread.self())) !nthreads !suspended !in_pool) ;
  Condition.wait pool_condition pool_mutex ;
(*DEBUG*)decr_locked suspended ;
(*DEBUG*)debug2 "POOL AWAKE" (sprintf "%i" (Thread.id (Thread.self()))) ;
  decr in_pool ;
  match !pool_kont with
  | f::rem ->
      pool_kont := rem ;
(*DEBUG*)debug2 "POOL RUN" (sprintf "%i" (Thread.id (Thread.self()))) ;
      Mutex.unlock pool_mutex ;
      f ()
  | [] ->
      do_pool ()

(* Get a chance to avoid suspending *)
let pool_enter () =
  Mutex.lock pool_mutex ;
  match !pool_kont with
  | f::rem ->
      pool_kont := rem ;
      Mutex.unlock pool_mutex ;
(*DEBUG*)debug2 "POOL FIRST RUN" (sprintf "%i" (Thread.id (Thread.self()))) ;
      f ()
  | [] ->
      do_pool ()
        

let exit_thread () =
(*DEBUG*)debug2 "EXIT THREAD" "" ;
  become_inactive () ;
  if !in_pool >= pool_size then
    really_exit_thread ()
  else
    pool_enter ()

let create_process f =
  incr_active () ;
(* Wapper around f, to be sure to call my exit_thread *)  
  let g () = 
    begin try f ()
    with e ->
      flush stdout; flush stderr;
      thread_uncaught_exception e
    end ;
    exit_thread () in

  if !in_pool = 0 then begin
    really_create_process g
  end else begin
    Mutex.lock pool_mutex ;
    pool_kont := g :: !pool_kont ;
    Condition.signal pool_condition ;
    Mutex.unlock pool_mutex ;
  end

type queue = Obj.t list

type status = int

type automaton = {
  mutable status : status ;
  mutex : Mutex.t ;
  queues : queue array ;
  mutable matches : reaction array ;
} 

and reaction = status * int * Obj.t


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


(* Continuation mutex is automaton mutex *)
let kont_create auto =
  {kmutex = auto.mutex ;
   kcondition = Condition.create () ;
   kval = Start}

(**********************)
(* Asynchronous sends *)
(**********************)

(* Transfert control to frozen principal thread *)
let kont_go k f =
  incr_active () ;
(*DEBUG*)debug2 "KONT_GO" "" ;
  k.kval <- Go f ;
  Condition.signal k.kcondition ;
  Mutex.unlock k.kmutex

(* Spawn new process *)
let fire_go auto f =
(*DEBUG*)debug3 "FIRE_GO" "" ;
  Mutex.unlock auto.mutex ;
  create_process f

and just_go auto f =
(*DEBUG*)debug3 "JUST_GO" "" ;
  Mutex.unlock auto.mutex ;
  f ()


(* Find a match *)
let rec attempt_match tail auto reactions i =
  if i >= Obj.size reactions then Mutex.unlock auto.mutex
  else begin
    let (ipat, iprim, f) = Obj.magic (Obj.field reactions i) in
    if ipat land auto.status = ipat then
      if iprim < 0 then
        f (if tail then just_go else fire_go) (* f will unlock auto's mutex *)
      else
        f kont_go
    else
      attempt_match tail auto reactions (i+1)
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
    attempt_match false auto (Obj.magic auto.matches) 0
  end

and tail_send_async auto idx a =
(*DEBUG*)  debug3 "TAIL_ASYNC" (sprintf "channel %i" idx) ;
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
    attempt_match true auto (Obj.magic auto.matches) 0
  end


(* Optimize forwarders *)
and send_async_alone auto g a =
(*DEBUG*)  debug3 "SEND_ASYNC_ALONE" (sprintf "match %i" g) ;
  let _,_,f = Obj.magic (Obj.field (Obj.magic auto.matches) g) in
  create_process (fun () -> f a)

and tail_send_async_alone auto g a =
(*DEBUG*)  debug3 "TAIL_ASYNC_ALONE" (sprintf "match %i" g) ;
  let _,_,f = Obj.magic (Obj.field (Obj.magic auto.matches) g) in
  f a

(*********************)
(* Synchronous sends *)
(*********************)


(* No match was found *)
let kont_suspend k =
(*DEBUG*)debug2 "KONT_SUSPEND" "" ;
(*DEBUG*)incr_locked suspended ;
  become_inactive () ;
  Condition.wait k.kcondition k.kmutex ;
(*DEBUG*)decr_locked suspended ;
  Mutex.unlock k.kmutex ;
  match k.kval with
  | Go f -> f ()
  | Ret v -> v
  | Start -> assert false


(* Suspend current thread when some match was found *)
let suspend_for_reply k =
(*DEBUG*)incr_locked suspended ;
  become_inactive () ;
  Condition.wait k.kcondition k.kmutex ;
(*DEBUG*)decr_locked suspended ;
  Mutex.unlock k.kmutex ;
  match k.kval with
  | Ret v -> v
  | Start|Go _ -> assert false

(* Transfert control to frozen principal thread and suspend current thread *)
let kont_go_suspend kme kpri f =
(*DEBUG*)debug2 "KONT_GO_SUSPEND" "" ;
(* awake principal *)
  incr_active () ;
  kpri.kval <- Go f ;
  Condition.signal kpri.kcondition ;
  suspend_for_reply kme

let just_go k f =
  Mutex.unlock k.kmutex ;
  f ()

(* Fire process and suspend : no principal name *)
let fire_suspend k auto f =
(*DEBUG*)  debug2 "FIRE_SUSPEND" "" ;
  create_process f ;
  suspend_for_reply k

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
  let kont = kont_create auto in
  put_queue auto idx (Obj.magic (kont,a)) ;
  auto.status <- new_status ;
  if old_status = new_status then begin
(*DEBUG*)    debug3 "SEND_SYNC" (sprintf "Return: %i" auto.status) ;
    kont_suspend kont
  end else begin
    attempt_match_sync idx kont auto (Obj.magic auto.matches) 0
  end

(* Optimize forwarders *)
and send_sync_alone auto g a =
(*DEBUG*)  debug3 "SEND_SYNC_ALONE" (sprintf "match %i" g) ;
  let _,ipri,f = Obj.magic (Obj.field (Obj.magic auto.matches) g) in
  if ipri >= 0 then begin
(*DEBUG*)    debug3 "SEND_SYNC_ALONE" "direct" ;
    f a    
  end else begin
(*DEBUG*)    debug3 "SEND_SYNC_ALONE" "fire" ;
    Mutex.lock auto.mutex ;
    let k = kont_create auto in
    fire_suspend k auto (fun () -> f (k,a))
  end


let reply_to v k =
(*DEBUG*)  debug3 "REPLY" (sprintf "%i" (Obj.magic v)) ;
  Mutex.lock k.kmutex ;
  k.kval <- Ret v ;
  Condition.signal k.kcondition ;
  Mutex.unlock k.kmutex 

