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

open Join_types
open Printf
open Join_debug

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
and signaled = ref 0
and pool_konts = ref 0
(* Number of threads devoted to join *)
and nthreads = ref 1
and suspended = ref 0

let nthreads_mutex = Mutex.create()

let incr_locked m r =
  Mutex.lock m ;
  incr r ;
  Mutex.unlock m

and decr_locked m r =
  Mutex.lock m ;
  decr r ;
  Mutex.unlock m

let tasks_status () =
  sprintf
    "act=%i, nth=%i sus=%i pool[in=%i, k=%i, sig=%i]"
    !active !nthreads !suspended !in_pool !pool_konts !signaled

let become_inactive () =
  decr_locked nthreads_mutex active ;
 (* if active reaches 0, this cannot change, so we unlock now *)
(*DEBUG*)debug2 "CHECK" "%s" (tasks_status ()) ;
  if !active <= 0 then begin
    Mutex.lock active_mutex ;
    Condition.signal active_condition ;
    Mutex.unlock active_mutex
  end

(* incr_active is performed by task creator or awaker *)
and incr_active () = incr_locked  nthreads_mutex active


(*************************************)
(* Real threads creation/destruction *)
(*************************************)



let pool_size =
  try
    int_of_string (Sys.getenv "JOPOOLSIZE")
  with
  | _ -> 10

and runmax =
   try
    Some (int_of_string (Sys.getenv "JORUNMAX"))
  with
  | _ -> None

let really_exit_thread () =
  decr_locked nthreads_mutex nthreads ;
(*DEBUG*)debug1 "REAL EXIT" "nthreads=%i" !nthreads ;
  Thread.exit ()

(* Note: really_create_process
   uses thread_new, to short-circuit handling of exceptions by Thread *)  

exception MaxRun

let n_msgs = ref 0

let really_create_process f =
  incr_locked nthreads_mutex nthreads ;
  try
    begin match runmax with
    | Some k when !nthreads > k -> raise MaxRun
    | _ -> ()
    end ;
    let t = Join_extern.thread_new f in
(*DEBUG*)debug1 "REAL FORK" "%i %s" (Thread.id t) (tasks_status ()) ;
    Some t
  with
  | e ->
(*DEBUG*)debug0 "REAL FORK FAILED"
(*DEBUG*)  "%s, %s" (tasks_status ()) (Printexc.to_string e) ;
      if !n_msgs <= 0 then begin
	debug "Warning" "Threads are exhausted, deadlock may occur" ;
	n_msgs := 100
      end else decr n_msgs ;
      decr_locked nthreads_mutex nthreads ;
      None
      


(****************)
(* Thread cache *)
(****************)

let pool_condition = Condition.create ()
and pool_mutex = Mutex.create ()
and pool_kont = ref [] 

(* Create actuall threads for all tasks in pool *)
let rec fork_for_pool () = match !pool_kont with
| f::rem ->
    pool_kont := rem ; decr pool_konts ;
    Mutex.unlock pool_mutex ;
    if really_create_process f = None then begin
      Mutex.lock pool_mutex ;
      pool_kont := f :: !pool_kont ; incr pool_konts ;
      Mutex.unlock pool_mutex ;
(*DEBUG*)debug1 "FORK" "DELAYED" ;
    end else if rem <> [] && !in_pool = 0 then begin
      Mutex.lock pool_mutex ;
      fork_for_pool ()
    end
| [] ->
    Mutex.unlock pool_mutex


let rec do_pool () =
  incr in_pool ;
(*DEBUG*)incr_locked nthreads_mutex suspended ;
(*DEBUG*)debug2 "POOL SLEEP" "%s" (tasks_status ()) ;
  Condition.wait pool_condition pool_mutex ;
(*DEBUG*)decr_locked nthreads_mutex suspended ;
(*DEBUG*)debug2 "POOL AWAKE" "%s" (tasks_status ()) ;
  decr in_pool ; decr signaled ;
  match !pool_kont with
  | f::rem ->
      pool_kont := rem ; decr pool_konts ;
(*DEBUG*)debug2 "POOL RUN" "%i" (Thread.id (Thread.self())) ;
      Mutex.unlock pool_mutex ;
      f ()
  | [] -> do_pool ()

(* Get a chance to avoid suspending *)
let pool_enter () =
  Mutex.lock pool_mutex ;
  match !pool_kont with
  | f::rem ->
      pool_kont := rem ; decr pool_konts ;
      Mutex.unlock pool_mutex ;
(*DEBUG*)debug2 "POOL FIRST RUN" "%i" (Thread.id (Thread.self())) ;
      f ()
  | [] ->
      do_pool ()

let grab_from_pool () =
  Mutex.lock pool_mutex ;
  if  !signaled < !pool_konts && !signaled < !in_pool then begin
    Condition.signal pool_condition ;
    incr signaled ;
    Mutex.unlock pool_mutex
  end else
    fork_for_pool ()

let exit_thread () =
(*DEBUG*)debug2 "EXIT THREAD" "%s" (tasks_status ()) ;
  become_inactive () ;
  if
    ((!in_pool - !signaled >= pool_size || !in_pool > pool_size) && !active > !pool_konts)
  then
    really_exit_thread ()
  else 
    pool_enter ()

let put_pool_locked  f =
  pool_kont := f :: !pool_kont ; incr pool_konts ;
  if !signaled < !pool_konts && !signaled < !in_pool then begin
    Condition.signal pool_condition ;
    incr signaled
  end ;
  (*DEBUG*)debug2 "PUT POOL" "%s" (tasks_status ()) ;
  Mutex.unlock pool_mutex

let put_pool f =
  Mutex.lock pool_mutex ;
  put_pool_locked f


let create_process f =
(*DEBUG*)debug2 "CREATE_PROCESS" "%s" (tasks_status ()) ;
  incr_active () ;
(* Wapper around f, to be sure to call my exit_thread *)  
  let g () = 
    begin try f ()
    with
    | Join_misc.JoinExit ->
(* technique to silentely suicide join-managed threads *)
(*DEBUG*)debug2 "PROCESS OVER" "%s" "by JoinExit" ;
      flush stdout; flush stderr
    | e ->
(*DEBUG*)debug2 "PROCESS OVER" "%s" "by exception" ;
      flush stdout; flush stderr;
      Join_extern.thread_uncaught_exception e
    end ;
    exit_thread () in

  Mutex.lock pool_mutex ;
  let pks = !pool_konts and nsigs = !signaled and inp = !in_pool in
  let not_signaled = inp - nsigs in
  let extra_signals =  min inp nsigs - pks in
  if not_signaled > 0 || extra_signals > 0 then begin
  (* In that case there is a thread waiting for me in the pool *)
    put_pool_locked g 
  end else begin
    Mutex.unlock pool_mutex ;
    match really_create_process g with
    | None -> (* Thread creation failed *) put_pool g
    | Some _ -> ()
  end
  
let inform_suspend () =
(*DEBUG*)incr_locked nthreads_mutex suspended ;
  become_inactive () ;
  if !pool_konts > 0 then grab_from_pool ()

and inform_unsuspend () =
(*DEBUG*)decr_locked nthreads_mutex suspended ;
  ()


let kont_create mutex =
  {kmutex = mutex ;
   kcondition = Condition.create () ;
   kval = Start}


(* Important: k.kmutex is locked ! *)
let rec suspend_for_reply k =
(*DEBUG*)debug3 "SUSPEND_FOR_REPLY"
(*DEBUG*)  "%s" (tasks_status ()) ;  
  match k.kval with
  | Start ->
      begin
        inform_suspend () ;
        Condition.wait k.kcondition k.kmutex ;
        inform_unsuspend () ;
        match k.kval with
        | Ret v ->
            Mutex.unlock k.kmutex ;
(*DEBUG*)debug3 "REPLIED" "%s" (tasks_status ()) ;
            (Obj.obj v)
        | Exn e ->
            Mutex.unlock k.kmutex ;
(*DEBUG*)debug3 "REPLIED EXN" "%s" (tasks_status ()) ;
            raise e
        | Go f ->
            Mutex.unlock k.kmutex ;
(*DEBUG*)debug3 "REACTIVATED" "%s" (tasks_status ()) ;
            Obj.obj (f ())
        | Start -> (* should not happen when properly signaled *)
(*DEBUG*)debug0 "SPONTANEOUS REPLY" "%s" (tasks_status ()) ;
            suspend_for_reply k
      end
  | Go f ->
      Mutex.unlock k.kmutex ;
(*DEBUG*)debug3 "REACTIVATED IMMEDIATE" "%s" (tasks_status ()) ;
      Obj.obj (f ())
  | Ret v ->
      Mutex.unlock k.kmutex ;
(*DEBUG*)debug3 "REPLIED IMMEDIATE" "%s" (tasks_status ()) ;
      (Obj.obj v)
  | Exn e ->
      Mutex.unlock k.kmutex ;
(*DEBUG*)debug3 "REPLIED EXN IMMEDIATE" "%s" (tasks_status ()) ;
      raise e

let reply_to v k = 
(*DEBUG*)debug3 "REPLY" "%i" (Obj.magic v : int) ;
  Mutex.lock k.kmutex ;
  assert (k.kval = Start) ;
  k.kval <- Ret (Obj.repr v) ;
  incr_active () ; (* The awaken task becomes active *)
  Condition.signal k.kcondition ;
  Mutex.unlock k.kmutex

let reply_to_exn e k = 
(*DEBUG*)debug3 "REPLY EXN"
(*DEBUG*) "%s" (Join_misc.exn_to_string e) ;
  Mutex.lock k.kmutex ;
  match k.kval with
  | Start ->
      k.kval <- Exn e ;
      incr_active () ; (* The awaken task becomes active *)
      Condition.signal k.kcondition ;
      Mutex.unlock k.kmutex
  | Exn _| Go _|Ret _ -> assert false

(********************************)
(* Management of initial thread *)
(********************************)


(* Called when all active tasks are waiting in thread pool *)
let from_pool () =
  if !in_pool > 0 then begin
(*DEBUG*)debug1 "HOOK" "%s" "SHOULD PERPHAPS SIGNAL" ;    
(*    Condition.signal pool_condition  *) ()
  end else begin (* Create a new thread to enter pool *)
(*DEBUG*)debug1 "HOOK" "%s" "CREATE" ;    
    incr_active () ;
    let b = really_create_process exit_thread in
(*DEBUG*)debug1 "HOOK" "%s" (if b <> None then "PROCESS CREATED" else "FAILED");
    if b=None then begin
      prerr_endline "Threads are exhausted, good bye !"
    end

  end

let exit_hook () =
(*DEBUG*)debug1 "HOOK" "enter" ;
(*DEBUG*)decr_locked nthreads_mutex nthreads ;
  Mutex.lock active_mutex ;
  decr active ;
  begin if !active > 0 then begin
    if !pool_konts = !active then begin
      Mutex.unlock active_mutex ;
      from_pool ()
    end ;
(*DEBUG*)debug1 "HOOK" "suspend %s" (tasks_status ()) ;
    Condition.wait active_condition active_mutex
  end else
    Mutex.unlock active_mutex
  end ;
(*DEBUG*)debug1 "HOOK" "over" ;
  ()
