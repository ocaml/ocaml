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
(*DEBUG*)open Join_debug

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
and pool_konts = ref 0
(* Number of threads devoted to join *)
(*DEBUG*)and nthreads = ref 1
(*DEBUG*)and suspended = ref 0

let nthreads_mutex = Mutex.create()

let incr_locked m r =
  Mutex.lock m ;
  incr r ;
  Mutex.unlock m

and decr_locked m r =
  Mutex.lock m ;
  decr r ;
  Mutex.unlock m

(*DEBUG*)let tasks_status () =
(*DEBUG*)sprintf "active=%i, nthread=%i suspended=%i[%i, %i]"
(*DEBUG*) !active !nthreads !suspended !in_pool !pool_konts

let check_active () =
(*DEBUG*)debug2 "CHECK" (tasks_status ()) ;
  if !active <= 0 then Condition.signal active_condition

let become_inactive () =
  decr_locked nthreads_mutex active ;
 (* if active reaches 0, this cannot change, so we unlock now *)
  check_active () ;

(* incr_active is performed by task creator or awaker *)
and incr_active () = incr_locked  nthreads_mutex active


(*************************************)
(* Real threads creation/destruction *)
(*************************************)

external thread_new : (unit -> unit) -> Thread.t = "caml_thread_new"
external thread_uncaught_exception : exn -> unit = "caml_thread_uncaught_exception"


let pool_size =
  try
    int_of_string (Sys.getenv "POOLSIZE")
  with
  | _ -> 10
and runmax =
   try
    Some (int_of_string (Sys.getenv "RUNMAX"))
  with
  | _ -> None


let really_exit_thread () =
  decr_locked nthreads_mutex nthreads ;
(*DEBUG*)debug1 "REAL EXIT" (sprintf "nthreads=%i" !nthreads);
  Thread.exit ()

(* Note: really_create_process
   uses thread_new, to short-circuit handling of exceptions by Thread *)  

exception MaxRun

let really_create_process f =
  incr_locked nthreads_mutex nthreads ;
  try
    begin match runmax with
    | Some k when !nthreads - !suspended > k -> raise MaxRun
    | _ -> ()
    end ;
    let t = Thread.id (thread_new f) in
(*DEBUG*)debug1 "REAL FORK" (sprintf "%i %s" t (tasks_status ())) ;
    ignore(t) ;
    true
  with
  | e ->
(*DEBUG*)debug2 "REAL FORK FAILED"
(*DEBUG*)  (sprintf "%s, %s" (tasks_status ()) (Printexc.to_string e)) ;
      decr_locked nthreads_mutex nthreads ;
      false
      


(****************)
(* Thread cache *)
(****************)

let pool_condition = Condition.create ()
and pool_mutex = Mutex.create ()
and pool_kont = ref [] 

let rec do_pool () =
  incr in_pool ;
(*DEBUG*)incr_locked nthreads_mutex suspended ;
(*DEBUG*)debug2 "POOL SLEEP" (tasks_status ()) ;
  Condition.wait pool_condition pool_mutex ;
(*DEBUG*)decr_locked nthreads_mutex suspended ;
(*DEBUG*)debug2 "POOL AWAKE" (tasks_status ()) ;
  decr in_pool ;
  match !pool_kont with
  | f::rem ->
      pool_kont := rem ; decr pool_konts ;
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
      pool_kont := rem ; decr pool_konts ;
      Mutex.unlock pool_mutex ;
(*DEBUG*)debug2 "POOL FIRST RUN" (sprintf "%i" (Thread.id (Thread.self()))) ;
      f ()
  | [] ->
      do_pool ()

let rec grab_from_pool delay =
  Mutex.lock pool_mutex ;
  if !in_pool > 0 then begin
    Condition.signal pool_condition ;
    Mutex.unlock pool_mutex
  end else match !pool_kont with
  | f::rem ->
      pool_kont := rem ; decr pool_konts ;
      Mutex.unlock pool_mutex ;
      if not (really_create_process f) then begin
        Mutex.lock pool_mutex ;
        pool_kont := f :: !pool_kont ; incr pool_konts ;
        Mutex.unlock pool_mutex ;
        prerr_endline "Threads exhausted" ;
        Thread.delay delay ;
        grab_from_pool (1.0 +. delay)
      end
  | [] ->
      Mutex.unlock pool_mutex

let exit_thread () =
(*DEBUG*)debug2 "EXIT THREAD" (tasks_status ()) ;
  become_inactive () ;
  if !in_pool >= pool_size && !active > !pool_konts then
    really_exit_thread ()
  else 
    pool_enter ()

let put_pool f =
  Mutex.lock pool_mutex ;
  pool_kont := f :: !pool_kont ; incr pool_konts ;
  Condition.signal pool_condition ;
(*DEBUG*)debug2 "PUT POOL" (tasks_status ()) ;
  Mutex.unlock pool_mutex

let create_process f =
(*DEBUG*)debug2 "CREATE_PROCESS" (tasks_status ()) ;
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
    if not (really_create_process g) then put_pool g 
  end else begin
    put_pool g
  end




(*
   Data structure for automaton is here seen
   as a record.
   In fact, automata are JoCustom blocks.
   JoCustom blocks are Custom blocks, which undergo
   standard gc

   Ident field acts both as a tag to discriminate
   local and remote automaton and as an identifier
   for exported threads.
*)

(*********************************)
(* Queues: implemented as stacks *)
(*********************************)

let put_queue auto idx a = auto.queues.(idx) <- a :: auto.queues.(idx)

let get_queue auto idx = match auto.queues.(idx) with
| [] -> assert false
| a::rem ->
    auto.queues.(idx) <- rem ;
    begin match rem with
    | [] -> auto.status.erase idx
    | _  -> ()
    end ;
    a

(*******************)
(* Automata status *)
(*******************)

(* int status *)

let int_ops () =
  let me = ref 0 in
  { 
    set = (fun i ->
      let old_me = !me in
      let new_me = old_me lor  (1 lsl i) in
      me := new_me ;
      old_me <> new_me) ;
    erase = (fun i -> me := !me land (lnot (1 lsl i))) ;
    includes = (fun mask -> !me land mask = mask) ;
    to_string = (fun () -> sprintf "%08x" !me) ;
  }

(* Or bitfields *)

let major i = i / 31
and minor i =  i mod 31

let bv_ops nchans =
  let nslots = (nchans + 30) / 31 in (* eh oui *)
  let me = Array.create nslots 0 in
  let set i =
    let slot = major i and idx = minor i in
    let old_me = me.(slot) in
    let new_me = old_me lor  (1 lsl idx) in
    me.(slot) <- new_me ;
    old_me <> new_me in

  let erase i =
    let slot = major i and idx = minor i in
    me.(slot) <- me.(slot) land (lnot (1 lsl idx)) in

  let rec do_includes mask slot =
    if slot >= nslots then true
    else
      let m = mask.(slot) in
      me.(slot) land m = m && do_includes mask (slot+1) in

  let includes mask = do_includes mask 0 in

  let rec do_to_string slot =
    if slot >= nslots then []
    else
      Printf.sprintf "%08x" me.(slot)::
      do_to_string (slot+1) in

  let to_string () = String.concat "" (do_to_string 0) in

  {
    set = set ;
    erase = erase ;
    includes = includes ;
    to_string = to_string ;
  } 

(* Allocate proper status, depending on number of channels in automata *)
let empty_status nchans =
  if nchans < 32 then
    Obj.magic (int_ops ())
  else
    Obj.magic (bv_ops nchans)

(* Creating local automata *)
external alloc_stub : t_local -> stub = "caml_alloc_stub"

let wrap_automaton a = alloc_stub (LocalAutomaton a)

let create_automaton_debug nchans names =
  let a = 
    {
      ident = 0 ;
      status = empty_status nchans ;
      mutex = Mutex.create () ;
      queues = Array.create nchans [] ;
      matches = [| |] ;
      names = names ;
    } in
  a

let create_automaton nchans = create_automaton_debug nchans (Obj.magic 0)


let get_name auto idx = Obj.magic (Obj.field auto.names idx)

let patch_table a t =  a.matches <- t

(***************************************)
(* Implementing reply to sync channels *)
(***************************************)

type kval = Start | Go of (unit -> Obj.t) | Ret of Obj.t

type continuation =
  { kmutex : Mutex.t ;
    kcondition : Condition.t ;
    mutable kval : kval }


let kont_create0 mutex =
  {kmutex = mutex ;
   kcondition = Condition.create () ;
   kval = Start}

(* Continuation mutex is automaton mutex *)
let kont_create auto = kont_create0 auto.mutex

(**********************)
(* Asynchronous sends *)
(**********************)

type async =
    Async of (stub) * int
  | Alone of (stub) * int


let create_async auto i = Async (auto, i)
and create_async_alone auto g = Alone (auto, g)


(* Callbacks from compiled code *)

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

(* Transfer control to current thread
   can be called when send triggers a match in the async case
   in thread-tail position *)
let just_go_async auto f =
(*DEBUG*)debug3 "JUST_GO_ASYNC" "" ;
  Mutex.unlock auto.mutex ;
  f ()

let rec attempt_match tail auto reactions idx i =
  if i >= Obj.size reactions then begin
(*DEBUG*)debug3 "ATTEMPT FAILED" (sprintf "%s %s"
(*DEBUG*)  (get_name auto idx) (auto.status.to_string ())) ;    
    Mutex.unlock auto.mutex
  end else begin
    let (ipat, iprim, f) = Obj.magic (Obj.field reactions i) in
    if auto.status.includes ipat then
      if iprim < 0 then begin
        f (if tail then just_go_async else fire_go) (* f will unlock auto's mutex *)
      end else begin
        f kont_go
      end
    else
      attempt_match tail auto reactions idx (i+1)
  end

let local_send_async auto idx a =
(*DEBUG*)debug3 "SEND_ASYNC" (sprintf "channel=%s, status=%s"
(*DEBUG*)  (get_name auto idx) (auto.status.to_string ())) ;
(* Acknowledge new message by altering queue and status *)
  Mutex.lock auto.mutex ;
  put_queue auto idx a ;
  if not (auto.status.set idx) then begin
(*DEBUG*)debug3 "SEND_ASYNC" (sprintf "Return: %s"
(*DEBUG*)  (auto.status.to_string ())) ;
    Mutex.unlock auto.mutex
  end else begin
    attempt_match false auto (Obj.magic auto.matches) idx 0
  end

(* Optimize forwarders *)
and local_send_async_alone auto g a =
(*DEBUG*)  debug3 "SEND_ASYNC_ALONE" (sprintf "match %i" g) ;
  let _,_,f = Obj.magic (Obj.field (Obj.magic auto.matches) g) in
  create_process (fun () -> f a)


let send_async chan a = match chan with
| Async ({local=LocalAutomaton auto}, idx) -> local_send_async auto idx a
| Alone ({local=LocalAutomaton auto}, g)   -> local_send_async_alone auto g a
| Async ({local=RemoteAutomaton _}, _)
| Alone ({local=RemoteAutomaton _}, _)
 -> assert false

let local_tail_send_async auto idx a =
(*DEBUG*)debug3 "TAIL_ASYNC" (sprintf "channel %s, status=%s"
(*DEBUG*)  (get_name auto idx) (auto.status.to_string ())) ;
(* Acknowledge new message by altering queue and status *)
  Mutex.lock auto.mutex ;
  put_queue auto idx a ;
  if not (auto.status.set idx) then begin
(*DEBUG*)debug3 "TAIL_ASYNC" (sprintf "Return: %s"
(*DEBUG*) (auto.status.to_string ())) ;
    Mutex.unlock auto.mutex
  end else begin
    attempt_match true auto (Obj.magic auto.matches) idx 0
  end


(* Optimize forwarders *)

and local_tail_send_async_alone auto g a =
(*DEBUG*)  debug3 "TAIL_ASYNC_ALONE" (sprintf "match %i" g) ;
  let _,_,f = Obj.magic (Obj.field (Obj.magic auto.matches) g) in
  f a

let tail_send_async chan a = match chan with
| Async ({local=LocalAutomaton auto}, idx) ->
    local_tail_send_async auto idx a
| Alone ({local=LocalAutomaton auto}, g)   ->
    local_tail_send_async_alone auto g a
| Async ({local=RemoteAutomaton _}, _)
| Alone ({local=RemoteAutomaton _}, _)
 -> assert false

(*********************)
(* Synchronous sends *)
(*********************)


(* No match was found *)
let kont_suspend k =
(*DEBUG*)debug3 "KONT_SUSPEND" (tasks_status ()) ;
(*DEBUG*)incr_locked nthreads_mutex suspended ;
  become_inactive () ;
  if !active = !pool_konts then grab_from_pool 0.1 ;      
  Condition.wait k.kcondition k.kmutex ;
(*DEBUG*)decr_locked nthreads_mutex suspended ;
  Mutex.unlock k.kmutex ;
  match k.kval with
  | Go f ->
(*DEBUG*)debug3 "REACTIVATED" (tasks_status ()) ;
      f ()
  | Ret v ->
(*DEBUG*)debug3 "REPLIED" (tasks_status ()) ;
      v
  | Start -> assert false

(* Suspend current thread when some match was found *)
let suspend_for_reply k =
(*DEBUG*)incr_locked nthreads_mutex suspended ;
  become_inactive () ;
  Condition.wait k.kcondition k.kmutex ;
(*DEBUG*)decr_locked nthreads_mutex suspended ;
  Mutex.unlock k.kmutex ;
  match k.kval with
  | Ret v ->
(*DEBUG*)debug3 "REPLIED" (tasks_status ()) ;
      v
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
(*DEBUG*)debug3 "JUST_GO" "" ;
  Mutex.unlock k.kmutex ;
  f ()

(* Fire process and suspend : no principal name *)
let fire_suspend k _ f =
(*DEBUG*)  debug2 "FIRE_SUSPEND" "" ;
  create_process f ;
  suspend_for_reply k

let rec attempt_match_sync idx kont auto reactions i =
  if i >= Obj.size reactions then begin
(*DEBUG*)debug3 "SYNC ATTEMPT FAILED" (sprintf "%s %s"
(*DEBUG*)  (get_name auto idx) (auto.status.to_string ())) ;    
    kont_suspend kont
  end else begin
    let (ipat, ipri, f) = Obj.magic (Obj.field reactions i) in
    if auto.status.includes ipat then begin
      if ipri < 0 then
        f (fire_suspend kont)   (* will create other thread *)
      else if ipri = idx then begin
        f just_go               (* will continue evaluation *)
      end else begin
        f (kont_go_suspend kont) (* will awake principal thread *)
      end
    end else attempt_match_sync idx kont auto reactions (i+1)
  end

let local_send_sync auto idx a =
(*DEBUG*)  debug3 "SEND_SYNC" (sprintf "channel %s" (get_name auto idx)) ;
(* Acknowledge new message by altering queue and status *)
  Mutex.lock auto.mutex ;
  let kont = kont_create auto in
  put_queue auto idx (Obj.magic (kont,a)) ;
  if not (auto.status.set idx) then begin
(*DEBUG*)debug3 "SEND_SYNC" (sprintf "Return: %s"
(*DEBUG*) (auto.status.to_string ())) ;
    kont_suspend kont
  end else begin
    attempt_match_sync idx kont auto (Obj.magic auto.matches) 0
  end

let send_sync auto idx arg = match auto.local with
| LocalAutomaton a -> local_send_sync a idx arg
| RemoteAutomaton (_,_) -> assert false

(*
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
*)

(* This code must create a one-argument closure,
   whose code pointer unambiguously characterize
   closures which synchronous channels.
   Additionaly the 'send_sync' free name must be registered
   as a special value for enabling specific marshalling of
   sync channels *)

let create_sync auto idx = 
  let r a = send_sync auto idx a in
  r



(* HACK :
   Inform marshaller about one
   code address and one value that are rebound dynamically
   by marshalling operations *)
type sync = Obj.t -> Obj.t
external register_value : 'a -> unit = "caml_register_saved_value"
external register_code : sync -> unit = "caml_register_saved_code"

let _ =
  register_value send_sync ;
  register_code (create_sync (Obj.magic 0) 0)


let reply_to v k =
(*DEBUG*)  debug3 "REPLY" (sprintf "%i" (Obj.magic v)) ;
  Mutex.lock k.kmutex ;
  k.kval <- Ret v ;
  Condition.signal k.kcondition ;
  Mutex.unlock k.kmutex 

(********************************)
(* Management of initial thread *)
(********************************)


(* Called when all active tasks are waiting in thread pool *)
let from_pool () =
  if !in_pool > 0 then begin
(*DEBUG*)debug1 "HOOK" "SHOULD PERPHAPS SIGNAL" ;    
(*    Condition.signal pool_condition  *) ()
  end else begin (* Create a new thread to enter pool *)
(*DEBUG*)debug1 "HOOK" "CREATE" ;    
    incr_active () ;
    let b = really_create_process exit_thread in
(*DEBUG*)debug1 "HOOK" (if b then "PROCESS CREATED" else "FAILED");
    if not b then begin
      prerr_endline "Threads are exhausted, good bye !"
    end

  end

let rec exit_hook () =
(*DEBUG*)debug1 "HOOK" "enter" ;
(*DEBUG*)decr_locked nthreads_mutex nthreads ;
  Mutex.lock active_mutex ;
  decr active ;
  begin if !active > 0 then begin
    if !pool_konts = !active then begin
      Mutex.unlock active_mutex ;
      from_pool ()
    end ;
(*DEBUG*)debug1 "HOOK" "suspend" ;
    Condition.wait active_condition active_mutex
  end else
    Mutex.unlock active_mutex
  end ;
(*DEBUG*)debug1 "HOOK" "over" ;
  ()


external init_join : unit -> unit = "caml_init_join"

let _ = init_join () ; at_exit exit_hook


let t v flags =
  let (_,t) as p = Join_space.marshal_message v flags in
  Join_space.unmarshal_message p
