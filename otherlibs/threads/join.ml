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

let create_process f = Join_scheduler.create_process f

type queue 

let put_queue auto idx a =
  auto.queues.(idx) <- Obj.magic (a :: Obj.magic (auto.queues.(idx)))

let get_queue auto idx = match Obj.magic (auto.queues.(idx)) with
| [] -> assert false
| a::rem ->
    auto.queues.(idx) <- Obj.magic rem ;
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
      queues = Array.create nchans (Obj.magic []) ;
      matches = [| |] ;
      names = names ;
    } in
  a

let create_automaton nchans = create_automaton_debug nchans [| |]

let get_name auto idx = auto.names.(idx)

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
let kont_go k (f:unit -> 'a) =
(*DEBUG*)debug2 "KONT_GO" "" ;
  k.kval <- Go f ;
  Join_scheduler.incr_active () ;
  Condition.signal k.kcondition ;
  Mutex.unlock k.kmutex

(* Spawn new process *)
let fire_go auto f =
(*DEBUG*)debug3 "FIRE_GO" "" ;
  Mutex.unlock auto.mutex ;
  Join_scheduler.create_process f

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
| Async ({local=RemoteAutomaton (rspace, uid)}, idx) ->
    Join_space.remote_send_async rspace uid idx a
| Alone (_,_) -> assert false

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
| Async ({local=RemoteAutomaton (rspace, uid)}, idx) ->
    Join_space.remote_send_async rspace uid idx a 
| Alone (_, _)
 -> assert false

(*********************)
(* Synchronous sends *)
(*********************)


(* No match was found *)
let kont_suspend k =
(*DEBUG*)debug3 "KONT_SUSPEND" (Join_scheduler.tasks_status ()) ;
  Join_scheduler.inform_suspend () ;
  Condition.wait k.kcondition k.kmutex ;
  Join_scheduler.inform_unsuspend () ;
  Mutex.unlock k.kmutex ;
  match k.kval with
  | Go f ->
(*DEBUG*)debug3 "REACTIVATED" (Join_scheduler.tasks_status ()) ;
      f ()
  | Ret v ->
(*DEBUG*)debug3 "REPLIED" (Join_scheduler.tasks_status ()) ;
      ((Obj.magic v) : 'a)
  | Start -> assert false

(* Suspend current thread when some match was found *)
let suspend_for_reply k =
  Join_scheduler.inform_suspend () ;
  Condition.wait k.kcondition k.kmutex ;
  Join_scheduler.inform_unsuspend () ;
  Mutex.unlock k.kmutex ;
  match k.kval with
  | Ret v ->
(*DEBUG*)debug3 "REPLIED" (Join_scheduler.tasks_status ()) ;
      v
  | Start|Go _ -> assert false


(* Transfert control to frozen principal thread and suspend current thread *)
let kont_go_suspend kme kpri f =
(*DEBUG*)debug2 "KONT_GO_SUSPEND" "" ;
(* awake principal *)
  kpri.kval <- Go f ;
  Join_scheduler.incr_active () ;
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

let _ = Join_space.send_async_ref.Join_space.f <- local_send_async


let send_sync auto idx arg = match auto.local with
| LocalAutomaton a -> local_send_sync a idx arg
| RemoteAutomaton (_,_) -> assert false


(* This code must create a one-argument closure,
   whose code pointer unambiguously characterize
   closures which synchronous channels.
   Additionaly the 'send_sync' free name must be registered
   as a special value for enabling specific marshalling of
   sync channels *)

let create_sync auto idx = 
  let r a = Obj.obj (send_sync auto idx a) in
  r



(* HACK :
   Inform marshaller about one
   code address and one value that are rebound dynamically
   by marshalling operations *)

type sync = Obj.t -> Obj.t
external register_value : 'a -> unit = "caml_register_saved_value"
external register_code : sync -> unit = "caml_register_saved_code"
external init_join : unit -> unit = "caml_init_join"

let _ =
  init_join () ;
  register_value send_sync ;
  register_code (create_sync (Obj.magic 0) 0)

let reply_to v k =
(*DEBUG*)  debug3 "REPLY" (sprintf "%i" (Obj.magic v)) ;
  Mutex.lock k.kmutex ;
  k.kval <- Ret (Obj.repr v) ;
  Join_scheduler.incr_active () ;
  Condition.signal k.kcondition ;
  Mutex.unlock k.kmutex 

(* TEST *)
open Join_space
let t v flags =
  let (_,t) as p = marshal_message v flags in
  Join_space.unmarshal_message p
