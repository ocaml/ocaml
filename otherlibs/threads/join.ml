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

(* There are two sort of queues
   - Unit queues are counters starting from one (1 <=> empty queue)
   - Standard queues are lists of messages *)

let put_queue auto idx a =
  let qs = auto.queues in
  let q = Array.unsafe_get qs idx in
  if Obj.is_int q && Obj.obj q <> 0 then
    Array.unsafe_set qs idx (Obj.repr (Obj.obj q+1))
  else
    Array.unsafe_set qs idx (Obj.repr (a :: Obj.obj q))

let get_queue auto idx =
  let qs = auto.queues in
  let q = Array.unsafe_get qs idx in
  if Obj.is_int q && Obj.obj q <> 0 then begin
    let count = Obj.obj q-1 in
    Array.unsafe_set qs idx (Obj.repr count) ;
    if  count=1 then auto.status.erase idx ;
    Obj.magic ()
  end else match Obj.obj q with
  | [] -> assert false
  | a::rem ->
      Array.unsafe_set qs idx (Obj.repr rem) ;
      begin match rem with
      | [] -> auto.status.erase idx
      | _  -> ()
      end ;
      a

let init_unit_queue auto idx = Array.unsafe_set auto.queues idx (Obj.repr 1)

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


external alloc_stub : stub_val -> stub = "caml_alloc_stub"

let wrap_automaton (a:automaton) = alloc_stub (Obj.magic a : stub_val)
and wrap_guard (g:'a -> 'b) = alloc_stub (Obj.magic g : stub_val)

(* Creating local automata *)

let create_automaton_debug nchans names =
  let a = 
    {
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
    Async of stub * int
  | Alone of stub

(* Oups ! *)
external field0 : async -> stub = "%field0"

let create_async auto i = Async (auto, i)
and create_alone guard = Alone (wrap_guard guard)
and alloc_alone () = Alone (wrap_guard (fun _ -> assert false))
and patch_alone (a:async) (g:'a -> unit) =
  let stub = field0 a in
  stub.stub_val <- (Obj.magic g : stub_val)

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
and local_send_alone g a =
(*DEBUG*)  debug3 "SEND_ALONE" "" ;
  create_process (fun () -> g a)

let _ = Join_space.send_async_ref.Join_space.async <- local_send_async


let send_async chan a = match chan with
| Async (stub, idx) ->
    begin match stub.stub_tag with
    | Local ->
	let auto = (Obj.magic stub.stub_val : automaton) in
	local_send_async auto idx a
    | Remote ->
	let rspace = (Obj.magic stub.stub_val : space_id) in
	Join_space.remote_send_async rspace stub.uid idx a
    end
| Alone stub ->
    begin match stub.stub_tag with
    | Local ->
	let guard = (Obj.magic stub.stub_val : 'a -> unit) in
	local_send_alone guard a
    | Remote ->
	let rspace = (Obj.magic stub.stub_val : space_id) in
	Join_space.remote_send_alone rspace stub.uid a
    end
    

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

and local_tail_send_alone g a =
(*DEBUG*)  debug3 "TAIL_SEND_ALONE" "" ;
  g a

let tail_send_async chan a = match chan with
| Async (stub, idx) ->
    begin match stub.stub_tag with
    | Local ->
	let auto = (Obj.magic stub.stub_val : automaton) in
	local_tail_send_async auto idx a
    | Remote ->
	let rspace = (Obj.magic stub.stub_val : space_id) in
	Join_space.remote_send_async rspace stub.uid idx a
    end
| Alone stub ->
    begin match stub.stub_tag with
    | Local ->
	let guard = (Obj.magic stub.stub_val : 'a -> unit) in
	local_tail_send_alone guard a
    | Remote ->
	let rspace = (Obj.magic stub.stub_val : space_id) in
	Join_space.remote_send_alone rspace stub.uid a
    end


(*********************)
(* Synchronous sends *)
(*********************)


(* No match was found *)
let kont_suspend k =
(*DEBUG*)debug3 "KONT_SUSPEND" (Join_scheduler.tasks_status ()) ;
  assert (k.kval = Start) ;
  Join_scheduler.inform_suspend () ;
  Condition.wait k.kcondition k.kmutex ;
  Join_scheduler.inform_unsuspend () ;
  Mutex.unlock k.kmutex ;
  match k.kval with
  | Go f ->
(*DEBUG*)debug3 "REACTIVATED" (Join_scheduler.tasks_status ()) ;
      Obj.obj (f ())
  | Ret v ->
(*DEBUG*)debug3 "REPLIED" (Join_scheduler.tasks_status ()) ;
      (Obj.obj v)
  | Exn e ->
(*DEBUG*)debug3 "REPLIED EXN" (Join_scheduler.tasks_status ()) ;
      raise e
  | Start -> assert false


(* Transfert control to frozen principal thread and suspend current thread *)
let kont_go_suspend kme kpri f =
(*DEBUG*)debug2 "KONT_GO_SUSPEND" "" ;
(* awake principal *)
  assert (kpri.kmutex == kme.kmutex) ;
  Join_scheduler.incr_active () ;
  assert (kpri.kval = Start) ;
  kpri.kval <- Go f ;
  Condition.signal kpri.kcondition ;
  Join_scheduler.suspend_for_reply kme

let just_go k f =
(*DEBUG*)debug3 "JUST_GO" "" ;
  Mutex.unlock k.kmutex ;
  f ()

(* Fire process and suspend : no principal name *)
let fire_suspend k _ f =
(*DEBUG*)  debug2 "FIRE_SUSPEND" "" ;
  create_process f ;
  Join_scheduler.suspend_for_reply k

let rec attempt_match_sync idx auto kont reactions i =
  if i >= Obj.size reactions then begin
(*DEBUG*)debug3 "SYNC ATTEMPT FAILED" (sprintf "%s %s"
(*DEBUG*)  (get_name auto idx) (auto.status.to_string ())) ;    
    kont_suspend kont
  end else begin
    let (ipat, ipri, _) as t = Obj.magic (Obj.field reactions i) in
    if auto.status.includes ipat then begin
      let (_, _, f) = t in
      if ipri < 0 then
        f (fire_suspend kont)   (* will create other thread *)
      else if ipri = idx then begin
        f just_go               (* will continue evaluation *)
      end else begin
        f (kont_go_suspend kont) (* will awake principal thread *)
      end
    end else attempt_match_sync idx auto kont reactions (i+1)
  end

let local_send_sync auto idx a =
(*DEBUG*)  debug3 "SEND_SYNC" (sprintf "channel %s" (get_name auto idx)) ;
  let kont = kont_create auto in
(* Acknowledge new message by altering queue and status *)
  Mutex.lock auto.mutex ;
  put_queue auto idx (Obj.magic (kont,a)) ;
  if not (auto.status.set idx) then begin
(*DEBUG*)debug3 "SEND_SYNC" (sprintf "Return: %s"
(*DEBUG*) (auto.status.to_string ())) ;
    kont_suspend kont
  end else begin
    attempt_match_sync idx auto kont (Obj.magic auto.matches) 0
  end


let _ = Join_space.send_sync_ref.Join_space.sync <-  local_send_sync

let send_sync stub idx arg = match stub.stub_tag with
| Local ->
    let a = (Obj.magic stub.stub_val : automaton ) in
    local_send_sync a idx arg
| Remote ->
    let kont = kont_create0 (Mutex.create ())
    and rspace_id = (Obj.magic stub.stub_val : space_id) in
    Join_space.remote_send_sync rspace_id stub.uid idx kont arg

let send_sync_alone stub arg =
(*DEBUG*)debug2 "SEND SYNC ALONE" "" ;
match stub.stub_tag with
| Local ->
    let g = (Obj.magic stub.stub_val : 'a -> 'b) in
    g arg
| Remote ->
    let kont = kont_create0 (Mutex.create ())
    and rspace_id = (Obj.magic stub.stub_val : space_id) in
    Join_space.remote_send_sync_alone rspace_id stub.uid kont arg


(* This code must create a one-argument closure,
   whose code pointer unambiguously characterize
   closures which synchronous channels.
   Additionaly the 'send_sync' free name must be registered
   as a special value for enabling specific marshalling of
   sync channels *)

let create_sync (auto:stub) idx = 
  let r a = Obj.obj (send_sync auto idx a) in
  r

(* this sync channel creator is shared *)
let do_create_sync_alone stub =
  let r a = send_sync_alone stub a in
  r

let create_sync_alone g =
  do_create_sync_alone (wrap_guard g)

let alloc_stub_guard () = wrap_guard (fun _ -> assert false)

let alloc_sync_alone (stub:stub) =
  do_create_sync_alone stub

let patch_sync_alone (stub:stub) (g:'a -> 'b) =
  stub.stub_val <- (Obj.magic g : stub_val)


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

let reply_to = Join_scheduler.reply_to
and reply_to_exn = Join_scheduler.reply_to_exn

let raise_join_exit () = raise Join_misc.JoinExit

let exit_hook = Join_scheduler.exit_hook

let exn_global = Join_space.exn_global
  
let flush_space = Join_space.flush_space

(* Debug from users programs *)

let debug = Join_debug.debug0
and debug0 = Join_debug.debug0
and debug1 = Join_debug.debug1
and debug2 = Join_debug.debug2
and debug3 = Join_debug.debug3

