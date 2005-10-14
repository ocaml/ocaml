(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* $Id$ *)

open Printf
open Join_misc
open Join_link
(*DEBUG*)open Join_debug

let creation_time = Unix.gettimeofday ()

 (* Create site socket *)
let local_port, local_socket = Join_misc.create_port 0
    ;;

 (* And compute global site identifier *)
let local_id = local_addr, local_port, creation_time
    ;;

let same_space (a1, p1, t1) (a2, p2, t2) =
  a1 = a2 && p1 = p2 && t1 = t2
    ;;

 
 open Join_types

 (* Attempt to handle SIGPIPE *)
 let _ =
   Sys.set_signal
     Sys.sigpipe
     (Sys.Signal_handle
	(fun _ ->
 (*DEBUG*)debug1 "SIG HANDLER" "SIGPIPE" ;
	  ()))

 (* Describe site *)
 let local_space = {
   space_id = local_id ;
   space_status = SpaceUp ;
   uid_mutex = Mutex.create () ;
   next_uid =
     begin 
       let uid_counter = ref 0 in
       (fun () ->
	 Pervasives.incr uid_counter;
	 let r = !uid_counter in
	 r)
     end ;
   uid2local = Join_hash.create () ;
   remote_spaces =  Join_hash.create () ;
   space_listener = Deaf (local_socket, Mutex.create ())
 } 
 ;;

let space_to_string (addr, port, _) =
  sprintf "%s:%i" (Unix.string_of_inet_addr addr) port

let get_remote_space space space_id =
  Join_hash.get space.remote_spaces
    (fun id ->
(*DEBUG*)debug2 "RSPACE CREATE" (space_to_string space_id) ;
      {
      rspace_id = id ;
      next_kid =
      begin
	let kid_counter = ref 0
	and kid_mutex = Mutex.create () in
	(fun () ->
	  Mutex.lock kid_mutex ;
	  let r = !kid_counter in
	  Pervasives.incr kid_counter ;
	  Mutex.unlock kid_mutex ;
	  r)
      end ;
      replies_pending = counter_create () ;
      konts = Join_hash.create () ;
      link = NoConnection (Mutex.create ()) ;
      write_mtx = Mutex.create () ;
    })
    space_id

let find_local space uid =  
  try Join_hash.find space.uid2local uid
  with Not_found -> assert false

let find_automaton space uid =
  let r =
    try  Join_hash.find space.uid2local uid
    with Not_found -> assert false in
  (Obj.magic r : automaton)

let find_async_forwarder space uid =
  let r =
    try  Join_hash.find space.uid2local uid
    with Not_found -> assert false in
  (Obj.magic r : 'a -> unit)

let find_sync_forwarder space uid =
  let r =
    try  Join_hash.find space.uid2local uid
    with Not_found -> assert false in
  (Obj.magic r : 'a -> 'b)

type async_ref =
    { mutable async : 'a . automaton -> int -> 'a -> unit }
let send_async_ref = { async =  (fun _ _ _ -> assert false) }


type sync_ref =
    { mutable sync : 'a 'b . automaton -> int -> 'a -> 'b}
let send_sync_ref = { sync = (fun _ _ _ -> assert false) }


(* Performed in extern.c/intern.c through custom operations
   defined in join.c *)

external do_globalize_message :
  'a -> Marshal.extern_flags list -> string * stub array
    = "caml_globalize_message" 

external do_localize_message :
  string -> (stub_tag * stub_val * int) array -> 'a
    = "caml_localize_message" 


let string_of_space = space_to_string 

let verbose_close caller fd =  
  (*DEBUG*)debug1 ("CLOSE from "^caller) (sprintf "%i" (Obj.magic fd)) ;
  try
    Unix.close fd
  with e ->
    (*DEBUG*)debug1 ("CLOSE from "^caller)
      (*DEBUG*) (Join_misc.exn_to_string e) ;
    ()

exception NoLink

(* listener is started in several occasions, nevertheless ensure unicity *)
let rec start_listener space = match space.space_listener with
| Listen _ -> ()
| Deaf (sock,mtx) ->
    Mutex.lock mtx ;
    match space.space_listener with
    | Deaf (sock,mtx) ->
	space.space_listener <- Listen sock ;
	Mutex.unlock mtx ;
	Join_scheduler.create_process (listener space sock)
    | Listen _ ->
	Mutex.unlock mtx

and listener space sock () =
  try while true do
(*DEBUG*)debug1 "LISTENER"
(*DEBUG*)  (sprintf "now accept on: %s"
(*DEBUG*)    (space_to_string space.space_id)) ;
    let s,_ = Join_misc.force_accept sock in
(*DEBUG*)debug1 "LISTENER" "someone coming" ;
    let link = Join_link.create s in
    let rspace_id = (input_value link : space_id) in
(*DEBUG*)debug1 "LISTENER" ("his name: "^space_to_string rspace_id)  ;
    output_value link true ; flush link ;
    let rspace = get_remote_space space rspace_id in
    open_link space rspace link
  done  with  e ->
(*DEBUG*)debug0 "LISTENER"
(*DEBUG*)  (sprintf "died of %s" (Join_misc.exn_to_string e)) ;
    ()

and open_link space rspace link =  match rspace.link with
| Connected _
| DeadConnection ->
    close link
| NoConnection mtx ->
    Mutex.lock mtx ;
    match rspace.link with
    | Connected _
    | DeadConnection -> (* quite unlikely ! *)
	close link ;
	Mutex.unlock mtx
    | NoConnection _ ->
	rspace.link <- Connected (link,mtx) ;
	Mutex.unlock mtx ;
	Join_scheduler.create_process
	  (join_handler space rspace link)

and join_handler space rspace link () =
(*DEBUG*)debug1 "HANDLER"
(*DEBUG*)  ("start receiving from "^string_of_space rspace.rspace_id) ;  try
  while true do
    let msg = Join_message.input_msg link in
(*DEBUG*)debug2 "HANDLER" ("message from "^string_of_space rspace.rspace_id) ;
    match msg with
    | AsyncSend (chan, v) ->
	let auto = find_automaton space chan.auto_id
	and v = localize_rec space v in
	send_async_ref.async auto chan.chan_id v
    | AloneSend (uid,v) ->
        let g = find_async_forwarder space uid
        and v = localize_rec space v in
(* inlined async call, must match local_send_alone in join.ml *)
        Join_scheduler.create_process (fun () -> g v)
    | SyncSend (chan, kid, v) ->
	let auto = find_automaton space chan.auto_id
	and v = localize_rec space v
        and idx = chan.chan_id in
        call_sync space rspace kid
          (fun v -> send_sync_ref.sync auto idx v) v
    | AloneSyncSend (uid, kid, v) ->
        let g = find_sync_forwarder space uid
        and v = localize_rec space v in
        call_sync space rspace kid g v
    | ReplySend (kid, v) ->
	let kont =
	  try Join_hash.find_remove rspace.konts kid
	  with Not_found -> assert false
	and v = localize_rec space v in
	Join_scheduler.reply_to v kont
    | ReplyExn (kid, e) ->
	let kont =
	  try Join_hash.find_remove rspace.konts kid
	  with Not_found -> assert false in
	let e = Join_message.localize_exn e in
	Join_scheduler.reply_to_exn e kont              
  done
with
| Failed ->
(*DEBUG*)debug1 "HANDLER" "input operation failed" ;
    close_link rspace
| e ->
(*DEBUG*)debug0 "HANDLER"
(*DEBUG*)  (sprintf "died of %s" (Join_misc.exn_to_string e)) ;
    ()

and call_sync space rspace kid g v =
  Join_scheduler.create_process
    (fun () ->
      incr rspace.replies_pending ;
      begin try
	let r = g v in
	remote_reply_to space rspace kid r
      with e -> remote_reply_to_exn space rspace kid e
      end ;
      (* At this point the reply is flushed, or the rspace is dead *)
      decr rspace.replies_pending)

and do_remote_send space rspace do_msg a =
  try
    let link = get_link space rspace in
    sender_work rspace link (do_msg (globalize_rec space a []))
  with NoLink -> ()

and remote_reply_to space rspace kid a =
  do_remote_send space rspace
    (fun v -> ReplySend (kid, v)) a

and remote_reply_to_exn space rspace kid e =
  try
    let link = get_link space rspace in
    sender_work rspace link
      (ReplyExn (kid, e))
  with NoLink -> ()

and close_link rspace = match rspace.link with
| NoConnection _ -> assert false
| DeadConnection -> ()
| Connected (_, mtx) ->
    Mutex.lock mtx ;
    match rspace.link with
    | NoConnection _ -> assert false
    | DeadConnection -> Mutex.unlock mtx
    | Connected (link,_) ->
	rspace.link <- DeadConnection ;
	Mutex.unlock mtx ;
	try
	  close link ;
(* For replies not made *)
	  Join_hash.iter_empty rspace.konts
	    (fun _ k -> Join_scheduler.reply_to_exn JoinExit k) ;
(*DEBUG*)debug1 "HANDLER" "cleanup is over" ;
	  ()
	with Failed -> assert false

(* Get link for rspace, in case no connection is here yet, create one *)
and get_link space rspace =  match rspace.link with
  | DeadConnection -> raise NoLink
  | Connected (link,_) -> link
  | NoConnection _ ->
      let addr,port,_ = rspace.rspace_id in
      let s = Join_misc.force_connect addr port in
      let link = Join_link.create s in
      output_value link space.space_id ; flush link ;
      ignore (input_value link : bool) ;
      open_link space rspace link ;
      get_link space rspace

and sender_work rspace link msg =
(*DEBUG*)debug2 "SENDER" ("message for "^string_of_space rspace.rspace_id) ;
  try
    Mutex.lock rspace.write_mtx ;
    Join_message.output_msg link msg ; flush link ;
    Mutex.unlock rspace.write_mtx
  with Failed ->
    Mutex.unlock rspace.write_mtx ;
(*DEBUG*)debug0 "SENDER" "output operation failed" ;
    close_link rspace

(* returns global identification for stub *)
and export_stub space stub = match stub.stub_tag with
|  Local ->
    let uid = stub.uid in
    if uid <> 0 then space.space_id, uid
    else begin
    (* race condition, since several threads can be exporting this stub *)
      Mutex.lock space.uid_mutex ;
      let uid = stub.uid in
      if uid <> 0 then begin (* lost *)
	Mutex.unlock space.uid_mutex  ;
	space.space_id, uid
      end else begin (* won, allocate new uid *)
	let uid = space.next_uid () in
	stub.uid <- uid ;
	Mutex.unlock space.uid_mutex ;	
	(* Listener is started  at first export *)
	if uid = 1 then start_listener space ;
	(* Remember binding uid -> local value *)
	Join_hash.add space.uid2local uid stub.stub_val ;
	(space.space_id, uid)
      end
    end
| Remote ->
    let rspace = (Obj.magic stub.stub_val : space_id ) in
    (rspace, stub.uid)

(* quasi-reverse of export stub, from global names to
   values, note that the stub is allocated by
   do_localize_message *)
and import_stub space (rspace_id, uid) =
  if same_space rspace_id space.space_id then
    Local, find_local space uid, uid
  else
    Remote, (Obj.magic rspace_id : stub_val), uid

and globalize_rec space v flags =
  let s,t =  do_globalize_message v flags in
  s, Array.map (export_stub space) t

and localize_rec space (s,t) =
  do_localize_message s (Array.map (import_stub space) t)
    

(***********************************)
(* Various remote message sendings *)
(***********************************)

(* When outlink is dead, get_tout_queue raises NoLink
   async messages are distroyed silentely *)

let do_remote_send_async space rspace_id uid idx a =
  do_remote_send space (get_remote_space space rspace_id)
    (fun v -> AsyncSend ({auto_id=uid; chan_id=idx;}, v)) a

let remote_send_async rspace uid idx a =
(*DEBUG*)debug3 "REMOTE" "SEND ASYNC" ;
  do_remote_send_async local_space rspace uid idx a
    
let do_remote_send_alone space rspace_id uid a =
  do_remote_send space (get_remote_space space rspace_id)
    (fun v -> AloneSend (uid, v)) a

let remote_send_alone rspace_id uid a =
(*DEBUG*)debug3 "REMOTE" "SEND ALONE" ;
  do_remote_send_alone local_space rspace_id uid a

(* When outlink is dead, get_link raises NoLink
   then, sync calls can fail the join way,
   this will make all tasks waiting replies to die silently *)

let do_remote_call space rspace_id do_msg kont a =
  let rspace = get_remote_space space rspace_id in
  let kid = rspace.next_kid () in
  if kid = 0 then start_listener space ; (* first continuation exported *)
(* There is a race condition with join_handler suicide here,
   the calling task may not get its Join_misc.Exit exception... *)
  Join_hash.add rspace.konts kid kont ;
  let link =
    try get_link space rspace
    with NoLink ->
      Join_hash.remove rspace.konts kid ;
      raise Join_misc.JoinExit in
  sender_work rspace link (do_msg kid (globalize_rec space a [])) ;
  Mutex.lock kont.kmutex ;
  Join_scheduler.suspend_for_reply kont

let do_remote_send_sync space rspace_id uid idx kont a =
  do_remote_call space rspace_id
    (fun kid v -> SyncSend ({auto_id=uid; chan_id=idx}, kid,v))
    kont a
    
let remote_send_sync rspace uid idx kont a =
(*DEBUG*)debug3 "REMOTE" "SEND SYNC" ;
  do_remote_send_sync local_space rspace uid idx kont a

let do_remote_send_sync_alone space rspace_id uid kont a =
  do_remote_call space rspace_id
    (fun kid v -> AloneSyncSend (uid, kid, v))
    kont a

let remote_send_sync_alone rspace_id uid kont a =
(*DEBUG*)debug3 "REMOTE" "SEND SYNC ALONE" ;
  do_remote_send_sync_alone local_space rspace_id uid kont a


let kill_remote_space space rspace = assert false


(********************************)
(* Use exit 0, in place of halt *)
(********************************)

let do_halt space = assert false
(*
  Mutex.lock space.space_mutex ; (* Against double halt, need more probably *)
  begin match space.space_status with
  | SpaceDown ->
      Mutex.unlock space.space_mutex
  | SpaceUp ->
      space.space_status <- SpaceDown ;
      Mutex.unlock space.space_mutex ;
      Join_hash.iter space.remote_spaces
        (fun _ rspace -> kill_remote_space space rspace) ;
      let listsock = match space.space_listener with
      | Deaf (fd,_) | Listen fd -> fd in
      Unix.shutdown listsock Unix.SHUTDOWN_ALL ;
      verbose_close "halt" listsock ;
      ()
  end
*)

let halt () = do_halt local_space

(* exported versions of globalize/localize, for tests *)
let globalize v flags = globalize_rec local_space v flags
and localize v = localize_rec local_space v



(************************************************)
(* Flush messages to rspace as much as possible *)
(************************************************)

(*
   Well, at least all replies are delivered.
   This will go into deadlock if some remote sync call
   is pending on local definition.

   Nothing particular is done for other remote message,
   except that the queue is flushed
 *)

let flush_out_queue space rspace =
(* First wait for replies to be passed to out queue *)
(*DEBUG*)debug2 "WAIT PENDING REPLIES" (space_to_string rspace.rspace_id) ;
  wait_zero rspace.replies_pending ;
(*DEBUG*)debug2 "NO PENDING REPLIES" (space_to_string rspace.rspace_id) ;
  ()

let do_flush_out_queues space =
(*DEBUG*)debug2 "FLUSH SPACE" "enter" ;
  Join_hash.iter space.remote_spaces
    (fun _ rspace -> flush_out_queue space rspace)

let flush_space () =  do_flush_out_queues local_space

