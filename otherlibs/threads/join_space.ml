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
(*DEBUG*)open Join_debug


let same_space s1 s2 = Pervasives.compare s1 s2 = 0

let compare_space s1 s2 = Pervasives.compare s1 s2
 
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
  services = Join_hash.create () ;
  listener = Deaf (Mutex.create ()) ;
} 
 ;;

let space_to_string raddr = Join_misc.string_of_sockaddr raddr

let create_remote_space id =
(*DEBUG*)debug1 "RSPACE CREATE" (space_to_string id) ;
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
    hooks = [] ;
  }

(* Get remote space from its name.
   When absent one unique rspace data structure is
   added to the space.remote_spaces table *)

let get_remote_space space space_id =
  try Join_hash.find space.remote_spaces space_id
  with Not_found ->
    let new_rspace = create_remote_space space_id in
    match
      Join_hash.add_once
        space.remote_spaces space_id new_rspace
    with
    | Some r -> r
    | None -> new_rspace

(*
and remove_remote_space space rspace =
  let space_id = rspace.rspace_id in
  Join_hash.remove space.remote_spaces space_id
*)

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

exception Service_not_found

let find_service space key =
  try
    let uid = Join_hash.find space.services key in
    uid
  with
  | Not_found -> raise Service_not_found


type async_ref =
    { mutable async : 'a . automaton -> int -> 'a -> unit }
let send_async_ref = { async =  (fun _ _ _ -> assert false) }

type async_gen_ref =
  { mutable async_gen : 'a.'a Join_types.async -> 'a -> unit ; }

let send_async_gen_ref = { async_gen = (fun _ _ -> assert false) }

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

let string_of_addr = function
  | None -> "default"
  | Some a -> string_of_sockaddr a

let check_addr id addro = match addro with
| None -> ()
| Some addr ->
    if not (same_space id addr) then
      failwith
        (sprintf "attempt to listen on %s, already running as %s"
           (string_of_space addr) (string_of_space id))

(* listener is started in several occasions, nevertheless ensure unicity *)
let rec start_listener space addr = match space.listener with
| Listen id -> check_addr id addr
| Deaf mtx ->
    Mutex.lock mtx ;
    match space.listener with
    | Listen id ->
	Mutex.unlock mtx ;
        check_addr id addr
    | Deaf _ ->
        let when_accepted link =
          let rspace_id = (Join_message.input_value link : space_id) in
(*DEBUG*)debug1 "LISTENER" ("his name: "^space_to_string rspace_id)  ;
          let rspace = get_remote_space space rspace_id in
          open_link_accepted space rspace link in
        begin try
          let my_id,_ = Join_port.establish_server addr when_accepted in
	  space.listener <- Listen my_id ;
        with
        | Join_port.Failed msg ->
            Mutex.unlock mtx ;
            prerr_endline
              (sprintf "cannot listen on %s: %s"
                 (string_of_addr addr) msg) ;
            exit 2 (* little we can do to repair that *)
        end ;
	Mutex.unlock mtx

and  get_id space = match space.listener with
| Listen id -> id
| Deaf _ -> start_listener space None ; get_id space

(* called when a connection is aborted, at moment
   used only at setup time when ruled out by a more prioritary connection *)
and close_link_accepted link =
  try (* announce partner we failed *)
    Join_message.output_value link false ;
    Join_link.flush link ;
    Join_link.close link
  with Join_link.Failed -> ()

(* terminate the winning connection attempt *)
and finally_open_link_accepted  space rspace link mtx =
  rspace.link <- Connected (link,mtx) ;
  Mutex.unlock mtx ;
  try
    Join_message.output_value link true ;
    Join_link.flush link ;
    Join_scheduler.create_process
      (join_handler space rspace link)
  with Join_link.Failed ->
(*DEBUG*)debug1 "OPEN LINK ACCEPTED" "failed" ;
    close_link space rspace

(* attempt to initiate communication with rspace, called by listener *)
and open_link_accepted space rspace link =  match rspace.link with
| Connected _ -> (* lost race against other connectors *)
    close_link_accepted link      
| DeadConnection  -> (* Re-open a dead connection *)
(*DEBUG*)debug1 "OPEN LINK ACCEPTED" "FOUND DEAD" ;
    begin try Join_link.close link
    with Join_link.Failed -> () end        
(* Race, including race with partner connector *)
| Connecting (mtx,_)
| NoConnection mtx ->
    Mutex.lock mtx ;
    match rspace.link with
    | Connected _ | DeadConnection -> (* lost race indeed *)
        Mutex.unlock mtx ;
        close_link_accepted link
    | Connecting (_,cond) -> (* race with partner *)
        (* partner necessarily is in same 'Connecting' state,
           since I have accepted its connect *)
        let c = compare_space (get_id space) rspace.rspace_id in
        if c < 0 then begin
          Mutex.unlock mtx ; (* stay in connecting state *)
          close_link_accepted link
        end else if c > 0 then begin
          (* I have replaced sender (it got false), should do its work *)
          Condition.broadcast cond ;
          finally_open_link_accepted space rspace link mtx
        end else assert false
    | NoConnection _ ->
        finally_open_link_accepted space rspace link mtx

(* once a connection with rspace is established,
   treat incomming messages *)
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
    | Service (key, kid, v) ->
        let v = localize_rec space v
        and fwd arg =
          let uid = find_service space key in
          let g = find_sync_forwarder space uid in
          g arg in
        call_sync space rspace kid fwd v
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
| Join_link.Failed ->
(*DEBUG*)debug1 "HANDLER" "input operation failed" ;
    close_link space rspace
| e ->
(*DEBUG*)debug0 "BUG IN HANDLER"
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
    sender_work space rspace link (do_msg (globalize_rec space a []))
  with NoLink -> ()

and remote_reply_to space rspace kid a =
  do_remote_send space rspace
    (fun v -> ReplySend (kid, v)) a

and remote_reply_to_exn space rspace kid e =
  try
    let link = get_link space rspace in
    sender_work space rspace link
      (ReplyExn (kid, e))
  with NoLink -> ()

and close_link space rspace = match rspace.link with
| NoConnection _|Connecting (_,_) -> assert false
| DeadConnection -> ()
| Connected (_, mtx) ->
(* race condition between concurrent calls to close_link,
   close_link can be called
    - By the unique join_handler
    - By all threads that send remote messages through sender_work *)
    Mutex.lock mtx ;
    match rspace.link with
    | NoConnection _|Connecting (_,_) -> assert false
    | DeadConnection -> Mutex.unlock mtx
    | Connected (link,_) ->
	rspace.link <- DeadConnection ;
	Mutex.unlock mtx ;
(*DEBUG*)debug1 "CLOSE LINK" "starting" ;
 (* assumes failure of 'close link' means double call *)
	begin try Join_link.close link
	with Join_link.Failed -> assert false end ;
(*DEBUG*)debug1 "CLOSE LINK" "link closed" ;
(* For replies not made *)
	Join_hash.iter_empty rspace.konts
	  (fun _ k -> Join_scheduler.reply_to_exn JoinExit k) ;
(*DEBUG*)debug1 "CLOSE LINK" "replies flushed" ;
(* and for async hooks *)
        List.iter
          (fun chan ->  send_async_gen_ref.async_gen chan ())
          rspace.hooks ;
        rspace.hooks <- [] ; (* no risk to look again at hooks anyway *)
(*DEBUG*)debug1 "CLOSE LINK" "cleanup is over" ;
        ()


(* First race between senders, mtx is locked! *)
and open_link_sender space rspace mtx =  match rspace.link with
  | DeadConnection  ->    (* lost and connection already dead... *)
      Mutex.unlock mtx ; raise NoLink
  | Connected (link,_) ->  (* lost and connection complete *)
(*DEBUG*)debug1 "OPEN SENDER" "lost race, connection done" ;
      Mutex.unlock mtx ; link
  | Connecting (_,cond) -> (* lost against other sender *)
(*DEBUG*)debug1 "OPEN SENDER" "lost race, waiting" ;
      Condition.wait cond mtx ;
      open_link_sender space rspace mtx
  | NoConnection _ -> (* won *)
(*DEBUG*)debug1 "OPEN SENDER" "won race" ;
      let cond = Condition.create () in
      rspace.link <- Connecting (mtx, cond) ;
      Mutex.unlock mtx ;
      let r_addr = rspace.rspace_id in
      let rec attempt_connect d =
        try Join_port.connect r_addr
        with Join_port.Failed msg ->
          if d > 5.0 then 
            failwith
              (sprintf "cannot connect to %s: %s"
                 (string_of_sockaddr r_addr) msg)
          else
            Thread.delay d ; attempt_connect (2.0 *. d) in
      let link = attempt_connect 0.1 in
      Join_message.output_value link (get_id space) ;
      Join_link.flush link ;
      let accepted =
        try (Join_message.input_value link : bool)
        with Join_link.Failed ->
(*DEBUG*)debug1 "OPEN SENDER" "definitively rejected" ;       
          begin
            try Join_link.close link with Join_link.Failed -> ()
          end ;
          failwith
            (sprintf
               "%s does not accept me with identity %s"
               (string_of_sockaddr r_addr)
               (string_of_sockaddr (get_id space))) in
      if accepted then begin
(*DEBUG*)debug1 "OPEN SENDER" "finally accepted" ;
        Mutex.lock mtx ;
        rspace.link <- Connected (link,mtx) ;
        Condition.broadcast cond ;
        Mutex.unlock mtx ;
	Join_scheduler.create_process (join_handler space rspace link) ;
        link
      end else begin (* lost race against partner, listener should connect *)
(*DEBUG*)debug1 "OPEN SENDER" "finally rejected" ;
        begin try Join_link.close link
        with Join_link.Failed -> () end ;
        get_link space rspace 
      end
  
(* Get link for rspace, in case no connection is here yet, create one *)
and get_link space rspace =  match rspace.link with
  | DeadConnection -> raise NoLink
  | Connected (link,_) -> link
  | NoConnection mtx|Connecting (mtx,_) ->
      Mutex.lock mtx ;
      open_link_sender space rspace mtx


and sender_work space rspace link msg =
(*DEBUG*)debug2 "SENDER" ("message for "^string_of_space rspace.rspace_id) ;
  try
    Mutex.lock rspace.write_mtx ;
    Join_message.output_msg link msg ;
    Join_link.flush link ;
    Mutex.unlock rspace.write_mtx
  with Join_link.Failed -> (* This can happen several times, no big deal *)
    Mutex.unlock rspace.write_mtx ;
(*DEBUG*)debug1 "SENDER" "output operation failed" ;
    close_link space rspace  (* since close_link is protected *)

(* returns global identification for stub *)
and export_stub space stub = match stub.stub_tag with
|  Local ->
    let uid = stub.uid and space_id = get_id space in
    if uid <> 0 then space_id, uid
    else begin
    (* race condition, since several threads can be exporting this stub *)
      Mutex.lock space.uid_mutex ;
      let uid = stub.uid in
      if uid <> 0 then begin (* lost *)
	Mutex.unlock space.uid_mutex  ;
        space_id, uid
      end else begin (* won, allocate new uid *)
	let uid = space.next_uid () in
	stub.uid <- uid ;
	Mutex.unlock space.uid_mutex ;	
	(* Remember binding uid -> local value *)
	Join_hash.add space.uid2local uid stub.stub_val ;
	space_id, uid
      end
    end
| Remote ->
    let rspace = (Obj.magic stub.stub_val : space_id ) in
    (rspace, stub.uid)

(* quasi-reverse of export stub, from global names to
   values, note that the stub is allocated by
   do_localize_message *)

and import_stub space (rspace_id, uid) =
  if same_space rspace_id (get_id space) then
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
(* There is a race condition with link destruction
   But the calling thread looks like it gets its JoinExit exception ? *)
  Join_hash.add rspace.konts kid kont ; (* do it before getting the link *)
  let link =
    try get_link space rspace
    with NoLink ->
      Join_hash.remove rspace.konts kid ; (* safe if absent *)
      raise Join_misc.JoinExit in
  sender_work space rspace link (do_msg kid (globalize_rec space a [])) ;
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

(* Services supply RCP by name *)

let do_register_service space key f =
(*DEBUG*)debug1 "REGISTER_SERVICE" key ;
(* Alloc some uid, but without a stub *)
  Mutex.lock space.uid_mutex ;
  let uid = space.next_uid () in
  Mutex.unlock space.uid_mutex ;
  Join_hash.add space.uid2local uid (Obj.magic f : stub_val) ;
  match Join_hash.add_once space.services key uid with
  | None -> ()
  | Some _ ->
      Join_hash.remove space.uid2local uid ;
      failwith
	(sprintf "register_service: %s already exists" key)

let register_service key stub =
  do_register_service local_space key stub

let do_call_service space rspace_id key kont a =
  if same_space (get_id space) rspace_id then
    let uid = find_service space key in
    let f = find_sync_forwarder space uid in
    f a
  else begin
(*DEBUG*)debug2 "RCALL SERVICE" key ;    
    do_remote_call space rspace_id
      (fun kid v -> Service (key, kid, v))
      kont a
  end
let call_service space_id key kont a =
  do_call_service local_space  space_id key kont a

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

(************************)
(* explicit connections *)
(************************)

let listen addr = start_listener local_space addr

let here () = get_id local_space

let do_connect space fd =
  let link = Join_link.create fd in
  try
    let id = get_id space in
    Join_message.output_value link id ; Join_link.flush link ;
    let oid = Join_message.input_value link in
    Join_message.output_value link true ; Join_link.flush link ;
    let accepted = (Join_message.input_value link : bool) in
    assert accepted ;
    let rspace = get_remote_space space oid in
    let mtx =
      match rspace.link with
      | NoConnection mtx -> mtx
      | Connecting (_,_)|Connected (_,_)|DeadConnection -> assert false in
    rspace.link <- Connected (link,mtx) ;
    Join_scheduler.create_process (join_handler space rspace link)
  with e ->
    failwith (sprintf "connect: %s" (exn_to_string e))
    
let connect fd = do_connect local_space fd  
  

(*************************************)
(* Async hooks on distant site death *)
(*************************************)

let rec do_at_fail space rspace_id (hook : unit async) =
  if not (same_space (get_id space) rspace_id) then begin
    let rspace = get_remote_space space rspace_id in
    match rspace.link with
    | DeadConnection ->
        send_async_gen_ref.async_gen hook ()
    | NoConnection mtx|Connected (_,mtx)|Connecting (mtx,_) ->
        (* race condition with itself, open_link and close_link *)
        Mutex.lock mtx ;
        match rspace.link with
        | DeadConnection ->
            Mutex.unlock mtx ;
            send_async_gen_ref.async_gen hook ()
        | Connecting (_,_) | Connected (_,_) ->
            rspace.hooks <- hook :: rspace.hooks ;
            Mutex.unlock mtx
        | NoConnection _ -> (* connect, just to test liveness! *)
            ignore (open_link_sender space rspace mtx) ;
            do_at_fail space rspace_id hook
  end

let at_fail rspace_id hook = do_at_fail local_space rspace_id hook

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

