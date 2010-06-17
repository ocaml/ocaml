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
open Join_types

external do_is_bytecode  : unit -> bool = "caml_is_bytecode"

let is_bytecode = do_is_bytecode ()

let rec string_of_sockaddrs = function
  | [] -> ""
  | [r] -> string_of_sockaddr r
  | r::rs -> string_of_sockaddr r ^","^string_of_sockaddrs rs

(* Connection to remote listener: simple timeout technique *)

exception ConnectFailed of string

let rec attempt_connect r_addr d =
  try Join_port.connect r_addr
  with Join_port.Failed (msg,_) ->
    if d > 5.0 then begin
(*DEBUG*)debug0 "ATTEMPT CONNECT" "failed: %s" msg ;
      raise (ConnectFailed msg)
    end else
      Thread.delay d ; attempt_connect r_addr (2.0 *. d)

let same_addr (s1:Unix.sockaddr) s2 = Pervasives.compare s1 s2 = 0

let same_space (s1:space_id) s2 = Pervasives.compare s1 s2 = 0

let compare_space (s1:space_id) s2 = Pervasives.compare s1 s2
 

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
  space_id =
    { host = local_name ;
      uniq = (Unix.getpid (), Unix.gettimeofday ()) ; } ;
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

let space_to_string { host=host ; uniq=(pid,_) } =
  host ^ ":" ^ string_of_int pid

let string_of_space = space_to_string 

let add_routes t routes = match routes with
| [] -> ()
| _::_ -> Join_set.adds t routes

let create_remote_space id originator routes =
(*DEBUG*)debug1 "RSPACE CREATE" "%s" (space_to_string id) ;
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
    originator = originator ;
    route = Join_set.from_list routes ;
    write_mtx = Mutex.create () ;
    hooks = [] ;
  }

let make_remote_space space space_id originator routes =
  try
    let r = Join_hash.find space.remote_spaces space_id in
    add_routes r.route routes
  with Not_found ->
    let new_rspace = create_remote_space space_id originator routes in
    match
      Join_hash.add_once space.remote_spaces space_id new_rspace
    with
    | Some r ->
(*DEBUG*)debug1 "MAKE_REMOTE" "recreation of %s" (space_to_string space_id) ;
	add_routes r.route routes
    | None -> ()

(* Get remote space from its name, should normally exist
   since a previous import_stub or LISTENER
   called make_remote_space *)

let get_remote_space space space_id =
  try Join_hash.find space.remote_spaces space_id
  with Not_found -> assert false


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


let verbose_close caller fd =  
(*DEBUG*)debug1 ("CLOSE from "^caller) "%i" (Obj.magic fd) ;
  try
    Unix.close fd
  with e ->
(*DEBUG*)debug1 ("CLOSE from "^caller) "%s" (Join_misc.exn_to_string e) ;
    ()

exception NoLink

let string_of_addr = function
  | None -> "default"
  | Some a -> string_of_sockaddr a

(*
let check_addr id addro = match addro with
| None -> ()
| Some addr ->
    if not (same_addr id addr) then
      failwith
        (sprintf "attempt to listen on %s, already running as %s"
           (string_of_sockaddr addr) (string_of_sockaddr id))
*)

exception RoutingFailed

type connect_msg =
  | Query
  | Connect of space_id * string * bool * route list

and connect_answer =
  | Ok
  | VersionMismatch
  | ByteNative

exception ConnectMismatch of connect_answer


(* there can be several listeners *)
let rec start_listener space addr = match space.listener with
| Listen socks ->
    let addr = do_start_listener space (Some addr) in
    Join_set.add socks addr
| Deaf mtx ->
    Mutex.lock mtx ;
    match space.listener with
    | Listen sock ->
	Mutex.unlock mtx ;
	start_listener space addr
    | Deaf _ ->
        begin try
	  let _ = do_start_listener space (Some addr) in
	  space.listener <- Listen (Join_set.singleton addr)
        with e ->
	  Mutex.unlock mtx ;
          raise e
        end ;
	Mutex.unlock mtx
          
(* anonymous listener is started at most once *)
and start_anonymous_listener space = match space.listener with
| Listen sock -> ()
|  Deaf mtx ->
    Mutex.lock mtx ;
    match space.listener with
    | Listen _ ->
	Mutex.unlock mtx ;
	()
    | Deaf _ ->
	begin try
	  let addr = do_start_listener space None in
	  space.listener <- Listen (Join_set.singleton addr)
	with e ->
	  Mutex.unlock mtx ;
          raise e
	end ;
	Mutex.unlock mtx


and do_start_listener space addr =
  let when_accepted link =
    let rec get_rspace () =	    
      let msg = (Join_message.input_value link : connect_msg) in
      match msg with
      | Query ->
(*DEBUG*)debug1 "LISTENER" "QUERIED" ;
	  let my_id = get_id space in
	  Join_message.output_value link my_id ;
(*DEBUG*)debug1 "LISTENER" "ANSWERED %s" (string_of_space my_id) ;
	  Join_link.flush link ;
	  get_rspace ()
      | Connect (rspace_id, r_magic, r_isb, routes) ->
(*DEBUG*)debug1 "LISTENER" "his name: %s [%s, routes %s]"
(*DEBUG*) (space_to_string rspace_id) 
(*DEBUG*) (if r_isb then "byte" else "native") (string_of_sockaddrs routes) ;
          let answer =
            if r_magic <> Join_message.magic then VersionMismatch
            else if r_isb <> is_bytecode then ByteNative
            else Ok in
          Join_message.output_value link answer ;
          Join_link.flush link ;
          begin match answer with
          | VersionMismatch|ByteNative -> raise (ConnectMismatch answer)
          | Ok -> ()
          end ;
	  make_remote_space space rspace_id rspace_id routes ;
	  get_remote_space space rspace_id  in	  
    try
      let rspace = get_rspace () in
      Join_message.output_value link
        (routes_for space rspace.rspace_id (get_routes space)) ;
      Join_link.flush link ;
      open_link_accepted space rspace link
    with
    | ConnectMismatch _ ->
(*DEBUG*)debug1 "LISTENER" "%s" "Mismatch" ;
        (try Join_link.close link with _ -> ()) ;
	()
    | Join_link.Failed (*From get_rspace*) ->
(*DEBUG*)debug1 "LISTENER" "%s" "First message not available" ;
	(try Join_link.close link with _ -> ()) ;
	() in
  let my_addr,_ = Join_port.establish_server addr when_accepted in
  my_addr


and get_id space = space.space_id

and get_routes space = match space.listener with
| Listen routes -> Join_set.elements routes
| Deaf _ -> start_anonymous_listener space ; get_routes space

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
          (* I have replaced my sender (it got false), should do its work *)
          Condition.broadcast cond ;
          finally_open_link_accepted space rspace link mtx
        end else assert false
    | NoConnection _ ->
        finally_open_link_accepted space rspace link mtx

(* once a connection with rspace is established,
   treat incomming messages *)
and join_handler space rspace link () =
  let rspace_id = rspace.rspace_id in
(*DEBUG*)debug1 "HANDLER"
(*DEBUG*)  "start receiving from %s" (string_of_space rspace_id) ;
  try
    while true do
      let msg = Join_message.input_msg link in
(*DEBUG*)debug2 "HANDLER"
(*DEBUG*) "message [%s] from %s"
(*DEBUG*)    (Join_message.string_of_msg msg)
(*DEBUG*)    (string_of_space rspace.rspace_id) ;
      match msg with
      | AsyncSend (chan, v) ->
	  let auto = find_automaton space chan.auto_id
	  and v = localize_rec space rspace_id v in
	  send_async_ref.async auto chan.chan_id v
      | AloneSend (uid,v) ->
          let g = find_async_forwarder space uid
          and v = localize_rec space rspace_id v in
(* inlined async call, must match local_send_alone in join.ml *)
          Join_scheduler.create_process (fun () -> g v)
      | SyncSend (chan, kid, v) ->
	  let auto = find_automaton space chan.auto_id
	  and v = localize_rec space rspace_id v
          and idx = chan.chan_id in
          call_sync space rspace kid
            (fun v -> send_sync_ref.sync auto idx v) v
      | AloneSyncSend (uid, kid, v) ->
          let g = find_sync_forwarder space uid
          and v = localize_rec space rspace_id v in
          call_sync space rspace kid g v
      | Service (key, kid, v) ->
          let v = localize_rec space rspace_id v
          and fwd arg =
            let uid = find_service space key in
            let g = find_sync_forwarder space uid in
            let r = g arg	 in
	    r in
          call_sync space rspace kid fwd v
      | ReplySend (kid, v) ->
	  let kont =
	    try Join_hash.find_remove rspace.konts kid
	    with Not_found -> assert false
	  and v = localize_rec space rspace_id v in
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
(*DEBUG*)  "died of %s" (Join_misc.exn_to_string e) ;
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


(* 
   - When outlink is dead, messages are discarded silently
   exception handler for NoLink

   - If blocking is not allowed, a process is created, since
   opening the link or sending on it may block (or even fail)
 *)

and blocking_remote_send space rspace do_msg a =
  try
    let link = get_link space rspace in
    sender_work space rspace link
      (do_msg (globalize_rec space a globalize_flags))
  with NoLink -> ()

and do_remote_send may_block space rspace do_msg a =
  if may_block then
    blocking_remote_send space rspace do_msg a
  else
    Join_scheduler.create_process
      (fun () -> blocking_remote_send space rspace do_msg a)


(* Remote replies are not exported, and always called inside a fresh process *)
and remote_reply_to space rspace kid a =
  blocking_remote_send space rspace (fun v -> ReplySend (kid, v)) a

and remote_reply_to_exn space rspace kid e =
  try
    let link = get_link space rspace in
    sender_work space rspace link (ReplyExn (kid, e))
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

(* Find route to remote space *)

(*
  and find_route space rspace = match find_routes space rspace with
  | [] ->
(*DEBUG*)debug0 "FIND ROUTE" "unreachable site: %s"
(*DEBUG*)  (string_of_space rspace.rspace_id) ;
  raise RoutingFailed
  | r::_ -> r
 *)

and find_routes space rspace =
(*DEBUG*)debug1 "FIND ROUTES" "to %s" (string_of_space rspace.rspace_id) ;
  let routes =  Join_set.elements rspace.route in
  match routes with
  | [] ->
(*DEBUG*)debug1 "FIND ROUTES" "to %s, by originator %s"
(*DEBUG*) (string_of_space rspace.rspace_id)
(*DEBUG*) (string_of_space rspace.originator) ;
      let new_routes =
	call_route_service space rspace.originator
	  (space.space_id,rspace.rspace_id) in
      add_routes rspace.route new_routes ;
(*DEBUG*)debug1 "FIND ROUTES" "to %s, found %s"
(*DEBUG*)  (string_of_space rspace.rspace_id)
(*DEBUG*)  (string_of_sockaddrs new_routes) ;
      new_routes
  | _::_ -> routes (* Easy case *)

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
    try
      let routes = find_routes space rspace in
      let rec get_link = function 
        | [] -> raise RoutingFailed
        | [route] ->  attempt_connect route 0.1, route
        | route::rem ->
(*DEBUG*)debug1 "GET_LINK" "try route %s" (string_of_sockaddr route) ;
            begin try
              attempt_connect route 0.1, route
            with
            | ConnectFailed _ -> get_link rem
            end in
      let link,route = get_link routes in
      connect_on_link  space rspace (mtx, cond) link route
    with
    |	RoutingFailed|ConnectFailed _ as _e ->
(*DEBUG*)debug1 "OPEN SENDER" "failed on %s" (Printexc.to_string _e) ;
        Mutex.lock mtx ;
        rspace.link <- DeadConnection ;
        Condition.broadcast cond ;
        Mutex.unlock mtx ;
        raise NoLink

and connect_on_link space rspace (mtx, cond) link route =
  Join_message.output_value link
    (Connect
       (get_id space, Join_message.magic, is_bytecode, get_routes space)) ;
  Join_link.flush link ;

  let accepted =
    let destroy_sender () =
      begin try Join_link.close link with Join_link.Failed -> () end ;
(*DEBUG*)debug1 "OPEN SENDER" "Distroy" ;
      Mutex.lock mtx ;
      rspace.link <- DeadConnection ;
      Condition.broadcast cond ;
      Mutex.unlock mtx in
    
    try
      let answer = (Join_message.input_value link : connect_answer)  in
      begin match answer with
      | Ok -> ()
      | ByteNative|VersionMismatch -> raise (ConnectMismatch answer)
      end ;
      let more_routes =  (Join_message.input_value link : route list) in
      add_routes rspace.route more_routes ;
      (Join_message.input_value link : bool)
    with
    | ConnectMismatch answer ->
(*DEBUG*)debug1 "OPEN SENDER" "Mismatch" ;       
        destroy_sender () ;
        failwith
          (sprintf "connection attempt failed: %s"
             (match answer with
             | ByteNative -> "byte against native"
             | VersionMismatch -> "different JoCaml versions"
             | Ok -> assert false))
    | Join_link.Failed ->
(*DEBUG*)debug1 "OPEN SENDER" "definitively rejected" ;       
        destroy_sender () ;
        failwith
          (sprintf
             "%s does not accept me with identity %s, on route %s"
	     (string_of_space rspace.rspace_id)
             (string_of_space (get_id space))
             (string_of_sockaddr route)) in
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
(*DEBUG*)debug2 "SENDER" "message for %s" (string_of_space rspace.rspace_id) ;
  try
    Mutex.lock rspace.write_mtx ;
    Join_message.output_msg link msg ;
    Join_link.flush link ;
    Mutex.unlock rspace.write_mtx
  with Join_link.Failed -> (* This can happen several times, no big deal *)
    Mutex.unlock rspace.write_mtx ;
(*DEBUG*)debug1 "SENDER" "%s" "output operation failed" ;
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

and import_stub space originator (rspace_id, uid) =
  if same_space rspace_id (get_id space) then
    Local, find_local space uid, uid
  else begin
    make_remote_space space rspace_id originator [] ;
    Remote, (Obj.magic rspace_id : stub_val), uid
  end

and globalize_rec space v flags =
  let s,t =  do_globalize_message v flags in
  s, Array.map (export_stub space) t

and localize_rec space originator (s,t) =
  do_localize_message s (Array.map (import_stub space originator) t)

(**************************************************)    
(*  Synchronous message sending is in big let rec *)
(*  because of calls to "joroute" service above   *)
(**************************************************)

(* When outlink is dead, get_link raises NoLink
   then, sync calls can fail the join way,
   this will make all tasks waiting replies to die silently *)

and do_remote_call space rspace_id do_msg kont a =
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
  sender_work space rspace link
    (do_msg kid (globalize_rec space a globalize_flags)) ;
  Mutex.lock kont.kmutex ;
  Join_scheduler.suspend_for_reply kont

and do_call_service space rspace_id key a =
  if same_space (get_id space) rspace_id then
    let uid = find_service space key in
    let f = find_sync_forwarder space uid in
    f a
  else begin
(*DEBUG*)debug1 "RCALL SERVICE" "%s" key ; 
    let kont = Join_scheduler.kont_create (Mutex.create ()) in
    do_remote_call space rspace_id
      (fun kid v -> Service (key, kid, v))
      kont a
  end

and call_route_service space rspace_id a =
  let routes =
    try
      Obj.magic
        (do_call_service space rspace_id "joroute" (Obj.magic a : 'a))
    with _ -> raise RoutingFailed in
  (routes : route list)

and route_service space ((ask_id,rspace_id):(space_id * space_id)) =
  if same_space rspace_id space.space_id then
    (get_routes space)
  else
    let rs = find_routes space (get_remote_space space rspace_id) in
    routes_for space ask_id rs

and routes_for space ask_id rs =
  let host_ask = ask_id.host
  and me = space.space_id.host in
  if me = host_ask then rs
  else
    List.filter
      (fun sock -> match sock with
      | Unix.ADDR_INET (h,_) ->
	  h <> Unix.inet_addr_loopback &&
	  h <> Unix.inet6_addr_loopback
      | _ -> false)
      rs

(***********************************)
(* Various remote message sendings *)
(***********************************)

let do_remote_send_async may_block space rspace_id uid idx a =
  do_remote_send may_block space (get_remote_space space rspace_id)
    (fun v -> AsyncSend ({auto_id=uid; chan_id=idx;}, v)) a

let remote_send_async may_block rspace uid idx a =
(*DEBUG*)debug3 "REMOTE" "SEND ASYNC (%s) " (if may_block then "MAY_BLOCK" else "BLOCK NOT ALLOWED") ;
  do_remote_send_async may_block local_space rspace uid idx a
    
let do_remote_send_alone  may_block space rspace_id uid a =
  do_remote_send  may_block space (get_remote_space space rspace_id)
    (fun v -> AloneSend (uid, v)) a

let remote_send_alone  may_block rspace_id uid a =
(*DEBUG*)debug3 "REMOTE" "SEND ALONE (%s)" (if may_block then "MAY_BLOCK" else "BLOCK NOT ALLOWED") ;
  do_remote_send_alone may_block local_space rspace_id uid a

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
(*DEBUG*)debug1 "REGISTER_SERVICE" "%s" key ;
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
	(sprintf "register_service: \"%s\" already exists" key)

let register_service key stub = do_register_service local_space key stub

let () = register_service "joroute"
    (route_service local_space : (space_id * space_id) -> route list)


let call_service space_id key a =
  do_call_service local_space  space_id key a

(*
  This connection attempt is quite special, since the remote structure
  may not exist yet. In fact we may not even know the remote id
*)
let do_rid_from_addr space addr =
  let my_routes = get_routes space in
  if List.exists (same_addr addr) my_routes then
    get_id space
  else begin
(*DEBUG*)debug1 "RID"
(*DEBUG*)  "GET COMPLETE ID OF %s" (string_of_sockaddr addr) ;
    let link =
      try attempt_connect addr 0.1
      (* Signal distant site is dead since
         rid_from_addr is a function *)
      with ConnectFailed msg -> failwith msg in
    let rid =
      try
	Join_message.output_value link Query ;
	Join_link.flush link ;
	(Join_message.input_value link : space_id )
      with Join_link.Failed ->
        begin try Join_link.close link with _ -> () end ;
	failwith
	  (sprintf "cannot query %s for its complete name"
	     (string_of_sockaddr addr)) in
(*DEBUG*)let _,stamp = rid.uniq in
(*DEBUG*)debug1 "RID" "GOT COMPLETE ID OF %s+%f"
(*DEBUG*)  (string_of_space rid) stamp ;
(* Now, we can create the remote space struture *)
    make_remote_space space rid rid [addr] ;
(* So as not to waste link just established, do actual Connect *)
   let _link = try
      let rspace = get_remote_space space rid in
      match rspace.link with
      | DeadConnection | Connected (_,_)
      | Connecting (_,_) ->
(*DEBUG*)debug1 "RID" "lost race for connecting to %s"
(*DEBUG*)  (string_of_space rid) ;	    
	  begin try Join_link.close link with _ -> () end ;
	  get_link space rspace
      | NoConnection mtx ->
	  Mutex.lock mtx ;
	  match rspace.link with
	  | DeadConnection | Connected (_,_)
	  | Connecting (_,_) ->
(*DEBUG*)debug1 "RID" "lost race (II) for connecting to %s"
(*DEBUG*)  (string_of_space rid) ;	    
              (* lost race, no need to wait on condition here *)
	      Mutex.unlock mtx ;
	      get_link space rspace
	  | NoConnection _ ->
(*DEBUG*)debug1 "RID" "won race for connecting to %s"
(*DEBUG*)  (string_of_space rid) ;	    
	      let cond = Condition.create () in
	      rspace.link <- Connecting (mtx, cond) ;
	      Mutex.unlock mtx ;
              connect_on_link space rspace (mtx,cond) link addr
    (* Signal distant site is dead since
       rid_from_addr is a function *)
      with NoLink -> raise Join_misc.JoinExit in
    rid
  end

let rid_from_addr addr = do_rid_from_addr local_space addr

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
and localize v = localize_rec local_space local_space.space_id v

(************************)
(* explicit connections *)
(************************)

let listen addr = start_listener local_space addr

let here = get_id local_space

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
            begin try
              ignore (open_link_sender space rspace mtx)
            with NoLink -> () end ;
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
 *)

let flush_out_queue space rspace =
(* First wait for replies to be passed to out queue *)
(*DEBUG*)debug2 "WAIT PENDING REPLIES" "%s"
(*DEBUG*)  (space_to_string rspace.rspace_id) ;
  wait_zero rspace.replies_pending ;
(*DEBUG*)debug2 "NO PENDING REPLIES" "%s"
(*DEBUG*)  (space_to_string rspace.rspace_id) ;
  ()

let do_flush_out_queues space =
(*DEBUG*)debug2 "FLUSH SPACE" "enter" ;
  Join_hash.iter space.remote_spaces
    (fun _ rspace -> flush_out_queue space rspace)

let flush_space () =  do_flush_out_queues local_space

(************************)
(* Access to the routes *)
(************************)
let get_sockaddrs () = get_routes local_space
