(***********************************************************************)
(*                                                                     *)
(*                    ______________                                   *)
(*                                                                     *)
(*      Fabrice Le Fessant, projet SOR/PARA INRIA Rocquencourt         *)
(*      Luc Maranget, projet Moscova                                   *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Printf
open Join_misc
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
  space_mutex = Mutex.create () ;
  space_status = SpaceUp ;
  next_uid =
    begin 
      let uid_counter = ref 0
      and uid_mutex = Mutex.create () in
      (fun () ->
        Mutex.lock uid_mutex ;
        incr uid_counter;
        let r = !uid_counter in
        Mutex.unlock uid_mutex ;
        r)
    end ;
  uid2local = Join_hash.create () ;
  remote_spaces =  Join_hash.create () ;
  space_listener = Deaf (local_socket, Mutex.create ())
} 
;;

let do_locked2 lock zyva a b =
  Mutex.lock lock ;
  let r =
    try zyva a b with e -> Mutex.unlock lock ; raise e in
  Mutex.unlock lock ;
  r


let get_remote_space space space_id =
  Join_hash.get space.remote_spaces
    (fun id ->
      {
        rspace_id = id ;
        next_kid =
          begin
	    let kid_counter = ref 0
	    and kid_mutex = Mutex.create () in
	    (fun () ->
	      Mutex.lock kid_mutex ;
	      let r = !kid_counter in
	      incr kid_counter ;
	      Mutex.unlock kid_mutex ;
	      r)
	  end ;
        konts = Join_hash.create () ;
        link_in = NoHandler ;
        link_out = NoConnection (Mutex.create ()) ;
       })
    space_id

let import_remote_automaton space space_id uid =
  let rspace = get_remote_space space space_id in
  RemoteAutomaton (rspace, uid)

let find_local_automaton space uid =  
  try Join_hash.find space.uid2local uid
  with Not_found -> assert false

type async_ref =
  { mutable async : 'a . automaton -> int -> 'a -> unit }
let send_async_ref = { async =  (fun _ _ _ -> assert false) }

type sync_ref =
    { mutable sync : 'a 'b . automaton -> int -> 'a -> 'b}
let send_sync_ref = { sync = (fun _ _ _ -> assert false) }

external do_marshal_message :
  'a -> Marshal.extern_flags list -> string * (t_local) array
  = "caml_marshal_message" 

external do_unmarshal_message :
  string -> (t_local) array -> 'a
  = "caml_unmarshal_message" 

let space_to_string (addr, port, _) =
  sprintf "%s:%i" (Unix.string_of_inet_addr addr) port

let string_of_space = space_to_string 

let verbose_close caller fd =  
(*DEBUG*)debug1 ("CLOSE from "^caller) (sprintf "%i" (Obj.magic fd)) ;
  try
    Unix.close fd
  with e ->
(*DEBUG*)debug1 ("CLOSE from "^caller)
(*DEBUG*) (Join_misc.exn_to_string e) ;
      ()

let close_link_in = function
  | NoHandler -> ()
  | Handler { in_channel = inc } ->
      Unix.shutdown inc Unix.SHUTDOWN_RECEIVE ;
      verbose_close "link_in" inc

let close_link_out = function
  | NoConnection _ | Connecting _ -> ()
  | Connected {out_channel = fd ; out_queue = queue } ->
      Join_queue.put queue GoodBye

and internal_close_link_out = function
  | NoConnection _ | Connecting _ -> ()
  | Connected {out_channel = fd ; out_queue = queue } ->
      Join_queue.put queue Killed


exception SawGoodBye
exception SawKilled
  
let rec start_listener space =
  begin match space.space_listener with
  | Deaf (sock,mtx) ->
      let rec do_rec () =
	Mutex.lock mtx ;
	begin match  space.space_listener with
	| Deaf (sock,mtx) ->
	    let listener () =
	      try
              while true do
(*DEBUG*)debug1 "LISTENER"
(*DEBUG*)  (sprintf "now accept on: %s" (space_to_string space.space_id)) ;
		let s,_ = Join_misc.force_accept sock in
(*DEBUG*)debug1 "LISTENER" "someone coming" ;
		Unix.shutdown s Unix.SHUTDOWN_SEND ;
		let inc = Unix.in_channel_of_descr s in
		let rspace_id = input_value inc in
(*DEBUG*)debug1 "LISTENER" ("his name: "^space_to_string rspace_id)  ;
		let rspace =  get_remote_space space rspace_id in
		begin match rspace.link_in with
		| Handler _ -> assert false
		| NoHandler ->
		    rspace.link_in <-
                      Handler {in_channel = s ; }
		end ;
		Join_scheduler.create_process
		  (join_handler space rspace s inc)
              done
	    with
(* 'normal' reaction to closing socket internally *)
              | Unix.Unix_error((Unix.EBADF|Unix.EINVAL), "accept", _) ->
(*DEBUG*)debug1 "LISTENER" "saw shutdowned or closed socket" ;
                  ()
	      | e ->
(*DEBUG*)debug0 "LISTENER"
(*DEBUG*)  (sprintf "died of %s" (Join_misc.exn_to_string e)) ;
                  () in
	    Join_scheduler.create_process listener ;
	    space.space_listener <- Listen sock ;
	    Mutex.unlock mtx
	| Listen _ -> ()
	end in
      do_rec ()
  | Listen _ -> ()
  end

and join_handler space rspace s inc () =
(*DEBUG*)debug1 "HANDLER"
(*DEBUG*)  ("start receiving from "^string_of_space rspace.rspace_id) ;  try
  while true do
    let msg = input_value inc in
(*DEBUG*)debug2 "HANDLER" ("message from "^string_of_space rspace.rspace_id) ;
    match msg with
    | AsyncSend (uid, idx, v) ->
        let auto = find_local_automaton space uid
        and v = unmarshal_message_rec space v in
        send_async_ref.async auto idx v
    | SyncSend (uid, idx, kid, v) ->
        let auto = find_local_automaton space uid
        and v = unmarshal_message_rec space v in
	Join_scheduler.create_process
	  (fun () ->
	    let r = send_sync_ref.sync auto idx v in
	    do_remote_reply_to space rspace kid r)
    | ReplySend (kid, v) ->
	let kont =
          try Join_hash.find_remove rspace.konts kid
          with Not_found -> assert false
        and v = unmarshal_message_rec space v in
	Join_scheduler.reply_to v kont
    | GoodBye -> raise SawGoodBye
    | Killed -> assert false
  done
  with
  | SawGoodBye|End_of_file as e ->
    (* Distant site annouce it halts, or halts without annoucing it *)
(*DEBUG*)debug1 "HANDLER"
(*DEBUG*)  (sprintf "site %s tells it halts with %s"
(*DEBUG*)     (space_to_string rspace.rspace_id) (Printexc.to_string e)) ;
      verbose_close "handler self close" s ;
      internal_close_link_out rspace.link_out ;
      Join_hash.remove space.remote_spaces rspace.rspace_id ;
      Join_hash.iter rspace.konts
        (fun _ k -> Join_scheduler.reply_to_exn JoinExit k) ;
(*DEBUG*)debug1 "HANDLER" "cleanup is over" ;
      ()
        
  | Sys_error("Bad file descriptor") ->
(*DEBUG*)debug1 "HANDLER" "Saw closed socket" ;
      ()
  | e ->
(*DEBUG*)debug0 "HANDLER"
(*DEBUG*)  (sprintf "died of %s" (Join_misc.exn_to_string e)) ;
      ()
      

      
and get_out_queue space rspace =
  match rspace.link_out with
  | Connected {out_queue = queue ; } -> queue
  | Connecting queue -> queue
  | NoConnection mtx ->
      (* Outbound connection is set asynchronously,
         hence check race condition, so as to create exactly
         one queue and sender_work process *)
      let rec do_rec () =
        Mutex.lock mtx ;
        begin match rspace.link_out with
        | Connected  {out_queue = queue ; }
        | Connecting queue -> (* Other got it ! *)
            Mutex.unlock mtx ;
            queue
        | NoConnection _ -> (* Got it ! *)
            let queue = Join_queue.create () in
            rspace.link_out <- Connecting queue ;
            Mutex.unlock mtx ;
(* The sender thread is join-scheduled *)
            Join_scheduler.create_process
              (start_sender_work space rspace queue) ;
	    queue end in
      do_rec ()

and close_link_out space rspace = 
  let queue = get_out_queue space rspace in
  Join_queue.put queue GoodBye

and do_remote_reply_to space rspace kid r =
  let queue = get_out_queue space rspace in
  Join_queue.put queue
    (ReplySend
       (kid, marshal_message_rec space r []))

and sender_work rspace queue s outc =
  try 
  while true do
    let msg = Join_queue.get queue in
    begin match msg with
    | Killed -> raise SawKilled
    | _ -> ()
    end ;
(*DEBUG*)debug2 "SENDER" ("message for "^string_of_space rspace.rspace_id) ;
    output_value outc msg ; flush outc ;
    match msg with
    | GoodBye -> raise SawGoodBye
    | _ -> ()	
  done
  with
(* Suicide neatly after announcing it *)
  | SawGoodBye ->
(*DEBUG*)debug1 "SENDER" "just sent goodbye" ;
      Unix.shutdown s Unix.SHUTDOWN_SEND ;
      verbose_close "self close from sender" s
(* The distant site has halted, no need to shutdown *)
  | SawKilled ->
(*DEBUG*)debug1 "SENDER" "just received Killed" ;
      verbose_close "self close from sender" s
  | e ->
(*DEBUG*)debug0 "SENDER" ("died of "^Join_misc.exn_to_string e) ;
      ()
      
and start_sender_work space rspace queue () =
(*DEBUG*)debug1 "SENDER"
(*DEBUG*)    (sprintf "connect from %s to %s"
(*DEBUG*)       (string_of_space space.space_id)
(*DEBUG*)       (string_of_space rspace.rspace_id)) ;
  let addr,port,_ = rspace.rspace_id in
  let s = force_connect addr port in
  Unix.shutdown s Unix.SHUTDOWN_RECEIVE ;
  let outc = Unix.out_channel_of_descr s in
  rspace.link_out <-
     Connected
       { out_queue = queue ;
         out_channel = s ; } ;
  output_value outc space.space_id  ; flush outc ;
(*DEBUG*)debug1 "SENDER"
(*DEBUG*)  (sprintf "just sent my name %s" (string_of_space space.space_id)) ;
  sender_work rspace queue s outc

and export_stub space local =  match local with
  | LocalAutomaton a ->
      if a.ident > 0 then
        GlobalAutomaton (space.space_id, a.ident)
      else begin
        Mutex.lock a.mutex ;
        let r =
          (* Race condition *)
          if a.ident > 0 then begin
            Mutex.unlock a.mutex ;
            GlobalAutomaton (space.space_id, a.ident)
          end else begin
            let uid = space.next_uid () in
            a.ident <- uid ;
            Mutex.unlock a.mutex ;            
(* Listener is started  at first export *)
            if uid = 1 then start_listener space ;
            Join_hash.add space.uid2local uid a ;
            GlobalAutomaton (space.space_id, uid)
          end in
        r
      end
  | RemoteAutomaton (rspace, uid) ->
      GlobalAutomaton (rspace.rspace_id, uid)

and import_stub space local = match local with
| GlobalAutomaton (space_id, uid) ->
    if same_space space_id space.space_id then begin
      LocalAutomaton (find_local_automaton space uid)
    end else
      import_remote_automaton space space_id uid

and marshal_message_rec space v flags =
  let s,t =  do_marshal_message v flags in
  s, Array.map (export_stub space) t

and unmarshal_message_rec space (s,locals) =
  do_unmarshal_message s (Array.map (import_stub space) locals)
  

let marshal_message v flags = marshal_message_rec local_space v flags
and  unmarshal_message v = unmarshal_message_rec local_space v


let do_remote_send_async space rspace uid idx a =
  let queue = get_out_queue space rspace in
  Join_queue.put queue
    (AsyncSend
       (uid, idx, marshal_message_rec space a []))

let remote_send_async rspace uid idx a =
(*DEBUG*)debug3 "REMOTE" "SEND ASYNC" ;
  do_remote_send_async local_space rspace uid idx a
              

let do_remote_send_sync space rspace uid idx kont a =
  let kid = rspace.next_kid ()
  and queue = get_out_queue space rspace in
(*  if kid = 0 then start_listener space ; (* first continuation exported *) *)
  Join_hash.add rspace.konts kid kont ;
  Join_queue.put queue
    (SyncSend (uid, idx, kid,  marshal_message_rec space a [])) ;
  Mutex.lock kont.kmutex ;
  Join_scheduler.suspend_for_reply kont
  
let remote_send_sync rspace uid idx kont a =
(*DEBUG*)debug3 "REMOTE" "SEND SYNC" ;
  do_remote_send_sync local_space rspace uid idx kont a
              


let kill_remote_space space rspace =
  close_link_in rspace.link_in ;
  close_link_out space rspace



let do_halt space =
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

let halt () = do_halt local_space

