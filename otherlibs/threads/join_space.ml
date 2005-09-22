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

open Unix
open Printf
(*DEBUG*)open Join_debug

let creation_time = gettimeofday ();;
let localname = gethostname ();;
let first_addr entry = entry.h_addr_list.(0);;

let localname = (* try to bind a socket to a INET port, if not possible,
use loopback (127.0.0.1) instead... *)
  let s = socket PF_INET SOCK_STREAM 0 in
  setsockopt s SO_REUSEADDR true;  
  let localaddr = 
    try
      first_addr (gethostbyname localname) 
    with
    | _ -> inet_addr_of_string "127.0.0.1"
    in
  let localname = 
    try
      bind s (ADDR_INET (localaddr, 0));
      localname
    with
      _ -> 
        eprintf "WARNING: Using loopback address (127.0.0.1). you will not be able to communicate with other machines. Verify your network connection if this is not normal.";
        print_newline ();
        let localaddr = inet_addr_of_string "127.0.0.1" in
        bind s (ADDR_INET (localaddr, 0));
        "localhost"
  in
  close s;
  localname

let localaddr = 
  try
    first_addr (gethostbyname localname)
  with
  | _ -> inet_addr_of_string "127.0.0.1"
;;

let local_addr = localaddr

let create_port port =
  handle_unix_error
    (fun () ->
      let s = socket PF_INET SOCK_STREAM 0 in
      let _ =
        try
          setsockopt s SO_REUSEADDR true;
          bind s (ADDR_INET (localaddr, port));
          listen s 5;
          ()
        with z -> close s ; raise z
      in
      let saddr = getsockname s in
      match saddr with
        ADDR_INET (_, port) -> port, s
      | _ -> assert false) ()
;;

let rec accept s =
  begin try
    Unix.accept s
  with
  | Unix_error((EAGAIN|EINTR),_,_) -> 
(*DEBUG*)debug0 "accept" "try again" ;
      accept s
  end


(* Create site socket *)
let local_port, local_socket = create_port 0
;;

(* And compute global site identifier *)
let local_id = localaddr, local_port, creation_time
;;

let same_space (a1, p1, t1) (a2, p2, t2) =
  a1 = a2 && p1 = p2 && t1 = t2
;;

open Join_types

(* Describe site *)
let local_space = {
  space_id = local_id ;
  space_mutex = Mutex.create () ;
  next_uid =
    begin 
      let uid_counter = ref 0 in
      (fun () -> incr uid_counter; !uid_counter)
    end ;
  uid2local = Hashtbl.create 13 ;
  remote_spaces = Hashtbl.create 13 ;
  space_listener = Deaf local_socket ;
} 
;;

let do_locked2 lock zyva a b =
  Mutex.lock lock ;
  let r =
    try zyva a b with e -> Mutex.unlock lock ; raise e in
  Mutex.unlock lock ;
  r
;;

let get_remote_space space space_id =
  try Hashtbl.find space.remote_spaces space_id
  with Not_found ->
    let r = {
      rspace_id = space_id ;
      link_in = NoHandler ;
      link_out = NoConnection ;
      } in
    Hashtbl.add space.remote_spaces space_id r ;
    r
;;

let start_listener space =
  Mutex.unlock space.space_mutex ;
  begin match space.space_listener with
  | Deaf sock ->
      let listener () =
        while true do
          let s,_ = accept sock in
          shutdown s SHUTDOWN_SEND ;
          let inc = in_channel_of_descr s in
          let rspace_id = input_value inc in
          let rspace = 
            do_locked2 space.space_mutex
              get_remote_space space rspace_id in
          begin match rspace.link_in with
          | NoHandler ->  assert false
          | Handler _ -> assert false
          end
        done in
      ()
  | _ -> assert false
  end ;
  Mutex.lock space.space_mutex

let export_stub space local =  match local with
  | LocalAutomaton a ->
      if a.ident > 0 then
        GlobalAutomaton (space.space_id, a.ident)
      else begin
        Mutex.lock space.space_mutex ;
        let r =
          (* Race condition *)
          if a.ident > 0 then
            GlobalAutomaton (space.space_id, a.ident)
          else begin
            let uid = space.next_uid () in
(* Listener is started  at first export *)
            if uid = 1 then start_listener space ;

            Hashtbl.add space.uid2local uid a ;
            a.ident <- uid ;
            GlobalAutomaton (space.space_id, uid)
          end in
        Mutex.unlock space.space_mutex ;
        r
      end
  | RemoteAutomaton (rspace, uid) ->
      GlobalAutomaton (rspace.rspace_id, uid)

external alloc_stub : t_local -> stub = "caml_alloc_stub"

let import_remote_automaton space space_id uid =
  let rspace =
    do_locked2 space.space_mutex
      get_remote_space space space_id in
  RemoteAutomaton (rspace, uid)

let get_local_automaton t uid =
  try LocalAutomaton (Hashtbl.find t uid)
  with Not_found -> assert false

let import_stub space local = match local with
| GlobalAutomaton (space_id, uid) ->
    if same_space space_id space.space_id then begin
      do_locked2 space.space_mutex
        get_local_automaton space.uid2local uid
    end else
      import_remote_automaton space space_id uid


  
external do_marshal_message :
  'a -> Marshal.extern_flags list -> string * (Join_types.t_local) array
  = "caml_marshal_message" 

let marshal_message v flags =
  let s,t =  do_marshal_message v flags in
  s, Array.map (export_stub local_space) t

external do_unmarshal_message :
  string -> (Join_types.t_local) array -> 'a
  = "caml_unmarshal_message" 

let unmarshal_message (s,locals) =
  do_unmarshal_message s (Array.map (import_stub local_space) locals)
