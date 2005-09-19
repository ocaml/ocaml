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
        printf "WARNING: Using loopback address (127.0.0.1). you will not be able to communicate with other machines. Verify your network connection if this is not normal.";
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

let create_port port =
  handle_unix_error
    (fun () ->
      let s = socket PF_INET SOCK_STREAM 0 in
      let _ =
        setsockopt s SO_REUSEADDR true;
        bind s (ADDR_INET (localaddr, port));
        listen s 5;
        ()
      in
      let saddr = getsockname s in
      match saddr with
        ADDR_INET (_, port) -> port, s
      | _ -> failwith "not ADDR_INET socket") ()

(* Create site socket *)
let local_port, local_socket = create_port 0
;;

(* And compute global site identifier *)
let local_id = localaddr, local_port, creation_time
;;

let same_space (a1, p1, t1) (a2, p2, t2) =
  a1 = a2 && p1 = p2 && t1 = t2
;;

type in_handler =
    { in_channel : in_channel;
      in_thread : Thread.t ; }

type in_connection = 
  | NoHandler
  | Handler of in_handler

type out_handler =
  { out_channel : out_channel;
    out_thread : Thread.t ; }

type out_connection =
  | NoConnection
  | Connecting of Thread.t
  | Connected of out_handler

open Join_types

type space = {
    space_id : space_name ;
    space_mutex : Mutex.t ;
    next_uid : unit -> int ;
    stubs : (int, stub) Hashtbl.t ;
    mutable link_in : in_connection ;
    mutable link_out : out_connection ;
  } 

(* Allocate a new unique ident for local site *)
let local_space = {
  space_id = local_id ;
  space_mutex = Mutex.create () ;
  next_uid =
    begin 
      let uid_counter = ref 0 in
      (fun () -> incr uid_counter; !uid_counter)
    end ;
  stubs = Hashtbl.create 13 ;
  link_in = NoHandler ;
  link_out = NoConnection ;
} 
;;

let export_stub space stub =
  let local = stub.local in
  match local with
  | LocalAutomaton a ->
      if a.ident > 0 then
        RemoteAutomaton (space.space_id, a.ident)
      else begin
        Mutex.lock space.space_mutex ;
        let r =
          (* Race condition *)
          if a.ident > 0 then
            RemoteAutomaton (space.space_id, a.ident)
          else begin
            let uid = space.next_uid () in
            eprintf "New uid : %i\n" uid ; flush Pervasives.stderr ;
            Hashtbl.add space.stubs uid stub ;
            a.ident <- uid ;
            RemoteAutomaton (space.space_id, uid)
          end in
        Mutex.unlock space.space_mutex ;
        r
      end
  | RemoteAutomaton _ -> local

external alloc_stub : t_local -> stub = "caml_alloc_stub"

let import_stub space local = match local with
| RemoteAutomaton (msg_space, uid) ->
    if same_space msg_space space.space_id then begin
      Mutex.lock space.space_mutex ;
      eprintf "Receive uid: %i\n" uid ; flush Pervasives.stderr ;
      let stub =
        try Hashtbl.find space.stubs uid
        with Not_found -> assert false in
      Mutex.unlock space.space_mutex ;
      stub.local
    end else
      local
| LocalAutomaton _ -> assert false
  
external do_marshal_message :
  'a -> Marshal.extern_flags list -> string * (Join_types.stub) array
  = "caml_marshal_message" 

let marshal_message v flags =
  let s,t =  do_marshal_message v flags in
  s, Array.map (export_stub local_space) t

external do_unmarshal_message :
  string -> (Join_types.t_local) array -> 'a
  = "caml_unmarshal_message" 

let unmarshal_message (s,locals) =
  do_unmarshal_message s (Array.map (import_stub local_space) locals)
