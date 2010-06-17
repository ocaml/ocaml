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

(*DEBUG*)open Join_debug
(*DEBUG*)open Printf
open Join_misc
open Unix

exception Failed of string * exn

type server =
  {
    loc_port : Unix.sockaddr ;
    loc_sock : Unix.file_descr ;
  }


(* Notice when port is zero, then a fresh port is allocated by bind  *)
let create_port porto =
  let sock =
    try
      let port = match porto with
      | None -> ADDR_INET (inet_addr_any, 0)
      | Some p -> p in
      let s =
	socket
	  (Unix.domain_of_sockaddr port)
	  SOCK_STREAM 0 in
      try
	set_close_on_exec s ;
        setsockopt s SO_REUSEADDR true;
	bind s port ;
        listen s 5;
        s
      with e -> close s ; raise e
    with e ->
(*DEBUG*)debug1 "CREATE PORT" "%s" (exn_to_string e) ;
        raise (Failed (exn_to_string e, e)) in
  let sockaddr = 
    let sockaddr = getsockname sock in
    match porto with
    | Some _ -> sockaddr
    | None -> match sockaddr with
      | ADDR_INET (_, port) ->  ADDR_INET (Join_misc.get_local_addr (), port)
      | ADDR_UNIX _ -> assert false in
  {loc_port=sockaddr ;  loc_sock=sock ; }


let rec force_accept s =
  try
(*DEBUG*)debug1 "UNIX" "accept" ;
    let (_,addr) as r = Unix.accept s in
(*DEBUG*)debug1 "UNIX" "accepted: %s" (string_of_sockaddr addr) ;
    r
  with
  | Unix_error((EAGAIN|EINTR),_,_) -> 
(*DEBUG*)debug1 "accept" "%s" "try again" ;
      force_accept s


and listener port when_accepted () =
  try while true do
(*DEBUG*)debug1 "LISTENER"
(*DEBUG*)  "now accept on %s" (string_of_sockaddr port.loc_port) ;
    let s,_ = force_accept port.loc_sock in
(*DEBUG*)debug1 "LISTENER" "%s" "someone coming" ;
    let link = Join_link.create s in
    try when_accepted link
    with e ->
(*DEBUG*)debug1 "LISTENER"
(*DEBUG*)  "acceptor died of %s" (Join_misc.exn_to_string e) ;
      ()
  done with  e ->
(*DEBUG*)debug0 "LISTENER"
(*DEBUG*)  "died of %s" (Join_misc.exn_to_string e) ;
    ()

let establish_server port when_accepted =
  let local_port = create_port port in
  Join_scheduler.create_process (listener local_port when_accepted) ;
  local_port.loc_port, local_port

let kill_server { loc_sock = sock ; } =
(*DEBUG*)debug1 "KILL_SERVER" "" ;
  try
    Unix.shutdown sock Unix.SHUTDOWN_ALL ;
    Unix.close sock ;
  with e ->
(*DEBUG*)debug0 "KILL SERVER"
(*DEBUG*)  "got %s" (Join_misc.exn_to_string e) ;
    raise e
    



let connect sockaddr =
  let sock =
    try
      let sock =
        socket
          (match sockaddr with
          | ADDR_INET (_,_) -> PF_INET
          | ADDR_UNIX _ -> PF_UNIX)
          SOCK_STREAM 0 in
      try
        Unix.connect sock sockaddr ;
(*DEBUG*)debug1 "CONNECTED" "%s" (string_of_sockaddr (getpeername sock)) ;
        sock
      with z -> close sock ; raise z
    with
    | e ->
(*DEBUG*)debug1 "CONNECT" "%s" (exn_to_string e) ;
        raise (Failed (exn_to_string e,e)) in
  Join_link.create sock (* Can fail only for OutOfMemory *)
