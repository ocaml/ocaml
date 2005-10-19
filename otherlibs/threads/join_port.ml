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

exception Failed

type server =
  {
    loc_port : int ;
    loc_sock : Unix.file_descr ;
  }


(* Notice when port is zero, then a fresh port is allocated by bind  *)
let create_port port =
  let sock =
    try
      let s = socket PF_INET SOCK_STREAM 0 in
      try
        setsockopt s SO_REUSEADDR true;
        bind s (ADDR_INET (inet_addr_any, port));
        listen s 5;
        s
      with e -> close s ; raise e
    with e ->
(*DEBUG*)debug0 "CREATE PORT" (sprintf "failed on %s" (exn_to_string e)) ;
        raise Failed in
  let saddr = getsockname sock in
  match saddr with
  | ADDR_INET (_, port) -> {loc_port=port ;  loc_sock=sock ; }
  | _ -> assert false

let rec force_accept s =
  try
(*DEBUG*)debug1 "UNIX" "accept" ;
    let (_,addr) as r = Unix.accept s in
(*DEBUG*)debug1 "UNIX" ("accepted: "^string_of_sockaddr addr) ;
    r
  with
  | Unix_error((EAGAIN|EINTR),_,_) -> 
(*DEBUG*)debug1 "accept" "try again" ;
      force_accept s


and listener port when_accepted () =
  try while true do
(*DEBUG*)debug1 "LISTENER"
(*DEBUG*)  (sprintf "now accept on port %i" port.loc_port) ;
    let s,_ = force_accept port.loc_sock in
(*DEBUG*)debug1 "LISTENER" "someone coming" ;
    let link = Join_link.create s in
    when_accepted link
  done with  e ->
(*DEBUG*)debug0 "LISTENER"
(*DEBUG*)  (sprintf "died of %s" (Join_misc.exn_to_string e)) ;
    ()

let establish_server port when_accepted =
  let local_port = create_port port in
  Join_scheduler.create_process (listener local_port when_accepted) ;
  Unix.ADDR_INET (Join_misc.local_addr, local_port.loc_port),
  local_port

let kill_server { loc_sock = sock ; } =
(*DEBUG*)debug1 "KILL_SERVER" "" ;
  try
    Unix.shutdown sock Unix.SHUTDOWN_ALL ;
    Unix.close sock ;
  with e ->
(*DEBUG*)debug0 "KILL SERVER"
(*DEBUG*)  (sprintf "got %s" (Join_misc.exn_to_string e)) ;
    raise Failed
    



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
(*DEBUG*)debug1 "CONNECTED" (string_of_sockaddr (getpeername sock)) ;
        sock
      with z -> close sock ; raise z
    with
    | e ->
(*DEBUG*)debug1 "CONNECT" (sprintf "failed on %s" (exn_to_string e)) ;
        raise Failed in
  Join_link.create sock

