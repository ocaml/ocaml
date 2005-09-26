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

type controler =
  {
    cond_w : Condition.t ;
    cond_r : Condition.t ;
    mutex : Mutex.t ;
    mutable n_read : int ;
    mutable n_wait : int ;
  } 


let controler_create () =
  {
    cond_w = Condition.create () ;
    cond_r = Condition.create () ;
    mutex = Mutex.create () ;
    n_read = 0 ;
    n_wait = 0 ;
  } 

let do_lock c f =
  Mutex.lock c.mutex ;
  f c ;
  Mutex.unlock c.mutex

let read_access c =
  do_lock c
    (fun c ->
      c.n_wait <- c.n_wait + 1 ;
      while c.n_read < 0 do
        Condition.wait c.cond_r c.mutex
      done ;
      c.n_wait <- c.n_wait - 1 ;
      c.n_read <- c.n_read + 1)

let read_return c =
  do_lock c
    (fun c ->
      c.n_read <- c.n_read - 1;
      if c.n_read = 0 then
        Condition.signal c.cond_w)

let protect_read c reader key =
  read_access c ;
  try
    let r = reader key in
    read_return c ;
    r
  with
  | e ->
      read_return c ;
      raise e

let write_access c =
  do_lock c
    (fun c ->
      while c.n_read <> 0 do
        Condition.wait c.cond_w c.mutex
      done ;
      c.n_read <- -1)

let write_return c =
  do_lock c
    (fun c ->
      c.n_read <- 0 ;
      if c.n_wait > 0 then
        Condition.broadcast c.cond_r
      else
        Condition.signal c.cond_w)
      

let protect_write c writer key =
  write_access c ;
  try
    let r = writer key in
    write_return c ;
    r
  with
  | e ->
      write_return c ;
      raise e

(* A few wrapping of socket primitives *)

open Unix

let prerr_exn = function
  | Unix_error(err, fun_name, arg) ->
    prerr_string Sys.argv.(0);
    prerr_string ": \"";
    prerr_string fun_name;
    prerr_string "\" failed";
    if String.length arg > 0 then begin
      prerr_string " on \"";
      prerr_string arg;
      prerr_string "\""
    end;
    prerr_string ": ";
    prerr_endline (error_message err)
  | e -> prerr_endline (Printexc.to_string e)

let exn_to_string = function
  | Unix_error(err, fun_name, arg) ->
    let buff = Buffer.create 80 in
    Buffer.add_string buff Sys.argv.(0);
    Buffer.add_string buff ": \"";
    Buffer.add_string buff fun_name;
    Buffer.add_string buff "\" failed";
    if String.length arg > 0 then begin
      Buffer.add_string buff " on \"";
      Buffer.add_string buff arg;
      Buffer.add_string buff "\""
    end;
    Buffer.add_string buff ": ";
    Buffer.add_string buff (error_message err) ;
    Buffer.contents buff
  | e -> Printexc.to_string e

let first_addr entry = entry.h_addr_list.(0)

let name = gethostname ()

let local_name,local_addr =
(* try to bind a socket to a INET port, if not possible,
use loopback (127.0.0.1) instead... *)
  let s = socket PF_INET SOCK_STREAM 0 in
  setsockopt s SO_REUSEADDR true;  
  let localaddr = 
    try
      first_addr (gethostbyname name)
    with
    | _ -> inet_addr_of_string "127.0.0.1"
    in
  let localname = 
    try
      bind s (ADDR_INET (localaddr, 0));
      name
    with
      _ -> 
        prerr_endline "WARNING: defaulting to loopback address, verify your network connection" ;
        bind s (ADDR_INET (localaddr, 0));
        "localhost"
  in
  close s;
  localname, localaddr



let create_port port =
  handle_unix_error
    (fun () ->
      let s = socket PF_INET SOCK_STREAM 0 in
      let _ =
        try
          setsockopt s SO_REUSEADDR true;
          bind s (ADDR_INET (local_addr, port));
          listen s 5;
          ()
        with z -> close s ; raise z
      in
      let saddr = getsockname s in
      match saddr with
        ADDR_INET (_, port) -> port, s
      | _ -> assert false) ()
;;


let string_of_sockaddr = function
  | ADDR_UNIX s -> s
  | ADDR_INET (a,p) ->
      string_of_inet_addr a^":"^string_of_int p

let rec force_accept s =
  begin try
(*DEBUG*)debug1 "UNIX" "accept" ;
    let (_,addr) as r = Unix.accept s in
(*DEBUG*)debug1 "UNIX" ("accepted: "^string_of_sockaddr addr) ;
    r
  with
  | Unix_error((EAGAIN|EINTR),_,_) -> 
(*DEBUG*)debug1 "accept" "try again" ;
      force_accept s
  end


let do_connect_to_server verbose addr port =
  let sock = socket PF_INET SOCK_STREAM 0 in
  try
    connect sock (ADDR_INET(addr, port)) ;
(*DEBUG*)debug1 "CONNECTED" (string_of_sockaddr (getpeername sock)) ;
    sock
  with
  | e ->
      if verbose then begin
        prerr_string "connect went wrong: " ;
        prerr_exn e
      end ;
      close sock ;
      raise e

let connect_to_server addr port =
   do_connect_to_server true addr port

let force_connect addr port =
  try
    do_connect_to_server true addr port
  with _ ->
    let rec do_rec d =
      Thread.delay d ;
      try
        do_connect_to_server false addr port
      with
      | _ ->
          do_rec (if d > 5.0 then d else 2.0 *. d)
    in do_rec 0.1

exception JoinExit

