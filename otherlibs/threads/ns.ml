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

open Unix
open Join_types
(*DEBUG*)open Printf
(*DEBUG*)open Join_debug

type request =
  | Put of bool * string (* + parameter *)
  | Get of string
  | Test

(* This  enforces critical initialization in Join,
   when ns is called before any join operation *)

let local_addr = Join.local_addr

exception Failed of string

type server =
  {server : Join_port.server ;
   mutable run : bool ;
   mutex : Mutex.t }


let do_serve t link =
(*DEBUG*)debug1 "NAME SERVER" "start" ;
  try
    let req = Join_message.input_value link in
    begin match req with
    | Test -> ()
    | Put (once, key) ->
	let v = Join_message.input_parameter link in
        let r =
          if once && Hashtbl.mem t key then
            false
          else begin
            Hashtbl.replace t key v ;
            true
          end in
        Join_message.output_value link r  ; Join_link.flush link
    | Get key ->
	begin try
          let r = Hashtbl.find t key in
	  Join_message.output_value link true ;
          Join_message.output_parameter link r ;
	  ()
	with Not_found ->
	  Join_message.output_value link false
	end ;
	Join_link.flush link
    end ;
    Join_link.close link
  with Join_link.Failed ->
(*DEBUG*)debug0 "NS" "server went wrong" ;
    try Join_link.close link with Join_link.Failed -> assert false

  
let start_server port =
  let t = (Hashtbl.create 13 : (string, parameter) Hashtbl.t) in
  try
    let port = Unix.ADDR_INET (Unix.inet_addr_any, port) in
    let  _,server = Join_port.establish_server port (do_serve t) in
    { server=server ; run=true ; mutex=Mutex.create () ; }
  with Join_port.Failed msg -> raise (Failed msg)

let stop_server ({server=s ; run=run ; mutex=mutex} as x) =
  Mutex.lock mutex ;
  if run then begin
    begin try
      Join_port.kill_server s
    with Join_port.Failed _ ->
      assert false (* assumes Failed means double close *)
    end ;
    x.run <- false
  end ;
  Mutex.unlock mutex
      
    
      

type link = Unix.sockaddr

let register_client addr port =
  let ns = Unix.ADDR_INET (addr, port) in
  let rec do_rec d =
    if d > 2.0 then raise (Failed "register_client: timeout")
    else try
      let link = Join_port.connect ns in
      Join_message.output_value link Test ; Join_link.flush link ;
      Join_link.close link
    with
    | Join_port.Failed _ |Join_link.Failed ->
        Thread.delay d ;
        do_rec (2.0 *. d) in
  do_rec 0.2 ;
  ns
      
  

let lookup port key =
  try
    let link = Join_port.connect port in
(*DEBUG*)debug3 "NS" (sprintf "client get %s" key) ;
    begin try
      Join_message.output_value link (Get key) ; Join_link.flush link ;
      let found = Join_message.input_value link in
(*DEBUG*)debug3 "NS" (sprintf "client get -> %b" found) ;
      if found then begin
	let r = Join_message.input_parameter link in
	Join_link.close link ;
	Join_space.localize r
      end else begin
	Join_link.close link ;
	raise Not_found
      end
    with
    | Not_found -> raise Not_found
    | Join_link.Failed  ->
	Join_link.close link ;
	raise (Failed "*zorglub*")
    end
  with
  | Not_found -> raise Not_found
  | Join_port.Failed msg -> raise (Failed msg)
  | e ->
(*DEBUG*)debug0 "NS LOOKUP" (sprintf "got %s" (Join_misc.exn_to_string e)) ;
      raise e

let do_sync_register once port key v =
  try
    let link = Join_port.connect port in
    try
      Join_message.output_value link (Put (once, key)) ;
      Join_message.output_parameter link (Join_space.globalize v []) ;
      Join_link.flush link ;
      let r = (Join_message.input_value link : bool ) in
      Join_link.close link ;
      r
    with
    | Join_link.Failed ->
	prerr_endline "ns_register went wrong: " ;
        Join_link.close link ;
	false
  with
  | Join_port.Failed msg -> raise (Failed msg)
  | e ->
(*DEBUG*)debug0 "NS REGISTER" (sprintf "got %s" (Join_misc.exn_to_string e)) ;
    raise e

let register ns key v =
  ignore (do_sync_register false ns key v)

and register_once ns key v =
  do_sync_register true ns key v
