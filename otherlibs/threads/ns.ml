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


let local_addr = Join_misc.local_addr

open Join_link

type server =
  {fd : Unix.file_descr ;
   mutable run : bool ;
   mutex : Mutex.t }

let start_server port =
  let t = (Hashtbl.create 13 : (string, parameter) Hashtbl.t) in
  let _,sacc = Join_misc.create_port port in
  Join.create_process
    (fun () ->
(*DEBUG*)debug1 "NAME SERVER" "start" ;
      try
        while true do
          let (s,_) = Join_misc.force_accept sacc in
	  let link = Join_link.create s in
          try
            let req = Join_message.input_value link in
            begin match req with
            | Put (once, key) ->
		let v = Join_message.input_parameter link in
                let r =
                  if once && Hashtbl.mem t key then
                    false
                  else begin
                    Hashtbl.replace t key v ;
                    true
                  end in
                Join_message.output_value link r  ; flush link
            | Get key ->
		begin try
                  let r = Hashtbl.find t key in
		  Join_message.output_value link true ;
                  Join_message.output_parameter link r ;
		  ()
		with Not_found ->
		  Join_message.output_value link false
		end ;
		flush link
            end (* match *)
	  with Failed    ->
	    prerr_endline "ns_lookup went wrong" ;
	    close link ;
	    raise Exit
        done
      with
      |	Exit -> ()
      |	e ->
(*DEBUG*)debug0 "NAME_SERVER"
(*DEBUG*)  (sprintf "died of %s" (Join_misc.exn_to_string e)) ;
        ()) ;
  { fd=sacc ; run=true ; mutex=Mutex.create () ; }

let stop_server ({fd=sacc ; run=run ; mutex=mutex } as s) =
  Mutex.lock mutex ;
  if run then begin
(*DEBUG*)debug1 "STOP_SERVER" "" ;
    begin try
      Unix.shutdown sacc Unix.SHUTDOWN_ALL ;
      Unix.close sacc ;
    with e ->
(*DEBUG*)debug0 "STOP SERVER"
(*DEBUG*)  (sprintf "got %s" (Join_misc.exn_to_string e)) ;
    end ;
    s.run <- false
  end ;
  Mutex.unlock mutex
      
    
      

type link = Unix.inet_addr * int

let register_client addr port =
(*DEBUG*)debug3 "NS" (sprintf "port=%i" port) ;
  addr, port

let lookup (addr, port) key =
  try
    let s = Join_misc.force_connect addr port in
(*DEBUG*)debug3 "NS" (sprintf "client get %s" key) ;
    let link = Join_link.create s in
    begin try
      Join_message.output_value link (Get key) ; flush link ;
      let found = Join_message.input_value link in
(*DEBUG*)debug3 "NS" (sprintf "client get -> %b" found) ;
      if found then begin
	let r = Join_message.input_parameter link in
	close link ;
	Join_space.localize r
      end else begin
	close link ;
	raise Not_found
      end
    with
    | Not_found -> raise Not_found
    | Failed    ->
	prerr_endline "ns_lookup went wrong" ;
	close link ;
	raise Not_found
    end
  with e ->
(*DEBUG*)debug0 "NS LOOKUP" (sprintf "got %s" (Join_misc.exn_to_string e)) ;
      raise e

let do_sync_register once (addr, port) key v =
  try
    let s = Join_misc.force_connect addr port in
    let link = Join_link.create s in
    try
      Join_message.output_value link (Put (once, key)) ;
      Join_message.output_parameter link (Join_space.globalize v []) ;
      flush link ;
      let r = (Join_message.input_value link : bool ) in
      close link ;
      r
    with
    | Failed ->
	prerr_endline "ns_register went wrong: " ;
	Unix.close s ;
	false
  with e ->
(*DEBUG*)debug0 "NS REGISTER" (sprintf "got %s" (Join_misc.exn_to_string e)) ;
    raise e

let register ns key v =
  ignore (do_sync_register false ns key v)

and register_once ns key v =
  do_sync_register true ns key v
