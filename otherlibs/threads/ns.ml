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
  | Put of string  (* + parameter *)
  | SyncPut of bool * string (* + parameter *)
  | Get of string


let local_addr = Join_misc.local_addr

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
          begin try
            let inc = in_channel_of_descr s
            and outc = out_channel_of_descr s in
            let req = input_value inc in
            begin match req with
            | Put (key) ->
		let v = Join_message.input_parameter inc in
		Hashtbl.replace t key v
            | SyncPut (once, key) ->
		let v = Join_message.input_parameter inc in
                let r =
                  if once && Hashtbl.mem t key then
                    false
                  else begin
                    Hashtbl.replace t key v ;
                    true
                  end in
                output_value outc r  ; flush outc
            | Get key ->
(*DEBUG*)debug3 "NS" (sprintf "server get %s" key) ;		
		begin try
                  let r = Hashtbl.find t key in
		  output_value outc true ;
                  Join_message.output_parameter outc r ;
(*DEBUG*)debug3 "NS" "server get -> found" ;
		  ()
		with Not_found ->
(*DEBUG*)debug3 "NS" "server get -> not found" ;
		  output_value outc false
		end ;
		flush outc
            end (* match *)
          with e ->
            prerr_string "ns_server went wrong: " ;
            Join_misc.prerr_exn e
          end ; (* try *)
          close s
        done
      with
      | Unix.Unix_error((Unix.EBADF|Unix.EINVAL), "accept", _) ->
(*DEBUG*)debug1 "NAME_SERVER" "saw shutdowned or closed socket" ;
          ()
      | e ->
(*DEBUG*)debug1 "NAME_SERVER"
(*DEBUG*)  (sprintf "died of %s" (Join_misc.exn_to_string e)) ;
          ()) ;
  { fd=sacc ; run=true ; mutex=Mutex.create () ; }

let stop_server ({fd=sacc ; run=run ; mutex=mutex } as s) =
  Mutex.lock mutex ;
  if run then begin
(*DEBUG*)debug1 "STOP_SERVER" "" ;
    Unix.shutdown sacc Unix.SHUTDOWN_ALL ;
    Unix.close sacc ;
    s.run <- false
  end ;
  Mutex.unlock mutex
      
    
      

type link = Unix.inet_addr * int

let register_client addr port =
(*DEBUG*)debug3 "NS" (sprintf "port=%i" port) ;
  addr, port

let lookup (addr, port) key =
  let s = Join_misc.force_connect addr port in
  try
(*DEBUG*)debug3 "NS" (sprintf "client get %s" key) ;
    let inc = in_channel_of_descr s
    and outc = out_channel_of_descr s in
    output_value outc (Get key) ; flush outc ;
    let found = input_value inc in
(*DEBUG*)debug3 "NS" (sprintf "client get -> %b" found) ;
    if found then begin
      let r = Join_message.input_parameter inc in
      close s ;
      Join_space.localize r
    end else begin
      close s ;
      raise Not_found
    end
  with
  | Not_found -> raise Not_found
  | e ->
      prerr_string "ns_lookup went wrong: " ;
      Join_misc.prerr_exn e ;
      close s ;
      raise Not_found
        

let do_sync_register once (addr, port) key v =
  let s = Join_misc.force_connect addr port in
  try
    let outc = out_channel_of_descr s
    and inc = in_channel_of_descr s in
    output_value outc (SyncPut (once, key)) ;
    Join_message.output_parameter outc (Join_space.globalize v []) ;
    flush outc ;
    let r = input_value inc in
    close s ;
    r
  with
  | e ->
      prerr_string "ns_register went wrong: " ;
      Join_misc.prerr_exn e ;
      close s ;
      raise e

let register ns key v =
  ignore (do_sync_register false ns key v)

and register_once ns key v =
  do_sync_register true ns key v
