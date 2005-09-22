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

type request =
  | Put of string * (string * Join_types.t_global array)
  | SyncPut of bool * string * (string * Join_types.t_global array)
  | Get of string

let prerr_exc = function
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

let local_addr = Join_space.local_addr

let start_server port =
  let t = Hashtbl.create 13 in
  let _,sacc = Join_space.create_port port in
  Join.create_process
    (fun () ->
      while true do
        let (s,_) = Join_space.accept sacc in
        begin try
          let inc = in_channel_of_descr s
          and outc = out_channel_of_descr s in
          let req = input_value inc in
          begin match req with
          | Put (key, v) -> Hashtbl.replace t key v
          | SyncPut (once, key, v) ->
              let r =
                if once && Hashtbl.mem t key then
                    false
                  else begin
                    Hashtbl.replace t key v ;
                    true
                  end in
              output_value outc r  ; Pervasives.flush outc
          | Get key ->
              let r =
                try Some (Hashtbl.find t key)
                with Not_found -> None in
              output_value outc r ; flush outc
          end
        with e ->
          close s ;
          prerr_string "ns_server went wrong: " ;
          prerr_exc e
        end ;
        close s
      done)


type link = Unix.inet_addr * int

let register_client addr port = addr, port

let connect_to_server verbose addr port =
  let sock = socket PF_INET SOCK_STREAM 0 in
  try
    connect sock (ADDR_INET(addr, port)) ;
    sock
  with
  | e ->
      if verbose then begin
        prerr_string "connect to ns went wrong: " ;
        prerr_exc e
      end ;
      close sock ;
      raise e


let lookup (addr, port) key =
  let s = connect_to_server true addr port in
  try
    let inc = in_channel_of_descr s
    and outc = out_channel_of_descr s in
    output_value outc (Get key) ; flush outc ;
    let r = input_value inc in
    close s ;
    match r with
    | None -> raise Not_found
    | Some r -> Join_space.unmarshal_message r
  with
  | Not_found -> raise Not_found
  | e ->
      prerr_string "ns_lookup went wrong: " ;
      prerr_exc e ;
      close s ;
      raise Not_found
        

let register  (addr, port) key v =
  let s = connect_to_server true addr port in
  try
    let outc = out_channel_of_descr s in
    let msg = Join_space.marshal_message v [] in
    output_value outc (Put (key,msg)) ; flush outc ;
    close s
  with
  | e ->
      prerr_string "ns_register went wrong: " ;
      prerr_exc e ;
      raise e

type 'a cell =
    {
      condition : Condition.t ;
      mutable cell : 'a option ;
    } 

let create_cell = { condition = Condition.create () ; cell = None ; }

let thread_call f =
  let mutex = Mutex.create () in
  let c = create_cell in
  ignore
    (Thread.create
       (fun () ->
         let r = f () in
         Mutex.lock mutex ;
         c.cell <- Some r ;
         Condition.signal c.condition ;
         Mutex.unlock mutex)
    ()) ;
  Mutex.lock mutex ;
  Condition.wait c.condition mutex ;
  Mutex.unlock mutex ;
  match c.cell with
  | Some v -> v
  | None -> assert false

  
      
let force_connect addr port =
  try
    connect_to_server true addr port
  with _ ->
    let rec do_rec d =
      Thread.delay d ;
      try
        connect_to_server false addr port
      with
      | _ ->
          do_rec (if d > 5.0 then d else 2.0 *. d)
    in do_rec 0.1
        
let do_sync_register once (addr, port) key v =
  let s = force_connect addr port in
  try
    let outc = out_channel_of_descr s
    and inc = in_channel_of_descr s in
    let msg = Join_space.marshal_message v [] in
    output_value outc (SyncPut (once, key, msg)) ; flush outc ;
    let r = input_value inc in
    close s ;
    r
  with
  | e ->
      prerr_string "ns_register went wrong: " ;
      prerr_exc e ;
      raise e

let sync_register ns key v =
  ignore (do_sync_register false ns key v)

and sync_register_once ns key v =
  do_sync_register true ns key v
