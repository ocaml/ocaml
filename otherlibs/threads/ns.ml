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

let local_addr = Join_misc.local_addr

let start_server port =
  let t = Hashtbl.create 13 in
  let _,sacc = Join_misc.create_port port in
  Join.create_process
    (fun () ->
      while true do
        let (s,_) = Join_misc.force_accept sacc in
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
          Join_misc.prerr_exn e
        end ;
        close s
      done)


type link = Unix.inet_addr * int

let register_client addr port = addr, port



let lookup (addr, port) key =
  let s = Join_misc.force_connect addr port in
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
      Join_misc.prerr_exn e ;
      close s ;
      raise Not_found
        

let register  (addr, port) key v =
  let s = Join_misc.connect_to_server addr port in
  try
    let outc = out_channel_of_descr s in
    let msg = Join_space.marshal_message v [] in
    output_value outc (Put (key,msg)) ; flush outc ;
    close s
  with
  | e ->
      prerr_string "ns_register went wrong: " ;
      Join_misc.prerr_exn e ;
      raise e


        
let do_sync_register once (addr, port) key v =
  let s = Join_misc.force_connect addr port in
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
      Join_misc.prerr_exn e ;
      raise e

let sync_register ns key v =
  ignore (do_sync_register false ns key v)

and sync_register_once ns key v =
  do_sync_register true ns key v
