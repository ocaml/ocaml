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

open Join_types
open Printf
(*DEBUG*)open Join_debug

(*****************************)
(* Magic number for messages *)
(*****************************)

(* Changes with format of messages *)
let magic = "JoCaml310"

(*********************)
(* Global exceptions *)
(*********************)

(*
   Global exceptions look like they are common to all runtimes,
   This is achieved by a translation when processing ReplyExn messages
*)

let exn_table = Join_hash.create ()

let get_exn_cstr exn = Obj.field exn 0
and get_cstr_name cstr = (Obj.obj (Obj.field cstr 0) : string )

let exn_global (file,line,_) exn_cstr =
  match Join_hash.add_once exn_table exn_cstr ((file, line), exn_cstr) with
  | None -> ()
  | Some ((old_file, old_line), _) ->
      failwith
	(sprintf
	   "File \"%s\", line %i: global exception clash %s\n\
	   File  \"%s\", line %i: conflicting declaration\n"
	   file line (get_cstr_name exn_cstr)
	   old_file old_line)

let localize_exn (exn : exn) =
   let cstr = get_exn_cstr (Obj.repr exn) in
   try
     let _,new_cstr = Join_hash.find exn_table cstr in
     let o = Obj.repr exn in
     let r =
       match Obj.size o with
       | 1|2 ->
	   let r = Obj.dup o in
	   Obj.set_field r 0 new_cstr ;
	   r
       | _ -> assert false in
 (*DEBUG*)debug2 "LOCALIZE"
 (*DEBUG*)  "successful localization of %s" (get_cstr_name cstr) ;
     Obj.obj r
   with
   | Not_found ->
 (*DEBUG*)debug2 "LOCALIZE"
 (*DEBUG*)  "failed localization of %s" (get_cstr_name cstr) ;    
       exn

(* For efficiency, messages are transmitted as several
   marshalled values *)

(* Read the a sequence of chars that encodes a marshalled value
   ie, returned by extern from extern.c

   This code is inspired from input_value in pervasives.ml,
   it assumes non-blocking really_input *)

open Join_link 
   
let header_size = Marshal.header_size  (* 20 ! *)

let input_marshalled_string  ic =
  let header = String.create header_size in
  really_input ic header 0 header_size ;
  let data_size = Marshal.data_size header 0 in
  let total_size = data_size + header_size in
  let buff = String.create total_size in
  String.blit header 0 buff 0 header_size ;
  really_input ic buff header_size data_size ;
  buff

and output_marshalled_string oc buff = output_string oc buff

let input_value link =
  let buff = input_marshalled_string link in
  Marshal.from_string buff 0 

and output_value link (v:'a) =
  let buff = Marshal.to_string v [] in
  output_string link buff
  
(* read the argument part of a message,
   ie a pair, string * t_global array *)

let input_parameter ic =
  let s = input_marshalled_string ic in
  let t = (input_value ic : t_global array) in
  (s, t)

and output_parameter oc ((s, t):parameter) =
  output_marshalled_string oc s ;
  output_value oc t

type msg_prefix =
  | PasyncSend of chan_id
  | PaloneSend of alone_id
  | PsyncSend of chan_id * kont_id
  | PaloneSyncSend of alone_id * kont_id
  | PreplySend of kont_id
  | PreplyExn of (kont_id * exn)
  | Pservice of (string * kont_id)

let input_msg ic = match input_value ic with
| PasyncSend chan ->
    AsyncSend (chan, input_parameter ic)
| PsyncSend (chan,kont) ->
    SyncSend (chan, kont, input_parameter ic)
| PaloneSyncSend (uid, kont) ->
    AloneSyncSend (uid, kont, input_parameter ic)
| PreplySend kont ->
    ReplySend (kont, input_parameter ic)
| PreplyExn (kont, e) ->
    ReplyExn (kont, e)
| PaloneSend alone ->
    AloneSend (alone, input_parameter ic)
| Pservice (key, kont) ->
    Service (key, kont, input_parameter ic)

and output_msg oc = function
  | AsyncSend (chan, arg) ->
      output_value oc (PasyncSend chan) ;
      output_parameter oc arg
  | AloneSend (alone, arg) ->
      output_value oc (PaloneSend alone) ;
      output_parameter oc arg
  | SyncSend (chan, kont, arg) ->
      output_value oc (PsyncSend (chan, kont)) ;
      output_parameter oc arg
  | AloneSyncSend (uid, kont, arg) ->
      output_value oc (PaloneSyncSend (uid, kont)) ;
      output_parameter oc arg
  | ReplySend (kont, arg) ->
      output_value oc (PreplySend kont) ;
      output_parameter oc arg
  | ReplyExn (kont, e) ->
      output_value oc (PreplyExn (kont, e))
  | Service (key, kont, arg) ->
      output_value oc (Pservice (key, kont)) ;
      output_parameter oc arg


let string_of_msg = function
  | AsyncSend (_, _) -> "AsyncSend (chan, arg)"
  | AloneSend (_, _) -> "AloneSend (alone, arg)"
  | SyncSend (_, _, _) -> "SyncSend (chan, kont, arg)"
  | AloneSyncSend (_, _, _) -> "AloneSyncSend (uid, kont, arg)"
  | ReplySend (_, _) -> "ReplySend (kont, arg)"
  | ReplyExn (_, _) -> "ReplyExn (kont, e)"
  | Service (key, _, _) -> Printf.sprintf "Service (\"%s\", kont, arg)" key

