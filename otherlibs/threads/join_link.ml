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

exception Failed

type t =
   {
    outc : out_channel ;
    inc : in_channel ;
    fd : Unix.file_descr ;
  } 


let create fd =
  try
    Unix.set_close_on_exec fd ;
  { outc = Unix.out_channel_of_descr fd ;
    inc  = Unix.in_channel_of_descr fd ;
    fd = fd ; }
  with e ->
(*DEBUG*)debug1 "CREATE" "failed on %s" (Join_misc.exn_to_string e) ;
    match e with
    | Out_of_memory -> raise e
    | _  -> assert false


let output_string { outc = outc } v =
  try
    output_string outc v
  with
  | Sys_error _ as _e ->
(*DEBUG*)debug1 "OUTPUT_STRING"  "failed on %s" (Join_misc.exn_to_string _e) ;
      raise Failed
  | _e ->
(*DEBUG*)debug0 "FATAL OUTPUT_STRING"
(*DEBUG*)  "failed on %s" (Join_misc.exn_to_string _e) ;   
      assert false
 

let flush { outc = outc } =
  try
     flush outc
  with
  | Sys_error _ as _e ->
(*DEBUG*)debug1 "FLUSH"
(*DEBUG*)  "failed on %s" (Join_misc.exn_to_string _e) ;
      raise Failed
  | e ->
(*DEBUG*)debug0 "FATAL FLUSH"
(*DEBUG*)  "failed on %s" (Join_misc.exn_to_string e) ;   
      assert false


let really_input { inc = inc } buff ofs len =
  try
    really_input inc buff ofs len
  with
  | End_of_file|Sys_error _ as _e ->
(*DEBUG*)debug1 "REALLY INPUT"
(*DEBUG*)  "failed on %s" (Join_misc.exn_to_string _e) ;
      raise Failed
  | e ->
(*DEBUG*)debug0 "FATAL REALLY INPUT"
(*DEBUG*)  "failed on %s" (Join_misc.exn_to_string e) ;   
      assert false

let close {fd=fd} =
  try
    Unix.close fd
  with
  | Unix.Unix_error (Unix.EBADF,_,_) as _e ->
(*DEBUG*)debug1 "LINK CLOSE"
(*DEBUG*)  "failed on %s" (Join_misc.exn_to_string _e) ;
      raise Failed
  | e ->
(*DEBUG*)debug0 "IO ERROR IN LINK CLOSE"
(*DEBUG*)  "failed on %s" (Join_misc.exn_to_string e) ;   
      exit 0
