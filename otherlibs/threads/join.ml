(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Join_types
open Printf
(*DEBUG*)open Join_debug

type site = space_id

let local_addr = Join_misc.local_addr

let here = Join_space.here

(* RPC by name *)
type service = Join_types.service

let remote_service addr key =
  Join_space.rid_from_addr addr, key

let register_service key (f : 'a -> 'b) =
  Join_space.register_service key f
  
let call_service (rspace_id, key) arg =
  Join_space.call_service rspace_id key arg
  


let listen addr =
  try Join_space.listen addr
  with Join_port.Failed (msg,e) ->
(*DEBUG*)debug0 "Join.listen" "failed: %s\n" msg ;
    raise e

let connect fd = Join_space.connect fd

let exit_hook = Join_scheduler.exit_hook

let at_fail site (chan:unit channel) =
  Join_space.at_fail site (Obj.magic chan : 'a async)
  
let flush_space = Join_space.flush_space

let get_sockaddrs = Join_space.get_sockaddrs

(* Debug from users programs *)
type 'a debug = string -> (('a, unit, string, unit) format4 -> 'a)

let debug = Join_debug.debug0
and debug0 = Join_debug.debug0
and debug1 = Join_debug.debug1
and debug2 = Join_debug.debug2
and debug3 = Join_debug.debug3

