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

type 'a chan = 'a async

let local_addr = Join_misc.local_addr

type site = space_id

let here = Join_space.here

let there addr = Join_space.rid_from_addr addr

let where_from chan = Join_prim.where_from chan


let at_fail site (chan:unit chan) =
  Join_space.at_fail site (Obj.magic chan : 'a async)
  

let listen addr =
  try Join_space.listen addr
  with Join_port.Failed (msg,e) ->
(*DEBUG*)debug0 "Join.listen" "failed: %s\n" msg ;
    raise e

let connect fd = Join_space.connect fd

let exit_hook = Join_scheduler.exit_hook

let flush_space = Join_space.flush_space

let get_sockaddrs = Join_space.get_sockaddrs

(* Debug from users programs *)
type 'a debug = string -> (('a, unit, string, unit) format4 -> 'a)

let debug = Join_debug.debug
