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
(*DEBUG*)open Join_debug

type 'a chan = 'a async

let get_local_addr = Join_misc.get_local_addr 

exception Exit = Join_misc.JoinExit

def exception Exit

let listen addr =
  try Join_space.listen addr
  with Join_port.Failed (msg,e) ->
(*DEBUG*)debug0 "Join.listen" "failed: %s\n" msg ;
    raise e

let connect fd = Join_space.connect fd

let exit_hook = Join_scheduler.exit_hook


(* Debug from users programs *)
type 'a debug = string -> (('a, unit, string, unit) format4 -> 'a)

let debug = Join_debug.debug


module Site = Site
module Ns = Ns
