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

let get_local_addr () = Join_misc.get_local_addr ()

type site = unit chan

let here = Join_prim.create_alone (fun _ -> ()) "here"

let site_service () = here

let site_key = "name_youhou"

let () = Join_prim.register_service site_key site_service

let there addr = 
  let space_id = Join_space.rid_from_addr addr in
  let site = Join_prim.call_service (space_id, site_key) () in
  (site : site)

let where_from chan =
  let stub = match chan with Async(stub,_)|Alone(stub,_) -> stub in
  match stub.stub_tag with
  | Local -> here
  | Remote -> 
      let space_id = (Obj.magic stub.stub_val : space_id) in
      let site = Join_prim.call_service (space_id, site_key) () in
      (site : site)

let same_site s1 s2 = 
  (Join_prim.space_id_of_chan s1) = (Join_prim.space_id_of_chan s2)

exception Exit = Join_misc.JoinExit

let () = Join_prim.exn_global ("join.ml", 50, 0) (Obj.repr Exit)

let at_fail site chan =
  Join_space.at_fail (Join_prim.space_id_of_chan site) chan  

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
