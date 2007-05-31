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


type t = unit async

def _here() = assert false ; 0

let here = (Obj.magic _here : unit async)

let site_service () = here
let site_key = "name_youhou"
let () = Join_prim.register_service site_key site_service

let there addr = 
  let space_id = Join_space.rid_from_addr addr in
  let site = Join_prim.call_service (space_id, site_key) () in
  (site : t)

let where_from chan =
  let stub = match chan with Async(stub,_)|Alone(stub,_) -> stub in
  match stub.stub_tag with
  | Local -> here
  | Remote -> 
      let space_id = (Obj.magic stub.stub_val : space_id) in
      let site = Join_prim.call_service (space_id, site_key) () in
      (site : t)

let equal s1 s2 = 
  (Join_prim.space_id_of_chan s1) = (Join_prim.space_id_of_chan s2)

and compare s1 s2 =
  Pervasives.compare
    (Join_prim.space_id_of_chan s1)
    (Join_prim.space_id_of_chan s2)
  
let at_fail site chan =
  Join_space.at_fail (Join_prim.space_id_of_chan site) chan  
