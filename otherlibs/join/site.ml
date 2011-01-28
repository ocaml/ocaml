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


(* Trick: a site simply is an async channel.
   Sites elements are to be found in stub *)
type t = unit async

def _here() = assert false ; 0

let here = (Obj.magic _here : unit async)


let get_local_addr = Join_misc.get_local_addr 

let listen addr =
  try Join_space.listen addr
  with Join_port.Failed (msg,_e) ->
(*DEBUG*)debug1 "Join.listen" "failed: %s" msg ;
    failwith msg

let connect fd = Join_space.connect fd


let site_service () = here
let site_key = "name_youhou"
let () = Join_prim.register_service site_key site_service

let there addr = 
  let space_id = Join_space.rid_from_addr addr in
  let site = Join_prim.call_service (space_id, site_key) () in
  (site : t)

let where_from ( chan : 'a async ) = (Obj.magic chan : t)

let equal s1 s2 = 
  let id1 = Join_prim.space_id_of_chan s1
  and id2 = Join_prim.space_id_of_chan s2 in
  id1 = id2

and compare s1 s2 =
  Pervasives.compare
    (Join_prim.space_id_of_chan s1)
    (Join_prim.space_id_of_chan s2)
  
let at_fail site chan =
  Join_space.at_fail (Join_prim.space_id_of_chan site) chan  
