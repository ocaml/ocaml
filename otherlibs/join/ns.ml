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
(*DEBUG*)open Printf
(*DEBUG*)open Join_debug


(* Readers/Writer controler, in join *)
def readers(n) & enter_read() = readers(n+1) & reply to enter_read
or  readers(n) & leave_read() = readers(n-1) & reply to leave_read
or  readers(0) & enter_write() = reply to enter_write
and leave_write() = readers(0) & reply to leave_write

let () = spawn readers(0)

let t = (Hashtbl.create 17 : (string, Obj.t) Hashtbl.t)

def do_lookup(key) =
  enter_read() ;
  reply
    try
      let r = Hashtbl.find t key in
      leave_read() ;
      (Obj.obj r : 'a)
    with Not_found ->
      leave_read() ;
      raise Not_found
  to do_lookup

and do_register(key,v) =
  enter_write() ;
  Hashtbl.add t key (Obj.repr v) ;
  leave_write () ;
  reply to do_register


type t =
   { lookup : 'a.string -> 'a ; register : 'a.(string * 'a) -> unit ;
     site : Site.t ;
   }


let ns = { lookup = do_lookup ;  register = do_register ;
           site = Site.here
         }

let here = ns
  
let name_service () = ns

let name_key = "name_zorglub"

let () = Join_prim.register_service name_key name_service

let of_site site =
  let link =
    Join_prim.call_service (Join_prim.space_id_of_chan site, name_key) () in
  (link : t)

let to_site ns = ns.site

let there sockaddr = of_site (Site.there sockaddr)

let of_sockaddr = there


let lookup {lookup=lookup} key = lookup key
and register {register=register} key v = register (key, v)
