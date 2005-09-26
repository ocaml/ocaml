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

open Join_misc

type ('a,'b) t =
  {
    add : 'a -> 'b -> unit ;
    find : 'a -> 'b ;
    find_remove : 'a -> 'b ;
    get : ('a -> 'b) -> 'a -> 'b ;
  } 

let create () =
  let t =  Hashtbl.create 17
  and c = controler_create () in
  {
    add = protect_write c (fun key v -> Hashtbl.add t key v) ;
    find = protect_read c (fun key -> Hashtbl.find t key) ;
    find_remove =
     protect_write c
       (fun key ->
	 let r = Hashtbl.find t key in
	 Hashtbl.remove t key ;
	 r) ;
    get = protect_write c
      (fun d key ->
        try Hashtbl.find t key
        with Not_found ->
          let r = d key in
          Hashtbl.add t key r ;
          r) ;
  } 


let add t key v = t.add key v
and find t key = t.find key
and find_remove t key = t.find_remove key
and get t d key = t.get d key
