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
    add_once : 'a -> 'b -> 'b option ;
    find : 'a -> 'b ;
    find_remove : 'a -> 'b ;
    get : ('a -> 'b) -> 'a -> 'b ;
    iter : ('a -> 'b -> unit) -> unit ;
    iter_empty : ('a -> 'b -> unit) -> unit ;
    remove : 'a -> unit
  } 

let create () =
  let t =  Hashtbl.create 17
  and c = controler_create () in
  {
    add = protect_write c (fun key v -> Hashtbl.add t key v) ;
    add_once =
      protect_write c
        (fun key v ->
          try
            Some (Hashtbl.find t key)
          with
          | Not_found ->
              Hashtbl.add t key v ;
              None) ;
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
   iter = protect_read c
     (fun do_it -> Hashtbl.iter do_it t) ;
   iter_empty = protect_write c
     (fun do_it -> Hashtbl.iter do_it t ; Hashtbl.clear t) ;
   remove = protect_write c
     (fun key -> Hashtbl.remove t key) ;
  } 


let add t key v = t.add key v
and add_once t key v = t.add_once key v
and find t key = t.find key
and find_remove t key = t.find_remove key
and get t d key = t.get d key
and iter t do_it = t.iter do_it
and iter_empty t do_it = t.iter_empty do_it
and remove t key = t.remove key
