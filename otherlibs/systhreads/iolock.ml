(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

let master_lock = Mutex.create()

let iolocks = (Hashtbl.create 11 : (Obj.t, Mutex.t) Hashtbl.t)

let add channel =
  let m = Mutex.create() in
  Mutex.lock master_lock;
  Hashtbl.add iolocks (Obj.repr channel) m;
  Mutex.unlock master_lock;
  channel
  
let remove channel =
  Mutex.lock master_lock;
  Hashtbl.remove iolocks (Obj.repr channel);
  Mutex.unlock master_lock

let find channel =
  try
    Hashtbl.find iolocks (Obj.repr channel) 
  with Not_found ->
    invalid_arg "Pervasives: channel closed or not correctly opened"
