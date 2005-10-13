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

type 'a t =
  {mutable bag : 'a list ; mutex : Mutex.t ; cond : Condition.t ;
   flush_cond : Condition.t ; mutable flush_wait : int ;
   mutable sender_waits : bool ; }

let create () =
  {
    bag = [] ;
    mutex = Mutex.create () ;
    cond = Condition.create () ;
    flush_cond = Condition.create () ;
    flush_wait = 0 ;
    sender_waits = false ;
  } 

let put q x =
  Mutex.lock q.mutex ;
  q.bag <- x :: q.bag ;
  if q.sender_waits then Condition.signal q.cond ;
  Mutex.unlock q.mutex
   

let rec hard_get q = match q.bag with
| [] ->
    if q.flush_wait > 0 then Condition.broadcast q.flush_cond ;
    assert (not q.sender_waits) ; (* sender is unique *)
    q.sender_waits <- true ;    
    Condition.wait q.cond q.mutex ;
    q.sender_waits <- false ;
    hard_get q
| x::rem ->
    q.bag <- rem ;
    x


let get q =
  Mutex.lock q.mutex ;
  let r = hard_get q in
  Mutex.unlock q.mutex ;
  r

let hard_empty q = match q.bag with
| [] ->
    if not q.sender_waits then
      Condition.wait q.flush_cond q.mutex
| _::_ -> Condition.wait q.flush_cond q.mutex

let wait_empty q = 
  Mutex.lock q.mutex ;
  q.flush_wait <- q.flush_wait + 1 ;
  hard_empty q ;
  q.flush_wait <- q.flush_wait - 1 ;
  Mutex.unlock q.mutex  

let clean q do_it =
  assert (not q.sender_waits) ; (* sender is unique *)
  Mutex.lock q.mutex ;
  let bag = q.bag in
  q.bag <- [] ;
  if q.flush_wait > 0 then Condition.broadcast q.flush_cond ;
  Mutex.unlock q.mutex ;
  List.iter do_it bag
