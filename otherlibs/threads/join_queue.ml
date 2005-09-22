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

type 'a t = {mutable bag : 'a list ; mutex : Mutex.t ; cond : Condition.t}

let create () =
  {
    bag = [] ;
    mutex = Mutex.create () ;
    cond = Condition.create () ;
  } 

let put q x =
  Mutex.lock q.mutex ;
  q.bag <- x :: q.bag ;
  Condition.signal q.cond ;
  Mutex.unlock q.mutex
   

let rec hard_get q = match q.bag with
| [] ->
    Condition.wait q.cond q.mutex ;
    hard_get q
| x::rem ->
    q.bag <- rem ;
    x


let get q =
  Mutex.lock q.mutex ;
  let r = hard_get q in
  Mutex.unlock q.mutex ;
  r
  
