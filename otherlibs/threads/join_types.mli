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

(******************)
(* Local automata *)
(******************)

(* Message queues *)
type queue = Obj.t list

(* Status managers *)
type 'a status =
  {
    set : int -> bool ;   (* set bit, answers true when bit was unset *)
    erase : int -> unit ; (* unset bit *)
    includes : 'a -> bool ; 
    to_string : unit -> string ; (* for debug *)
  } 

(* Local automaton *)
type automaton = {
  mutable ident : int ; 
  status : Obj.t status ; 
  mutex : Mutex.t ;
  queues : queue array ;
  mutable matches : reaction array ;
  names : Obj.t ; (* Used for debug : array of channel names *)
} 

and reaction = Obj.t * int * (Obj.t -> Obj.t)

(*******************)
(* Remote pointers *)
(*******************)

type space_name = Unix.inet_addr * int * float

type global_name = space_name * int

(* Stubs for handling remote pointers,
   they are implemented trough JoCustom blocks *)

type t_local =
  | LocalAutomaton of automaton
  | RemoteAutomaton of global_name

type stub =
  { ops : Obj.t ; (* custom ops, cf. join.c *)
    local : t_local ;
  } 
    
