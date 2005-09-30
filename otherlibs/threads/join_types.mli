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

(* Status managers *)
type 'a status =
  {
    set : int -> bool ;   (* set bit, answers true when bit was unset *)
    erase : int -> unit ; (* unset bit *)
    includes : 'a -> bool ; 
    to_string : unit -> string ; (* for debug *)
  } 

(* Local automaton *)
type queue = Obj.t

type automaton = {
  mutable ident : int ; 
  status : Obj.t status ; 
  mutex : Mutex.t ;
  queues : queue array ;
  mutable matches : reaction array ;
  names : string array ; (* For debug, channel names *)
} 

and reaction = Obj.t * int * (Obj.t -> Obj.t)

type kval = Start | Go of (unit -> Obj.t) | Ret of Obj.t | Exn of exn

type continuation =
  { kmutex : Mutex.t ;
    kcondition : Condition.t ;
    mutable kval : kval }

(*******************)
(* Remote pointers *)
(*******************)

type space_id = Unix.inet_addr * int * float

type global_name = space_id * int

type t_global =
  | GlobalAutomaton of global_name

type message =
  | AsyncSend of
      int (* uid *) * int (* channel *) *
      (string * t_global array) (* parameter *)
  | SyncSend of
      int (* uid *) * int (* channel *) * int (* continuation *) *
      (string * t_global array) (* parameter *)
  | ReplySend of
      int (* continuation *) *
      (string * t_global array) (* parameter *)
  | GoodBye
  | Killed

type out_connection =
  {
    out_queue : message Join_queue.t ;
    out_channel : Unix.file_descr ;
  }

type in_connection =
  { 
    in_channel :  Unix.file_descr ;
  }

type link_out =
  | NoConnection of Mutex.t
  | Connecting of message Join_queue.t
  | Connected of out_connection

type link_in =
  | NoHandler
  | Handler of in_connection


type remote_space =
    {
      rspace_id : space_id ;
      next_kid : unit -> int ;
      konts : (int, continuation) Join_hash.t ;
      mutable link_in : link_in ;
      mutable link_out : link_out ;
    }  

type t_local =
  | LocalAutomaton of automaton
  | RemoteAutomaton of remote_space * int


(* Stubs for handling remote pointers,
   they are implemented trough JoCustom blocks *)

type stub =
  {
    ops : Obj.t ; (* custom ops, cf. join.c *)
    local : t_local ;
  } 

type listener =
  | Deaf of Unix.file_descr *  Mutex.t
  | Listen of Unix.file_descr

type space_status = SpaceUp | SpaceDown

type space =
  {
    space_id : space_id ;
    mutable space_status : space_status ;
    space_mutex : Mutex.t ;
    next_uid : unit -> int ;
    uid2local : (int, automaton) Join_hash.t ;
    remote_spaces : (space_id, remote_space) Join_hash.t ;
    mutable space_listener : listener ;
  } 

