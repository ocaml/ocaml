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

type t_global= space_id * int (* value names in network *)

type chan_id = {auto_id:int ; chan_id:int}
and  kont_id = int
and  alone_id = int
and parameter = string * t_global array 

type message = 
  | AsyncSend of chan_id * parameter
  | AloneSend of alone_id * parameter
  | SyncSend of chan_id * kont_id * parameter
  | AloneSyncSend of alone_id * kont_id * parameter
  | ReplySend of kont_id * parameter
  | ReplyExn of kont_id * exn

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
  | DeadConnection

type link_in =
  | NoHandler
  | Handler of in_connection
  | DeadHandler

type remote_space =
    {
      rspace_id : space_id ;
      next_kid : unit -> int ;
      replies_pending : Join_misc.counter ;
      konts : (int, continuation) Join_hash.t ;
      mutable link_in : link_in ;
      mutable link_out : link_out ;
    }  


(* Stubs for handling localized 'values' (eg join-definitions)
   They are implemented trough JoCustom blocks *)

(* custom ops, cf join.c *)
type ops

type stub_tag = Local | Remote

(* Either local value/remote space description *)
type stub_val

type stub =
  {
    ops : ops ;
    stub_tag : stub_tag ;
    mutable stub_val : stub_val ;
    mutable uid : int ; (* identity in space *)
    
  } 

(*
  If stub_tag is Local
    - The site name (local space) is not present in stub.
    - stub_val is the localized value (well pointer to it).
    - uid=0 means that the stub has not been exported yet.
  If stub_tag is Remote
    - the stub stands for value uid at (remote) space stub_val
*)


type listener =
  | Deaf of Unix.file_descr *  Mutex.t
  | Listen of Unix.file_descr

type space_status = SpaceUp | SpaceDown

type space =
  {
    space_id : space_id ;
    mutable space_status : space_status ;
    uid_mutex : Mutex.t ;
    next_uid : unit -> int ;
    uid2local : (int, stub_val) Join_hash.t ;
    remote_spaces : (space_id, remote_space) Join_hash.t ;
    mutable space_listener : listener ;
  } 

