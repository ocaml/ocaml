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

type t_uniq = int * float

type space_id = { host : string ; uniq : t_uniq ; }

type service = space_id * string

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
 (* Like AloneSyncSend, except that function is given by name *)
  | Service of string * kont_id * parameter

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

(* internal structure for channels *)
type 'a async =
    Async of stub * int
  | Alone of stub * string

type route = Unix.sockaddr

type link =
  | NoConnection of Mutex.t
  | Connecting of Mutex.t * Condition.t
  | Connected of Join_link.t * Mutex.t
  | DeadConnection

type remote_space =
    {
      rspace_id : space_id ;
      next_kid : unit -> int ;
      replies_pending : Join_misc.counter ;
      konts : (int, continuation) Join_hash.t ;
      originator : space_id ;
      route : route Join_set.t ;
      mutable link : link ;
      write_mtx : Mutex.t ;
      mutable hooks : unit async list
    }  


type listener =
  | Deaf of Mutex.t
  | Listen of Unix.sockaddr Join_set.t

type space =
  {
    space_id : space_id ;
    uid_mutex : Mutex.t ;
    next_uid : unit -> int ;
    uid2local : (int, stub_val) Join_hash.t ;
    remote_spaces : (space_id, remote_space) Join_hash.t ;
    services : (string, int) Join_hash.t ;
    mutable listener : listener ;
  } 

