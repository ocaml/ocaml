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

open Join_types

val create_process : (unit -> unit) -> unit


val get_queue : automaton -> int -> Obj.t
val create_automaton : int -> automaton

(* create_automaton nchans *)
val create_automaton_debug : int -> Obj.t -> automaton
val wrap_automaton : automaton -> stub
val patch_table : automaton -> reaction array -> unit

(* Asynchronous channels *)
type async
val create_async : stub -> int -> async
val create_async_alone : stub -> int -> async
val local_send_async : automaton -> int -> Obj.t -> unit
val local_tail_send_async : automaton -> int -> Obj.t -> unit
val send_async : async -> Obj.t -> unit
val tail_send_async : async -> Obj.t -> unit

(* Synchronous channels are plain fonctions *)
val create_sync : stub -> int -> (Obj.t -> Obj.t)


type continuation
val reply_to : Obj.t -> continuation -> unit

val t : 'a -> 'a
