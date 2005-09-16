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


val get_queue : 'a automaton -> int -> Obj.t
val create_automaton : int -> 'a automaton

(* create_automaton nchans *)
val create_automaton_debug : int -> Obj.t -> 'a automaton
val wrap_automaton : 'a automaton -> 'a stub
val patch_table : 'a automaton -> 'a reaction array -> unit

(* Asynchronous channels *)
type 'a async
val create_async : 'a stub -> int -> 'a async
val create_async_alone : 'a stub -> int -> 'a async
val local_send_async : 'a automaton -> int -> Obj.t -> unit
val local_tail_send_async : 'a automaton -> int -> Obj.t -> unit
val send_async : 'a async -> Obj.t -> unit
val tail_send_async : 'a async -> Obj.t -> unit

(* Synchronous channels are plain fonctions *)
val create_sync : 'a stub -> int -> (Obj.t -> Obj.t)


type continuation
val reply_to : Obj.t -> continuation -> unit
