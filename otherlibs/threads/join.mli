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

val create_process : (unit -> unit) -> unit

type 'a automaton

val get_queue : 'a automaton -> int -> Obj.t

val create_automaton : int -> 'a automaton
(* create_automaton nchans *)
val create_automaton_debug : int -> Obj.t -> 'a automaton


type 'a reaction

val patch_table : 'a automaton -> 'a reaction array -> unit

type 'a async
val create_async : 'a automaton -> int -> 'a async
val create_async_alone : 'a automaton -> int -> 'a async

(* Asynchronous sends *)

val direct_send_async : 'a automaton -> int -> Obj.t -> unit
val direct_send_async_alone  : 'a automaton -> int -> Obj.t -> unit
val send_async : 'a async -> Obj.t -> unit

val tail_direct_send_async : 'a automaton -> int -> Obj.t -> unit
val tail_direct_send_async_alone : 'a automaton -> int -> Obj.t -> unit
val tail_send_async : 'a async -> Obj.t -> unit

(* Synchornous sends *)

val send_sync : 'a automaton -> int -> Obj.t -> Obj.t
val send_sync_alone : 'a automaton -> int -> Obj.t -> Obj.t

type continuation
val reply_to : Obj.t -> continuation -> unit
