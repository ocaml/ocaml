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

type automaton

val get_queue : automaton -> int -> Obj.t

val create_automaton : int -> automaton
val create_automaton_debug : int -> Obj.t -> automaton
(* create_automaton nchans *)

type reaction

val patch_table : automaton -> reaction array -> unit

type async
val create_async : automaton -> int -> async
val create_async_alone : automaton -> int -> async

(* Asynchronous sends *)

val direct_send_async : automaton -> int -> Obj.t -> unit
val direct_send_async_alone  : automaton -> int -> Obj.t -> unit
val send_async : async -> Obj.t -> unit

val tail_direct_send_async : automaton -> int -> Obj.t -> unit
val tail_direct_send_async_alone : automaton -> int -> Obj.t -> unit
val tail_send_async : async -> Obj.t -> unit

(* Synchornous sends *)

val send_sync : automaton -> int -> Obj.t -> Obj.t
val send_sync_alone : automaton -> int -> Obj.t -> Obj.t

type continuation
val reply_to : Obj.t -> continuation -> unit
