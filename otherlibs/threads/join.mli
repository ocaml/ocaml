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

val create_automaton : int -> int -> automaton
(* create_automaton nchans nmatches *)

type reaction

val patch_table : automaton -> reaction array -> unit

val send_async : automaton -> int -> Obj.t -> unit
val send_async_alone : automaton -> int -> Obj.t -> unit
val tail_send_async : automaton -> int -> Obj.t -> unit
val tail_send_async_alone : automaton -> int -> Obj.t -> unit
val send_sync : automaton -> int -> Obj.t -> Obj.t
val send_sync_alone : automaton -> int -> Obj.t -> Obj.t

type continuation
val reply_to : Obj.t -> continuation -> unit
