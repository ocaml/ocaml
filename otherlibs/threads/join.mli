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

type argument
type automaton

val get_queue : automaton -> int -> argument

val create_automaton : int -> int -> automaton
(* create_automaton nchans nmatches *)

type reaction

val patch_table : automaton -> reaction array -> unit

val unlock_automaton : automaton -> unit

val send_async : automaton -> int -> argument -> unit
val send_sync : automaton -> int -> argument -> unit

