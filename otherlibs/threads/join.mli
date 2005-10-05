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

val get_queue : automaton -> int -> 'a
val init_unit_queue : automaton -> int -> unit

val create_automaton : int -> automaton

(* create_automaton nchans *)
val create_automaton_debug : int -> string array -> automaton
val wrap_automaton : automaton -> stub
val patch_table : automaton -> reaction array -> unit

(* Asynchronous channels *)
type async
val create_async : stub -> int -> async
val create_async_alone : stub -> int -> async
val local_send_async : automaton -> int -> 'a -> unit
val local_tail_send_async : automaton -> int -> 'a -> unit
val send_async : async -> 'a -> unit
val tail_send_async : async -> 'a -> unit

(* Synchronous channels are plain fonctions *)
val create_sync : stub -> int -> ('a -> 'b)

(* Explicit reply to continuation *)
val reply_to : 'a -> continuation -> unit
val reply_to_exn : exn -> continuation -> unit

(* Silent suicide of a join thread (compiler use only) *)
val raise_join_exit : unit -> unit

(* Hook for 'at_exit' will somehow control termination of program.
   More precisely, program terminates when they is no more
   work to achieve.
   This does not apply to program engaged in distribution. *)
val exit_hook : unit -> unit

(* Register an exception as a global one, compiler use *)
val exn_global : (string * int * int) -> Obj.t -> unit

(* Give message to distant sites a chance to leave *)
val flush_space : unit -> unit

val debug0 : string -> string -> unit
val debug1 : string -> string -> unit
val debug2 : string -> string -> unit
val debug3 : string -> string -> unit

val t : 'a -> Marshal.extern_flags list -> 'a
