(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Modules used by the program. *)
val modules : string list ref

(* Events used by the program *)
val events : Instruct.debug_event list ref
val events_by_pc : (int, Instruct.debug_event) Hashtbl.t
val events_by_file : (string, Instruct.debug_event array) Hashtbl.t

val read_symbols : string -> unit
val event_at_pc : int -> Instruct.debug_event
val event_at_pos : string -> int -> Instruct.debug_event
val set_all_events : unit -> unit
