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

(********************** Configuration file *****************************)

exception Toplevel

(*** Miscellaneous parameters. ***)

val debugger_prompt : string
val event_mark_before : string
val event_mark_after : string
val shell : string
val runtime_program : string
val history_size : int ref

(*** Time travel paramaters. ***)

val checkpoint_big_step : int ref
val checkpoint_small_step : int ref
val checkpoint_max_count : int ref
val make_checkpoints : bool ref

(*** Dynamic loader ***)
val stdlib_units : string list
