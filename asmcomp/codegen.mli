(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* From C-- to assembly code *)

val phrase: Cmm.phrase -> unit
val file: string -> unit

val dump_cmm: bool ref
val dump_selection: bool ref
val dump_live: bool ref
val dump_spill: bool ref
val dump_split: bool ref
val dump_interf: bool ref
val dump_prefer: bool ref
val dump_regalloc: bool ref
val dump_reload: bool ref
val dump_linear: bool ref
