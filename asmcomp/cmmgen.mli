(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Translation from closed lambda to C-- *)

val compunit: Clambda.ulambda -> Cmm.phrase list

val apply_function: int -> Cmm.phrase
val curry_function: int -> Cmm.phrase list
val entry_point: string list -> Cmm.phrase
val global_table: string list -> Cmm.phrase
val frame_table: string list -> Cmm.phrase
val predef_exception: string -> Cmm.phrase
