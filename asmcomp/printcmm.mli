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

(* Pretty-printing of C-- code *)

val machtype_component : Cmm.machtype_component -> unit
val machtype : Cmm.machtype_component array -> unit
val comparison : Cmm.comparison -> unit
val chunk : Cmm.memory_chunk -> unit
val operation : Cmm.operation -> unit
val expression : Cmm.expression -> unit
val fundecl : Cmm.fundecl -> unit
val data : Cmm.data_item list -> unit
val phrase : Cmm.phrase -> unit
