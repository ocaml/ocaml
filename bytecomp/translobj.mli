(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

val oo_prim: string -> Lambda.lambda

val meth: Label.t -> Ident.t

val reset_labels: unit -> unit
val transl_label_init: Lambda.lambda -> Lambda.lambda
