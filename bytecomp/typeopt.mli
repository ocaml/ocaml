(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

val has_base_type : Typedtree.expression -> Types.type_expr -> bool
val maybe_pointer : Typedtree.expression -> bool
val array_kind : Typedtree.expression -> Lambda.array_kind
val array_pattern_kind : Typedtree.pattern -> Lambda.array_kind
