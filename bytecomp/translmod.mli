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

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Typedtree
open Lambda

val transl_implementation: string -> structure -> module_coercion -> lambda
val transl_toplevel_definition: structure -> lambda
