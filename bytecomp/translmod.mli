(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Typedtree
open Lambda

val transl_implementation: string -> structure * module_coercion -> lambda
val transl_store_implementation:
      string -> structure * module_coercion -> int * lambda
val transl_toplevel_definition: structure -> lambda

val primitive_declarations: string list ref
