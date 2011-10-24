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

type module_coercion = Types.module_coercion

val transl_implementation: string -> structure * module_coercion -> lambda
val transl_store_phrases: string -> structure -> int * lambda
val transl_store_implementation:
      string -> structure * module_coercion -> int * lambda
val transl_toplevel_definition: structure -> lambda
val transl_package:
      Ident.t option list -> Ident.t -> module_coercion -> lambda
val transl_store_package:
      Ident.t option list -> Ident.t -> module_coercion -> int * lambda

val toplevel_name: Ident.t -> string
val nat_toplevel_name: Ident.t -> Ident.t * int

val primitive_declarations: Primitive.description list ref

type error =
  Circular_dependency of Ident.t

exception Error of Location.t * error

val report_error: Format.formatter -> error -> unit
