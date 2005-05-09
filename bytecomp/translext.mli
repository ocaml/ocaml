(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml + CDuce                    *)
(*                                                                     *)
(*            Alain Frisch, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Translation from typed abstract syntax to lambda terms,
   for the CDuce extension *)

open Lambda

val enter: unit -> unit
val leave: lambda -> lambda

val transl_ext: (Typedtree.expression -> lambda) -> 
  Env.t -> Types.type_expr ->
  Typedtree.ext_exp -> lambda

