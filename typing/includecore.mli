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

(* Inclusion checks for the core language *)

open Types
open Typedtree

exception Dont_match

val value_descriptions:
        Env.t -> value_description -> value_description -> module_coercion
val type_declarations:
        Env.t -> Ident.t -> type_declaration -> type_declaration -> bool
val exception_declarations:
        Env.t -> exception_declaration -> exception_declaration -> bool
(*
val class_types:
        Env.t -> class_type -> class_type -> bool
*)
