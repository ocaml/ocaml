(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
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

type type_mismatch =
    Arity
  | Privacy
  | Kind
  | Constraint
  | Manifest
  | Variance
  | Field_type of string
  | Field_mutable of string
  | Field_arity of string
  | Field_names of int * string * string
  | Field_missing of bool * string
  | Record_representation of bool

val value_descriptions:
    Env.t -> value_description -> value_description -> module_coercion
val type_declarations:
    Env.t -> string ->
    type_declaration -> Ident.t -> type_declaration -> type_mismatch list
val exception_declarations:
    Env.t -> exception_declaration -> exception_declaration -> bool
(*
val class_types:
        Env.t -> class_type -> class_type -> bool
*)

val report_type_mismatch:
    string -> string -> string -> Format.formatter -> type_mismatch list -> unit
