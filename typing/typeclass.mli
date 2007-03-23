(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Asttypes
open Types
open Typedtree
open Format

val class_declarations:
  Env.t -> Parsetree.class_declaration list ->
  (Ident.t * class_declaration *
   Ident.t * cltype_declaration *
   Ident.t * type_declaration *
   Ident.t * type_declaration *
   int * string list * class_expr) list * Env.t

val class_descriptions:
  Env.t -> Parsetree.class_description list ->
  (Ident.t * class_declaration *
   Ident.t * cltype_declaration *
   Ident.t * type_declaration *
   Ident.t * type_declaration *
   int * string list * class_type) list * Env.t

val class_type_declarations:
  Env.t -> Parsetree.class_description list ->
  (Ident.t * cltype_declaration *
   Ident.t * type_declaration *
   Ident.t * type_declaration) list * Env.t

val approx_class_declarations:
  Env.t -> Parsetree.class_description list ->
  (Ident.t * cltype_declaration *
   Ident.t * type_declaration *
   Ident.t * type_declaration) list

val virtual_methods: Types.class_signature -> label list

type error =
    Unconsistent_constraint of (type_expr * type_expr) list
  | Field_type_mismatch of string * string * (type_expr * type_expr) list
  | Structure_expected of class_type
  | Cannot_apply of class_type
  | Apply_wrong_label of label
  | Pattern_type_clash of type_expr
  | Repeated_parameter
  | Unbound_class of Longident.t
  | Unbound_class_2 of Longident.t
  | Unbound_class_type of Longident.t
  | Unbound_class_type_2 of Longident.t
  | Abbrev_type_clash of type_expr * type_expr * type_expr
  | Constructor_type_mismatch of string * (type_expr * type_expr) list
  | Virtual_class of bool * string list * string list
  | Parameter_arity_mismatch of Longident.t * int * int
  | Parameter_mismatch of (type_expr * type_expr) list
  | Bad_parameters of Ident.t * type_expr * type_expr
  | Class_match_failure of Ctype.class_match_failure list
  | Unbound_val of string
  | Unbound_type_var of (formatter -> unit) * Ctype.closed_class_failure
  | Make_nongen_seltype of type_expr
  | Non_generalizable_class of Ident.t * Types.class_declaration
  | Cannot_coerce_self of type_expr
  | Non_collapsable_conjunction of
      Ident.t * Types.class_declaration * (type_expr * type_expr) list
  | Final_self_clash of (type_expr * type_expr) list
  | Mutability_mismatch of string * mutable_flag

exception Error of Location.t * error

val report_error : formatter -> error -> unit
