(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Asttypes
open Parsetree
open Typedtree

val transl_classes:
  Env.t -> Parsetree.class_def list ->
  (Ident.t * class_type * Ident.t * type_declaration *
   Ident.t * type_declaration * class_def) list * Env.t
val transl_class_types:
  Env.t -> Parsetree.class_type list ->
  (Ident.t * class_type * Ident.t * type_declaration *
   Ident.t * type_declaration) list * Env.t


type error =
    Duplicate_method of string
  | Duplicate_variable of string
  | Duplicate_super_variable of string
  | Repeated_parameter
  | Virtual_class of string * string
  | Closed_class of string
  | Closed_ancestor of string * Path.t * string
  | Non_closed of Ident.t * type_expr list * type_expr
  | Mutable_var of string
  | Undefined_var of string
  | Variable_type_mismatch of string * type_expr * type_expr
  | Method_type_mismatch of string * type_expr * type_expr
  | Unconsistent_constraint
  | Unbound_class of Longident.t
  | Argument_type_mismatch of type_expr * type_expr
  | Abbrev_type_clash of type_expr * type_expr * type_expr
  | Bad_parameters of Ident.t * type_expr * type_expr
  | Illdefined_class of string
  | Argument_arity_mismatch of Path.t * int * int
  | Parameter_arity_mismatch of Path.t * int * int
  | Parameter_mismatch of type_expr * type_expr

exception Error of Location.t * error

val report_error : error -> unit
