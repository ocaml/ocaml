(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Type inference for the core language *)

open Asttypes
open Types

val is_nonexpansive: Typedtree.expression -> bool

val type_binding:
        Env.t -> rec_flag ->
          (Parsetree.pattern * Parsetree.expression) list -> 
          (Typedtree.pattern * Typedtree.expression) list * Env.t
val type_let:
        Env.t -> rec_flag ->
          (Parsetree.pattern * Parsetree.expression) list -> 
          (Typedtree.pattern * Typedtree.expression) list * Env.t
val type_expression:
        Env.t -> Parsetree.expression -> Typedtree.expression
val type_class_arg_pattern:
        string -> Env.t -> Env.t -> Parsetree.pattern ->
        Typedtree.pattern * (Ident.t * Ident.t * type_expr) list *
        Env.t * Env.t
val type_self_pattern:
        string -> Env.t -> Env.t -> Env.t -> Parsetree.pattern ->
        Typedtree.pattern *
        (Ident.t * type_expr) Meths.t ref *
        (Ident.t * Asttypes.mutable_flag * type_expr) Vars.t ref *
        Env.t * Env.t * Env.t
val type_expect:
        Env.t -> Parsetree.expression -> type_expr ->
        Typedtree.expression
val type_exp:
        Env.t -> Parsetree.expression -> Typedtree.expression

type error =
    Unbound_value of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Constructor_arity_mismatch of Longident.t * int * int
  | Label_mismatch of Longident.t * (type_expr * type_expr) list
  | Pattern_type_clash of (type_expr * type_expr) list
  | Multiply_bound_variable
  | Orpat_not_closed
  | Expr_type_clash of (type_expr * type_expr) list
  | Apply_non_function of type_expr
  | Label_multiply_defined of Longident.t
  | Label_missing
  | Label_not_mutable of Longident.t
  | Bad_format of string
  | Undefined_method of type_expr * string
  | Undefined_inherited_method of string
  | Unbound_class of Longident.t
  | Virtual_class of Longident.t
  | Unbound_instance_variable of string
  | Instance_variable_not_mutable of string
  | Not_subtype of (type_expr * type_expr) list * (type_expr * type_expr) list
  | Outside_class
  | Value_multiply_overridden of string
  | Coercion_failure of type_expr * type_expr * (type_expr * type_expr) list
  | Too_many_arguments
  | Scoping_let_module of string * type_expr
  | Masked_instance_variable of Longident.t

exception Error of Location.t * error

val report_error: error -> unit

(* Forward declaration, to be filled in by Typemod.type_module *)
val type_module: (Env.t -> Parsetree.module_expr -> Typedtree.module_expr) ref
