(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Type inference for the core language *)

open Asttypes
open Typedtree

val type_binding:
        Env.t -> rec_flag ->
          (Parsetree.pattern * Parsetree.expression) list -> 
            (pattern * expression) list * Env.t
val type_expression:
        Env.t -> Parsetree.expression -> expression
val type_method:
        Env.t -> Typedtree.type_expr -> string option ->
        Parsetree.expression -> expression * type_expr
val type_pattern_list:
        Env.t -> Parsetree.pattern list -> Typedtree.pattern list * Env.t
val type_expect:
        Env.t -> Parsetree.expression -> Typedtree.type_expr ->
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
  | Undefined_method_err of string
  | Unbound_class of Longident.t
  | Virtual_class of Longident.t
  | Unbound_instance_variable of string
  | Instance_variable_not_mutable of string
  | Not_subtype of (type_expr * type_expr) list * (type_expr * type_expr) list
  | Outside_class
  | Value_multiply_overridden of string
  | Coercion_failure of type_expr * type_expr * (type_expr * type_expr) list

exception Error of Location.t * error

val report_error: error -> unit
