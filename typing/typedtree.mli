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

(* Abstract syntax tree after typing *)

open Asttypes
open Types

(* Value expressions for the core language *)

type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_type: type_expr }

and pattern_desc =
    Tpat_any
  | Tpat_var of Ident.t
  | Tpat_alias of pattern * Ident.t
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of constructor_description * pattern list
  | Tpat_record of (label_description * pattern) list
  | Tpat_or of pattern * pattern

type expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_type: type_expr;
    exp_env: Env.t }

and expression_desc =
    Texp_ident of Path.t * value_description
  | Texp_constant of constant
  | Texp_let of rec_flag * (pattern * expression) list * expression
  | Texp_function of (pattern * expression) list
  | Texp_apply of expression * expression list
  | Texp_match of expression * (pattern * expression) list
  | Texp_try of expression * (pattern * expression) list
  | Texp_tuple of expression list
  | Texp_construct of constructor_description * expression list
  | Texp_record of (label_description * expression) list
  | Texp_field of expression * label_description
  | Texp_setfield of expression * label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * expression * expression * direction_flag * expression
  | Texp_when of expression * expression
  | Texp_send of expression * meth
  | Texp_new of Path.t
  | Texp_instvar of Path.t * Path.t
  | Texp_setinstvar of Path.t * Path.t * expression
  | Texp_override of Path.t * (Path.t * expression) list
  | Texp_letmodule of Ident.t * module_expr * expression

and meth =
    Tmeth_name of string
  | Tmeth_val of Ident.t

(* Value expressions for classes *)

and class_field =
    Cf_inher of
      Path.t * expression list * (string * Ident.t) list *
      (string * Ident.t) list * string list
  | Cf_val of string * Ident.t * private_flag * expression option
  | Cf_meth of string * expression

and class_def =
  { cl_args: pattern list;
    cl_field: class_field list;
    cl_pub_meths: string list;
    cl_meths: Ident.t Meths.t;
    cl_loc: Location.t }

(* Value expressions for the module language *)

and module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: module_type;
    mod_env: Env.t }

and module_expr_desc =
    Tmod_ident of Path.t
  | Tmod_structure of structure
  | Tmod_functor of Ident.t * module_type * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of module_expr * module_type * module_coercion

and structure = structure_item list

and structure_item =
    Tstr_eval of expression
  | Tstr_value of rec_flag * (pattern * expression) list
  | Tstr_primitive of Ident.t * value_description
  | Tstr_type of (Ident.t * type_declaration) list
  | Tstr_exception of Ident.t * exception_declaration
  | Tstr_module of Ident.t * module_expr
  | Tstr_modtype of Ident.t * module_type
  | Tstr_open of Path.t
  | Tstr_class of (Ident.t * class_def) list

and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of Primitive.description

(* Auxiliary functions over the a.s.t. *)

val pat_bound_idents: pattern -> Ident.t list
val let_bound_idents: (pattern * expression) list -> Ident.t list
val rev_let_bound_idents: (pattern * expression) list -> Ident.t list
