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

(* Abstract syntax tree after typing *)

open Asttypes
open Types

(* Value expressions for the core language *)

type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_type: type_expr;
    pat_env: Env.t }

and pattern_desc =
    Tpat_any
  | Tpat_var of Ident.t
  | Tpat_alias of pattern * Ident.t
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of Path.t * constructor_description * pattern list
  | Tpat_variant of label * pattern option * row_desc ref
  | Tpat_record of (label_description * pattern) list
  | Tpat_array of pattern list
  | Tpat_or of pattern * pattern * row_desc option
  | Tpat_lazy of pattern


type expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_type: type_expr;
    exp_env: Env.t }

and expression_desc =
    Texp_ident of Path.t * value_description
  | Texp_constant of constant
  | Texp_let of rec_flag * (pattern * expression) list * expression
  | Texp_function of (pattern * expression) list * partial
  | Texp_apply of expression * (expression option * optional) list
  | Texp_match of expression * (pattern * expression) list * partial
  | Texp_try of expression * (pattern * expression) list
  | Texp_tuple of expression list
  | Texp_construct of Path.t * constructor_description * expression list
  | Texp_variant of label * expression option
  | Texp_record of (label_description * expression) list * expression option
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
  | Texp_new of Path.t * class_declaration
  | Texp_instvar of Path.t * Path.t
  | Texp_setinstvar of Path.t * Path.t * expression
  | Texp_override of Path.t * (Path.t * expression) list
  | Texp_letmodule of Ident.t * module_expr * expression
  | Texp_assert of expression
  | Texp_assertfalse
  | Texp_lazy of expression
  | Texp_object of class_structure * class_signature * string list
(* add below 3 for contract checking *)
  | Texp_contract of core_contract * expression * expression * expression
  | Texp_bad of blame 
  | Texp_unr of blame 

and core_contract = 
  { contract_desc: core_contract_desc;
    contract_loc:  Location.t;
    contract_type: type_expr;
    contract_env: Env.t }

and core_contract_desc = 
    Tctr_pred of Ident.t * expression
  | Tctr_arrow of Ident.t option * core_contract * core_contract
  | Tctr_tuple of core_contract list

and contract_declaration =  
  { ttopctr_id: Path.t;
    ttopctr_desc: core_contract;
    ttopctr_type: type_expr;
    ttopctr_loc: Location.t }


(* Value expressions for the class language *)

and class_expr =
  { cl_desc: class_expr_desc;
    cl_loc: Location.t;
    cl_type: class_type;
    cl_env: Env.t }

and class_expr_desc =
    Tclass_ident of Path.t
  | Tclass_structure of class_structure
  | Tclass_fun of pattern * (Ident.t * expression) list * class_expr * partial
  | Tclass_apply of class_expr * (expression option * optional) list
  | Tclass_let of rec_flag *  (pattern * expression) list *
                  (Ident.t * expression) list * class_expr
  | Tclass_constraint of class_expr * string list * string list * Concr.t
    (* Visible instance variables, methods and concretes methods *)

and class_structure =
  { cl_field: class_field list;
    cl_meths: Ident.t Meths.t }

and class_field =
    Cf_inher of class_expr * (string * Ident.t) list * (string * Ident.t) list
    (* Inherited instance variables and concrete methods *)
  | Cf_val of string * Ident.t * expression option * bool
        (* None = virtual, true = override *)
  | Cf_meth of string * expression
  | Cf_let of rec_flag * (pattern * expression) list *
              (Ident.t * expression) list
  | Cf_init of expression

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
  | Tstr_exn_rebind of Ident.t * Path.t
  | Tstr_module of Ident.t * module_expr
  | Tstr_recmodule of (Ident.t * module_expr) list
  | Tstr_modtype of Ident.t * module_type
  | Tstr_open of Path.t
  | Tstr_class of
      (Ident.t * int * string list * class_expr * virtual_flag) list
  | Tstr_cltype of (Ident.t * cltype_declaration) list
  | Tstr_include of module_expr * Ident.t list
  | Tstr_contract of contract_declaration list
  | Tstr_mty_contracts of (Path.t, Types.contract_declaration) Tbl.t
  | Tstr_opened_contracts of (Path.t, Types.contract_declaration) Tbl.t

(* Auxiliary functions over the a.s.t. *)

val iter_pattern_desc : (pattern -> unit) -> pattern_desc -> unit
val map_pattern_desc : (pattern -> pattern) -> pattern_desc -> pattern_desc

val let_bound_idents: (pattern * expression) list -> Ident.t list
val rev_let_bound_idents: (pattern * expression) list -> Ident.t list

(* Alpha conversion of patterns *)
val alpha_pat : (Ident.t * Ident.t) list -> pattern -> pattern

(* for embed contract to each function call in an expression *)
val map_expression: (expression -> expression) ->  expression -> expression
val expression_to_iface: expression -> Types.expression
val expression_desc_to_iface: (expression -> Types.expression) -> 
                              expression_desc -> Types.expression_desc
val core_contract_to_iface: core_contract -> Types.core_contract
val contract_declaration_to_iface: contract_declaration -> Types.contract_declaration

val core_contract_from_iface: Types.core_contract -> core_contract
 
val ensuresC : core_contract -> expression -> Path.t option -> expression_desc
val requiresC : core_contract -> expression -> 
                Path.t option -> Path.t option -> expression_desc


