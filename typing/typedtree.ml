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

open Misc
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

(* We use those in Types 
type partial = Partial | Total
type optional = Required | Optional
*)

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
  | Texp_pack of module_expr
(* local contract *)
  | Texp_local_contract of core_contract * expression 
(* below is e |>r1,r2<| c *)
  | Texp_contract of core_contract * expression * expression * expression
  | Texp_bad of blame 
  | Texp_unr of blame
  | Texp_Lambda of Ident.t list * expression 
(* /\'a. e |><| c, 'a is a contractvar *)
  | Texp_App of expression * expression list (* e 'a, 'a is a contractvar *)
(* this is for re-raise contract exception in translcore.ml *)
  | Texp_raise of expression 

(* We use those in Types
and meth =
    Tmeth_name of string
  | Tmeth_val of Ident.t
*)

(* typed axiom declaration *)

and logical_formula = 
  { taxm_desc: logical_formula_desc;
    taxm_loc: Location.t }

and logical_formula_desc = 
  | Taxm_forall of Ident.t list * type_expr * logical_formula
  | Taxm_exist of Ident.t * type_expr * logical_formula
  | Taxm_iff of logical_formula * logical_formula
  | Taxm_imply of logical_formula * logical_formula
  | Taxm_and of logical_formula * logical_formula
  | Taxm_or of logical_formula * logical_formula
  | Taxm_atom of expression

and axiom_declaration = 
  { ttopaxm_id: Path.t;
    ttopaxm_desc: logical_formula;
    ttopaxm_loc: Location.t }

(* typed contracts *) 

and core_contract = 
  { contract_desc: core_contract_desc;
    contract_loc:  Location.t;
    contract_type: type_expr;
    contract_env: Env.t }

and core_contract_desc = 
    Tctr_pred of Ident.t * expression * ((pattern * expression) list) option
  | Tctr_arrow of Ident.t * core_contract * core_contract
  | Tctr_tuple of (Ident.t * core_contract) list
  | Tctr_constr of Path.t * constructor_description 
                          * (Ident.t * core_contract) list
  | Tctr_and of core_contract * core_contract
  | Tctr_or of core_contract * core_contract
  | Tctr_typconstr of Path.t * core_contract list
  | Tctr_var of Ident.t
  | Tctr_poly of Ident.t list * core_contract

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

and class_structure =
  { cl_field: class_field list;
    cl_meths: Ident.t Meths.t }

and class_field =
    Cf_inher of class_expr * (string * Ident.t) list * (string * Ident.t) list
  | Cf_val of string * Ident.t * expression option * bool
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
  | Tmod_unpack of expression * module_type

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
  | Tstr_axiom of axiom_declaration 
 (* contracts in module type signature are put in 
    Tstr_mty_contracts *)
  | Tstr_mty_contracts of 
      (Path.t * Types.contract_declaration) Ident.tbl
 (* opened contracts are put in Tstr_opened_contracts *)
  | Tstr_opened_contracts of 
      (Path.t * Types.contract_declaration) Ident.tbl

(*  We use the module_coercion in Types 
and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of Primitive.description
*)

(* define constructor true and false *)
let trueCon = let true_desc = {cstr_res = Predef.type_bool; 
                       cstr_args = [];
		       cstr_arity = 0;
		       cstr_tag = Cstr_constant 1;
		       cstr_consts = 2;
		       cstr_nonconsts = 0;
		       cstr_private = Public} in
              Tpat_construct (Predef.path_bool, true_desc, [])
      
let falseCon =  let false_desc = {cstr_res = Predef.type_bool; 
                       cstr_args = [];
		       cstr_arity = 0;
		       cstr_tag = Cstr_constant 0;
		       cstr_consts = 2;
		       cstr_nonconsts = 0;
		       cstr_private = Public} in                       
                Tpat_construct (Predef.path_bool, false_desc, [])  

let trueExp = let true_desc = {cstr_res = Predef.type_bool; 
                       cstr_args = [];
		       cstr_arity = 0;
		       cstr_tag = Cstr_constant 1;
		       cstr_consts = 2;
		       cstr_nonconsts = 0;
		       cstr_private = Public} in
              Texp_construct (Predef.path_bool, true_desc, []) 
		
      
let falseExp =  let false_desc = {cstr_res = Predef.type_bool; 
                       cstr_args = [];
		       cstr_arity = 0;
		       cstr_tag = Cstr_constant 0;
		       cstr_consts = 2;
		       cstr_nonconsts = 0;
		       cstr_private = Public} in                       
                Texp_construct (Predef.path_bool, false_desc, [])                

let trueExpression e = {e with exp_desc = trueExp}
let falseExpression e = {e with exp_desc = falseExp}

let truePat e = { pat_desc = trueCon;
                  pat_loc = e.exp_loc;
		  pat_type = e.exp_type;
		  pat_env = e.exp_env }

let falsePat e = { pat_desc = falseCon;
                   pat_loc = e.exp_loc;
	           pat_type = e.exp_type;
		   pat_env = e.exp_env }

(* Auxiliary functions over the a.s.t. *)

let iter_pattern_desc f = function
  | Tpat_alias(p, id) -> f p
  | Tpat_tuple patl -> List.iter f patl
  | Tpat_construct(path, cstr, patl) -> List.iter f patl
  | Tpat_variant(_, pat, _) -> may f pat
  | Tpat_record lbl_pat_list ->
      List.iter (fun (lbl, pat) -> f pat) lbl_pat_list
  | Tpat_array patl -> List.iter f patl
  | Tpat_or(p1, p2, _) -> f p1; f p2
  | Tpat_lazy p -> f p
  | Tpat_any
  | Tpat_var _
  | Tpat_constant _ -> ()

let map_pattern_desc f d =
  match d with
  | Tpat_alias (p1, id) ->
      Tpat_alias (f p1, id)
  | Tpat_tuple pats ->
      Tpat_tuple (List.map f pats)
  | Tpat_record lpats ->
      Tpat_record (List.map (fun (l,p) -> l, f p) lpats)
  | Tpat_construct (path, c,pats) ->
      Tpat_construct (path, c, List.map f pats)
  | Tpat_array pats ->
      Tpat_array (List.map f pats)
  | Tpat_lazy p1 -> Tpat_lazy (f p1)
  | Tpat_variant (x1, Some p1, x2) ->
      Tpat_variant (x1, Some (f p1), x2)
  | Tpat_or (p1,p2,path) ->
      Tpat_or (f p1, f p2, path)
  | Tpat_var _
  | Tpat_constant _
  | Tpat_any
  | Tpat_variant (_,None,_) -> d

let option_to_iface (f:'a->'b) (opt:'a option) = match opt with
      | None -> None
      | Some x -> Some (f x) 

let option_from_iface (f:'a->'b) (opt:'a option) = match opt with
      | None -> None
      | Some x -> Some (f x) 

let rec map_module_expr f modexp = 
     { mod_desc = map_module_expr_desc f modexp.mod_desc;
       mod_loc  = modexp.mod_loc;
       mod_type = modexp.mod_type;
       mod_env  = modexp.mod_env }

and map_structure_item f str_item = match str_item with
  | Tstr_eval (expr) -> Tstr_eval (map_expression f expr)
  | Tstr_value (rec_flag, pat_expr_list) ->
      Tstr_value (rec_flag, 
                  List.map (fun (p, e) -> (p, map_expression f e)) pat_expr_list)
  | Tstr_module (id, modexp) -> Tstr_module (id, map_module_expr f modexp)
  | Tstr_recmodule (id_modexp_list) -> 
      Tstr_recmodule (List.map (fun (i, me) -> (i, map_module_expr f me)) 
                               id_modexp_list)
  | Tstr_class cs -> (* not done yet *) Tstr_class cs
  | Tstr_include (modexp, ids) -> Tstr_include (map_module_expr f modexp, ids)
  | others -> others (* we do not apply f to contract. *)

(* val map_module_expr_desc: (expression_desc -> expression_desc) -> 
                             module_expr_desc -> module_expr_desc  *)

and map_module_expr_desc f modexpr = match modexpr with
  | Tmod_ident (path) -> Tmod_ident path
  | Tmod_structure strs -> 
      Tmod_structure (List.map (map_structure_item f) strs)    
  | Tmod_functor (id, modtype, modexp) -> 
      Tmod_functor (id, modtype, map_module_expr f modexp)
  | Tmod_apply (modexp1, modexp2, cc) -> 
      Tmod_apply (map_module_expr f modexp1, map_module_expr f modexp2, cc)
  | Tmod_constraint (modexp, modtype, cc) ->
      Tmod_constraint (map_module_expr f modexp, modtype, cc)
  | Tmod_unpack (e, modtype) ->
      Tmod_unpack (map_expression f e, modtype)

(* We want to traverse the datatype expression and apply function f
   to the atomic expression, specifically wrapping Tident with contract. 
   map_expression is called in translmod. 

   Why f is not given type expression_desc -> expression_desc?
   Ans: in the function transl_str_contracts in translmod.ml, 
   Texp_contract expects a core_contract and an *expression*.

val map_expression: (expression -> expression) ->  expression -> expression  
val map_expression_aux: (expression -> expression_desc) ->  expression -> expression_desc
*)

and map_expression f (expr:expression) =
   let map_expropt g exprop = match exprop with
       | None -> None
       | Some e -> Some (map_expression g e) in
   let map_expression_aux f expr = match expr.exp_desc with 
    | Texp_let (rec_flag, pat_expr_list, expr1) -> 
        Texp_let (rec_flag, 
                    List.map (fun (p, e) -> (p, map_expression f e)) pat_expr_list, 
                    map_expression f expr1)                               
    | Texp_function (pat_expr_list, partial) ->
        Texp_function (List.map (fun (p, e) -> (p, map_expression f e)) 
                                pat_expr_list, partial) 
    | Texp_apply (expr1, exprop_opt_list) ->
        Texp_apply (map_expression f expr1, 
                                List.map (fun (eop, opt) -> (map_expropt f eop, opt))
                          exprop_opt_list) 
    | Texp_match (expr1, pat_expr_list, partial) ->
        Texp_match (map_expression f expr1, 
                                List.map (fun (p, e) -> (p, map_expression f e)) 
                                pat_expr_list, partial) 
    | Texp_try (expr1,  pat_expr_list) ->
        Texp_try (map_expression f expr1, 
                              List.map (fun (p, e) -> (p, map_expression f e)) 
                              pat_expr_list) 
    | Texp_tuple (expr_list) ->
        Texp_tuple (List.map (map_expression f) expr_list) 
    | Texp_construct (path, constr_desc, expr_list) ->
        Texp_construct (path, constr_desc, List.map (map_expression f) expr_list)
    | Texp_variant (l, exprop) -> 
        Texp_variant (l, map_expropt f exprop)
    | Texp_record (ldesc_expr_list, exprop) ->
        Texp_record (List.map (fun (l, e) -> (l, map_expression f e)) 
                                     ldesc_expr_list, 
                           map_expropt f exprop)
    | Texp_field (expr1, ldesc) -> Texp_field (map_expression f expr1, ldesc) 
    | Texp_setfield (expr1, ldesc, expr2) -> 
        Texp_setfield (map_expression f expr1, ldesc, map_expression f expr2) 
    | Texp_array (expr_list) -> 
        Texp_array (List.map (fun e -> map_expression f e) expr_list)
    | Texp_ifthenelse (expr1, then_expr, exprop) -> 
        Texp_ifthenelse (map_expression f expr1, 
                         map_expression f then_expr, 
                         map_expropt f exprop) 
    | Texp_sequence (expr1, expr2) -> 
        Texp_sequence (map_expression f expr1, map_expression f expr2)
    | Texp_while (expr1, expr2) -> 
        Texp_while (map_expression f expr1, map_expression f expr2)
    | Texp_for (id, expr1, expr2, dir_flag, expr3) ->
        Texp_for (id, map_expression f expr1, map_expression f expr2, 
                  dir_flag, map_expression f expr3) 
    | Texp_when (expr1, expr2) -> 
        Texp_when (map_expression f expr1, map_expression f expr2)
    | Texp_send (expr1, meth) -> Texp_send (map_expression f expr1, meth)
(*  | Texp_new (path, class_decl) -> Texp_new (path, class_decl) *)
    | Texp_setinstvar (path1, path2, expr1) -> 
        Texp_setinstvar (path1, path2, map_expression f expr1)
    | Texp_override (path1, path_expr_list) ->
        Texp_override (path1, List.map (fun (p, e) -> (p, map_expression f e)) 
                                       path_expr_list)
    | Texp_letmodule (id, modexpr, expr1) ->
        Texp_letmodule (id, map_module_expr f modexpr, map_expression f expr1)
    | Texp_assert (expr1) -> Texp_assert (map_expression f expr1)
    | Texp_lazy (expr1) -> Texp_lazy (map_expression f expr1)
(*    | Texp_object (class_str, class_sig, string_list) -> 
        Texp_object (class_str, class_sig, string_list)  *)
    | Texp_contract (c, e, r1, r2) -> 
        Texp_contract (c, map_expression f e, r1, r2) 
    | others -> others
  in 
  let result_exp_desc = map_expression_aux f expr in
  f {expr with exp_desc = result_exp_desc }


(* We want to convert expression to Types.expression *)
let rec module_expr_to_iface modexp = 
     { Types.mod_desc = module_expr_desc_to_iface module_expr_to_iface modexp.mod_desc;
       mod_loc  = modexp.mod_loc;
       mod_type = modexp.mod_type }

and structure_item_to_iface f str_item = match str_item with
  | Tstr_eval (expr) -> Types.Tstr_eval (expression_to_iface expr)
  | Tstr_value (rec_flag, pat_expr_list) ->
      Types.Tstr_value (rec_flag, 
                  List.map (fun (p, e) -> (pattern_to_iface p, 
					   expression_to_iface e)) 
		           pat_expr_list)
  | Tstr_primitive (i, vd) -> Types.Tstr_primitive (i, vd)
  | Tstr_type (ls) -> Types.Tstr_type (ls)
  | Tstr_exception (i, ex) -> Types.Tstr_exception (i, ex)
  | Tstr_exn_rebind (i, path) -> Types.Tstr_exn_rebind (i, path)
  | Tstr_module (id, modexp) -> Types.Tstr_module (id, f modexp)
  | Tstr_recmodule (id_modexp_list) -> 
      Types.Tstr_recmodule (List.map (fun (i, me) -> (i, f me)) 
                               id_modexp_list)
  | Tstr_modtype (i, mty) -> Types.Tstr_modtype (i, mty)
  | Tstr_open (path) -> Types.Tstr_open (path)
  | Tstr_class (ls) -> 
      Types.Tstr_class (List.map (fun (id, i, s, ce, vflag) ->
                              (id, i, s, class_expr_to_iface ce, vflag)) ls)
  | Tstr_cltype (ls) -> Types.Tstr_cltype (ls)  
  | Tstr_include (modexp, ids) -> Types.Tstr_include (f modexp, ids)
  | Tstr_contract (ls) -> Types.Tstr_contract 
  | Tstr_axiom (a) -> Types.Tstr_contract
  | Tstr_mty_contracts(cs) -> Types.Tstr_contract
  | Tstr_opened_contracts (ctbl) -> Types.Tstr_contract

and module_expr_desc_to_iface f modexpr = match modexpr with
  | Tmod_ident (path) -> Types.Tmod_ident path
  | Tmod_structure strs -> 
      Types.Tmod_structure (List.map (structure_item_to_iface f) strs)    
  | Tmod_functor (id, modtype, modexp) -> 
      Types.Tmod_functor (id, modtype, f modexp)
  | Tmod_apply (modexp1, modexp2, cc) -> 
      Types.Tmod_apply (f modexp1, f modexp2, cc)
  | Tmod_constraint (modexp, modtype, cc) ->
      Types.Tmod_constraint (f modexp, modtype, cc)
  | Tmod_unpack (e, modtype) -> 
      Types.Tmod_unpack (expression_to_iface e, modtype)

and pattern_desc_to_iface f desc = match desc with
      | Tpat_alias (p1, id) ->
	  Types.Tpat_alias (f p1, id)
      | Tpat_tuple pats ->
	  Types.Tpat_tuple (List.map f pats)
      | Tpat_record lpats ->
	  Types.Tpat_record (List.map (fun (l,p) -> (l, f p)) lpats)
      | Tpat_construct (path, c,pats) ->
	(* let (path, _) = lookup_constructor_and_path c in *)
	  Types.Tpat_construct (path, c, List.map f pats)
      | Tpat_array pats ->
	  Types.Tpat_array (List.map f pats)
      | Tpat_lazy p1 -> Types.Tpat_lazy (f p1)
      | Tpat_variant (x1, Some p1, x2) ->
	  Types.Tpat_variant (x1, Some (f p1), x2)
      | Tpat_or (p1,p2,path) ->
	  Types.Tpat_or (f p1, f p2, path)
      | Tpat_var (v) -> Types.Tpat_var (v)
      | Tpat_constant (c) -> Types.Tpat_constant (c)
      | Tpat_any -> Types.Tpat_any
      | Tpat_variant (l, patop, rds) -> 
          Types.Tpat_variant(l, option_to_iface pattern_to_iface patop, rds)
 
and pattern_to_iface p = 
   { Types.pat_desc = pattern_desc_to_iface pattern_to_iface p.pat_desc;
     pat_loc = p.pat_loc;
     pat_type = p.pat_type } 

(* iface expression is the expression in Types
val expression_to_iface: (expression -> Types.expression) ->  
                                        expression -> Types.expression 
*)

and expression_desc_to_iface f desc = match desc with 
    | Texp_ident (path, vd) -> Types.Texp_ident (path, vd)
    | Texp_constant (c) -> Types.Texp_constant (c)
    | Texp_let (rec_flag, pat_expr_list, expr1) -> 
        Types.Texp_let (rec_flag, 
                    List.map (fun (p, e) -> (pattern_to_iface p, 
                                             f e)) pat_expr_list, 
                              f expr1)                       
    | Texp_function (pat_expr_list, partial) ->
        Types.Texp_function (List.map (fun (p, e) -> (pattern_to_iface p, 
						      f e)) 
                                pat_expr_list, partial) 
    | Texp_apply (expr1, exprop_opt_list) ->
        Types.Texp_apply (f expr1, 
                          List.map (fun (eop, opl) -> (option_to_iface f eop, opl))
                          exprop_opt_list) 
    | Texp_match (expr1, pat_expr_list, pl) ->
        Types.Texp_match (f expr1, 
                          List.map (fun (p, e) -> (pattern_to_iface p, f e)) 
                                   pat_expr_list, 
                          pl) 
    | Texp_try (expr1,  pat_expr_list) ->
        Types.Texp_try (f expr1, 
                        List.map (fun (p, e) -> (pattern_to_iface p, f e)) 
                                 pat_expr_list) 
    | Texp_tuple (expr_list) ->
        Types.Texp_tuple (List.map (fun e -> f e) expr_list) 
    | Texp_construct (path, constr_desc, expr_list) ->
        Types.Texp_construct (path, constr_desc, 
                              List.map (fun e -> f e) expr_list)
    | Texp_variant (l, exprop) -> 
        Types.Texp_variant (l, option_to_iface f exprop)
    | Texp_record (ldesc_expr_list, exprop) ->
        Types.Texp_record (List.map (fun (l, e) -> (l, f e)) 
                                    ldesc_expr_list, 
                           option_to_iface f exprop)
    | Texp_field (expr1, ldesc) -> 
        Types.Texp_field (f expr1, ldesc) 
    | Texp_setfield (expr1, ldesc, expr2) -> 
        Types.Texp_setfield (f expr1, ldesc, f expr2)
    | Texp_array (expr_list) -> 
        Types.Texp_array (List.map (fun e -> f e) expr_list)
    | Texp_ifthenelse (expr1, then_expr, exprop) -> 
        Types.Texp_ifthenelse (f expr1, 
                         f then_expr, 
                         option_to_iface f exprop) 
    | Texp_sequence (expr1, expr2) -> 
        Types.Texp_sequence (f expr1, 
                             f expr2)
    | Texp_while (expr1, expr2) -> 
        Types.Texp_while (f expr1, 
                          f expr2)
    | Texp_for (id, expr1, expr2, dir_flag, expr3) ->
        Types.Texp_for (id, 
                        f expr1, 
                        f expr2, 
                        dir_flag, 
                        f expr3) 
    | Texp_when (expr1, expr2) -> 
        Types.Texp_when (f expr1, 
                         f expr2)
    | Texp_send (expr1, meth) -> 
        Types.Texp_send (f expr1, meth)
    | Texp_new (path, class_decl) -> Types.Texp_new (path, class_decl)
    | Texp_instvar (p1, p2) -> Types.Texp_instvar (p1, p2)
    | Texp_setinstvar (path1, path2, expr1) -> 
        Types.Texp_setinstvar (path1, path2, f expr1)
    | Texp_override (path1, path_expr_list) ->
        Types.Texp_override (path1, List.map (fun (p, e) -> (p, f e)) 
                                    path_expr_list)
    | Texp_letmodule (id, modexpr, expr1) ->
        Types.Texp_letmodule (id, module_expr_to_iface modexpr, f expr1)
    | Texp_assert (expr1) -> Types.Texp_assert (f expr1)
    | Texp_assertfalse -> Types.Texp_assertfalse
    | Texp_lazy (expr1) -> Types.Texp_lazy (f expr1)
    | Texp_object (class_str, class_sig, string_list) -> 
        Types.Texp_object (class_structure_to_iface class_str, 
                           class_sig, string_list)
    | Texp_pack (modexpr) -> Types.Texp_pack (module_expr_to_iface modexpr)
    | Texp_local_contract (c, e) -> 
        Types.Texp_local_contract(core_contract_to_iface c, f e)
    | Texp_contract (c, e1, e2, e3) -> 
        Types.Texp_contract(core_contract_to_iface c, f e1, f e2, f e3)
    | Texp_bad (bl) -> Types.Texp_bad (bl)
    | Texp_unr (bl) -> Types.Texp_unr (bl)
    | Texp_Lambda (v, e) -> Types.Texp_Lambda (v, f e)
    | Texp_App (e, es) -> Types.Texp_App (f e, List.map f es)
    | Texp_raise (exp) -> Types.Texp_assertfalse (* this should not be reached *)

and expression_to_iface expr =  
  { Types.exp_desc =  expression_desc_to_iface expression_to_iface expr.exp_desc;
    exp_loc  = expr.exp_loc;
    exp_type = expr.exp_type } 

and core_contract_to_iface ccntr = 
  let core_contract_desc_to_iface f desc = match desc with
      | Tctr_pred (i, e, exnop) -> Types.Tctr_pred(i, expression_to_iface e, 
                          match exnop with
                          | None -> None
                          | Some pat_expr_list ->  
                    Some (List.map (fun (p, e) -> (pattern_to_iface p, 
                                                   expression_to_iface e)) 
                          pat_expr_list)) 
      | Tctr_arrow (x, c1, c2) -> Types.Tctr_arrow (x, f c1, f c2)
      | Tctr_tuple (cs) -> Types.Tctr_tuple (List.map (fun (x,c) -> (x, f c)) cs)
      | Tctr_constr (i, cdesc, cs) -> 
            Types.Tctr_constr (i, cdesc, List.map (fun (x, c) -> (x, f c)) cs)
      | Tctr_and (c1, c2) -> Types.Tctr_and (f c1, f c2)
      | Tctr_or (c1, c2) -> Types.Tctr_or (f c1, f c2)
      | Tctr_typconstr (i, cs) -> Types.Tctr_typconstr (i, List.map f cs)
      | Tctr_var (v) -> Types.Tctr_var (v)
      | Tctr_poly (vs, c) -> Types.Tctr_poly (vs, f c)
  in
  { Types.contract_desc = core_contract_desc_to_iface core_contract_to_iface 
						      ccntr.contract_desc;
    contract_loc = ccntr.contract_loc;
    contract_type = ccntr.contract_type }

and contract_declaration_to_iface decl = 
  { Types.ttopctr_id = decl.ttopctr_id;
    Types.ttopctr_desc = core_contract_to_iface decl.ttopctr_desc;
    Types.ttopctr_type = decl.ttopctr_type;
    Types.ttopctr_loc = decl.ttopctr_loc }


and class_expr_to_iface cexpr = 
  let class_expr_desc_to_iface f ce_desc = match ce_desc with
      | Tclass_ident (path) -> Types.Tclass_ident (path)
      | Tclass_structure (cstr) -> 
	  Types.Tclass_structure (class_structure_to_iface cstr)
      | Tclass_fun (p, ies, ce, pl) -> 
          Types.Tclass_fun (pattern_to_iface p, 
                            List.map (fun (i,e) -> (i, expression_to_iface e)) ies,
                            f ce, 
                            pl)
      | Tclass_apply (ce, expop_opls) ->
          Types.Tclass_apply (f ce, 
		      List.map (fun (expop, opl) -> 
                                      (option_to_iface expression_to_iface expop, opl)) 
                                   expop_opls)
      | Tclass_let (rflag, patexps, ies, ce) -> 
          Types.Tclass_let (rflag,
                            List.map (fun (p, e) -> (pattern_to_iface p, 
                                                expression_to_iface e)) patexps,
                            List.map (fun (i, e) -> (i, expression_to_iface e)) ies,
                            f ce)
      | Tclass_constraint (ce, s1, s2, concr) -> 
          Types.Tclass_constraint (f ce, s1, s2, concr)
   in
   { Types.cl_desc = class_expr_desc_to_iface class_expr_to_iface cexpr.cl_desc;
     cl_loc = cexpr.cl_loc;
     cl_type = cexpr.cl_type }

and class_structure_to_iface cstr = 
   { Types.cl_field = List.map class_field_to_iface cstr.cl_field;
     cl_meths = cstr.cl_meths }

and class_field_to_iface cf = match cf with
      | Cf_inher (ce, s1, s2) -> Types.Cf_inher (class_expr_to_iface ce, s1, s2)
      | Cf_val (s, i, expop, b) -> 
	  Types.Cf_val (s, i, option_to_iface expression_to_iface expop, b)
      | Cf_meth (s, e) -> Types.Cf_meth (s, expression_to_iface e)
      | Cf_let (rflag, patexps, ies) -> 
          Types.Cf_let (rflag, 
            List.map (fun (p, e) -> (pattern_to_iface p, expression_to_iface e)) patexps,
            List.map (fun (i, e) -> (i, expression_to_iface e)) ies)
      | Cf_init (e) -> Types.Cf_init (expression_to_iface e)
                                                     
(* construct e |> contract (e ensures t) and e <| contract (e requires t) *)
let ensuresC c expr bl = 
    Texp_contract (c, expr, 
                   { expr with exp_desc = Texp_bad bl},
                   { expr with exp_desc = Texp_unr UnknownBlame })
    
let requiresC c expr bl1 bl2 = 
    Texp_contract (c, expr, 
               { expr with exp_desc = Texp_unr bl2 },
               { expr with exp_desc = Texp_bad bl1 })

(* 
let requiresC c expr r1_path r2_path =
    Texp_contract (c, expr, 
               { expr with exp_desc = Texp_unr (Blame (expr.exp_loc, r2_path)) },
               { expr with exp_desc = Texp_bad (Blame (expr.exp_loc, r1_path)) })
*)


(* List the identifiers bound by a pattern or a let *)

let idents = ref([]: Ident.t list)

let rec bound_idents pat =
  match pat.pat_desc with
  | Tpat_var id -> idents := id :: !idents
  | Tpat_alias(p, id) -> bound_idents p; idents := id :: !idents
  | Tpat_or(p1, _, _) ->
      (* Invariant : both arguments binds the same variables *)
      bound_idents p1
  | d -> iter_pattern_desc bound_idents d

let pat_bound_idents pat =
  idents := []; bound_idents pat; let res = !idents in idents := []; res

let rev_let_bound_idents pat_expr_list =
  idents := [];
  List.iter (fun (pat, expr) -> bound_idents pat) pat_expr_list;
  let res = !idents in idents := []; res

let let_bound_idents pat_expr_list =
  List.rev(rev_let_bound_idents pat_expr_list)

let alpha_var env id = List.assoc id env

let rec alpha_pat env p = match p.pat_desc with
| Tpat_var id -> (* note the ``Not_found'' case *)
    {p with pat_desc =
     try Tpat_var (alpha_var env id) with
     | Not_found -> Tpat_any}
| Tpat_alias (p1, id) ->
    let new_p =  alpha_pat env p1 in
    begin try
      {p with pat_desc = Tpat_alias (new_p, alpha_var env id)}
    with
    | Not_found -> new_p
    end
| d ->
    {p with pat_desc = map_pattern_desc (alpha_pat env) d}

(* We want to convert Types.expression to Typedtree.expression *)
let rec module_expr_from_iface modexp = 
     { mod_desc = module_expr_desc_from_iface module_expr_from_iface modexp.Types.mod_desc;
       mod_loc  = modexp.Types.mod_loc;
       mod_type = modexp.Types.mod_type;
       mod_env  = Env.empty }

and structure_item_from_iface f (str_item:Types.structure_item) = 
  match str_item with
  | Types.Tstr_eval (expr) -> Tstr_eval (expression_from_iface expr)
  | Types.Tstr_value (rec_flag, pat_expr_list) ->
      Tstr_value (rec_flag, 
                  List.map (fun (p, e) -> (pattern_from_iface p, 
					   expression_from_iface e)) 
		           pat_expr_list)
  | Types.Tstr_primitive (i, vd) -> Tstr_primitive (i, vd)
  | Types.Tstr_type (ls) -> Tstr_type (ls)
  | Types.Tstr_exception (i, ex) -> Tstr_exception (i, ex)
  | Types.Tstr_exn_rebind (i, path) -> Tstr_exn_rebind (i, path)
  | Types.Tstr_module (id, modexp) -> Tstr_module (id, f modexp)
  | Types.Tstr_recmodule (id_modexp_list) -> 
      Tstr_recmodule (List.map (fun (i, me) -> (i, f me)) 
                               id_modexp_list)
  | Types.Tstr_modtype (i, mty) -> Tstr_modtype (i, mty)
  | Types.Tstr_open (path) -> Tstr_open (path)
  | Types.Tstr_class (ls) -> 
      Tstr_class (List.map (fun (id, i, s, ce, vflag) ->
             (id, i, s, class_expr_from_iface ce, vflag)) ls)
  | Types.Tstr_cltype (ls) -> Tstr_cltype (ls)  
  | Types.Tstr_include (modexp, ids) -> Tstr_include (f modexp, ids)
  | Types.Tstr_axiom (a) -> Tstr_axiom (axiom_declaration_from_iface a)
  | Types.Tstr_contract -> Tstr_contract []

and module_expr_desc_from_iface f (modexpr:Types.module_expr_desc) = 
  match modexpr with
  | Types.Tmod_ident (path) -> Tmod_ident path
  | Types.Tmod_structure strs -> 
      Tmod_structure (List.map (structure_item_from_iface f) strs)    
  | Types.Tmod_functor (id, modtype, modexp) -> 
      Tmod_functor (id, modtype, f modexp)
  | Types.Tmod_apply (modexp1, modexp2, cc) -> 
      Tmod_apply (f modexp1, f modexp2, cc)
  | Types.Tmod_constraint (modexp, modtype, cc) ->
      Tmod_constraint (f modexp, modtype, cc)
  | Types.Tmod_unpack (e, modtype) -> 
      Tmod_unpack (expression_from_iface e, modtype) 

and pattern_from_iface p = 
  let pattern_desc_from_iface f d = match d with
      | Types.Tpat_alias (p1, id) ->
	  Tpat_alias (f p1, id)
      | Types.Tpat_tuple pats ->
	  Tpat_tuple (List.map f pats)
      | Types.Tpat_record lpats ->
	  Tpat_record (List.map (fun (l,p) -> (l, f p)) lpats)
      | Types.Tpat_construct (path, c,pats) ->
	(* let (path, _) = lookup_constructor_and_path c in *)
	  Tpat_construct (path, c, List.map f pats)
      | Types.Tpat_array pats ->
	  Tpat_array (List.map f pats)
      | Types.Tpat_lazy p1 -> Tpat_lazy (f p1)
      | Types.Tpat_variant (x1, Some p1, x2) ->
	  Tpat_variant (x1, Some (f p1), x2)
      | Types.Tpat_or (p1,p2,path) ->
	  Tpat_or (f p1, f p2, path)
      | Types.Tpat_var (v) -> Tpat_var (v)
      | Types.Tpat_constant (c) -> Tpat_constant (c)
      | Types.Tpat_any -> Tpat_any
      | Types.Tpat_variant (l, patop, rds) -> 
          Tpat_variant(l, option_from_iface pattern_from_iface patop, rds)
  in
   { pat_desc = pattern_desc_from_iface pattern_from_iface p.Types.pat_desc;
     pat_loc  = p.Types.pat_loc;
     pat_type = p.Types.pat_type;
     pat_env  = Env.empty } 

(* from Types. (i.e. iface) to Typedtree. *)
(* iface expression is the expression in Types
val expression_from_iface: (Types.expression -> expression) ->  
                                        Types.expression -> expression 
*)

and expression_from_iface expr = 
   let expression_desc_from_iface f desc = match desc with 
    | Types.Texp_ident (path, vd) -> Texp_ident (path, vd)
    | Types.Texp_constant (c) -> Texp_constant (c)
    | Types.Texp_let (rec_flag, pat_expr_list, expr1) -> 
        Texp_let (rec_flag, 
                    List.map (fun (p, e) -> (pattern_from_iface p, 
                                             f e)) pat_expr_list, 
                              f expr1)                       
    | Types.Texp_function (pat_expr_list, partial) ->
        Texp_function (List.map (fun (p, e) -> (pattern_from_iface p, 
						      f e)) 
                                pat_expr_list, partial) 
    | Types.Texp_apply (expr1, exprop_opt_list) ->
        Texp_apply (f expr1, 
                          List.map (fun (eop, opl) -> (option_from_iface f eop, opl))
                          exprop_opt_list) 
    | Types.Texp_match (expr1, pat_expr_list, pl) ->
        Texp_match (f expr1, 
                          List.map (fun (p, e) -> (pattern_from_iface p, f e)) 
                                   pat_expr_list, 
                          pl) 
    | Types.Texp_try (expr1,  pat_expr_list) ->
        Texp_try (f expr1, 
                        List.map (fun (p, e) -> (pattern_from_iface p, f e)) 
                                 pat_expr_list) 
    | Types.Texp_tuple (expr_list) ->
        Texp_tuple (List.map (fun e -> f e) expr_list) 
    | Types.Texp_construct (path, constr_desc, expr_list) ->
        Texp_construct (path, constr_desc, 
                              List.map (fun e -> f e) expr_list)
    | Types.Texp_variant (l, exprop) -> 
        Texp_variant (l, option_from_iface f exprop)
    | Types.Texp_record (ldesc_expr_list, exprop) ->
        Texp_record (List.map (fun (l, e) -> (l, f e)) 
                                    ldesc_expr_list, 
                           option_from_iface f exprop)
    | Types.Texp_field (expr1, ldesc) -> 
        Texp_field (f expr1, ldesc) 
    | Types.Texp_setfield (expr1, ldesc, expr2) -> 
        Texp_setfield (f expr1, ldesc, f expr2)
    | Types.Texp_array (expr_list) -> 
        Texp_array (List.map (fun e -> f e) expr_list)
    | Types.Texp_ifthenelse (expr1, then_expr, exprop) -> 
        Texp_ifthenelse (f expr1, 
                         f then_expr, 
                         option_from_iface f exprop) 
    | Types.Texp_sequence (expr1, expr2) -> 
        Texp_sequence (f expr1, 
                             f expr2)
    | Types.Texp_while (expr1, expr2) -> 
        Texp_while (f expr1, 
                          f expr2)
    | Types.Texp_for (id, expr1, expr2, dir_flag, expr3) ->
        Texp_for (id, 
                        f expr1, 
                        f expr2, 
                        dir_flag, 
                        f expr3) 
    | Types.Texp_when (expr1, expr2) -> 
        Texp_when (f expr1, 
                         f expr2)
    | Types.Texp_send (expr1, meth) -> 
        Texp_send (f expr1, meth)
    | Types.Texp_new (path, class_decl) -> Texp_new (path, class_decl)
    | Types.Texp_instvar (p1, p2) -> Texp_instvar (p1, p2)
    | Types.Texp_setinstvar (path1, path2, expr1) -> 
        Texp_setinstvar (path1, path2, f expr1)
    | Types.Texp_override (path1, path_expr_list) ->
        Texp_override (path1, List.map (fun (p, e) -> (p, f e)) 
                                    path_expr_list)
    | Types.Texp_letmodule (id, modexpr, expr1) ->
        Texp_letmodule (id, module_expr_from_iface modexpr, f expr1)
    | Types.Texp_assert (expr1) -> Texp_assert (f expr1)
    | Types.Texp_assertfalse -> Texp_assertfalse
    | Types.Texp_lazy (expr1) -> Texp_lazy (f expr1)
    | Types.Texp_object (class_str, class_sig, string_list) -> 
        Texp_object (class_structure_from_iface class_str, 
                           class_sig, string_list)
    | Types.Texp_pack (modexpr) -> 
	Texp_pack (module_expr_from_iface modexpr)
    | Types.Texp_local_contract (c, e) -> 
        Texp_local_contract(core_contract_from_iface c, f e)
    | Types.Texp_contract (c, e1, e2, e3) -> 
        Texp_contract(core_contract_from_iface c, f e1, f e2, f e3)
    | Types.Texp_bad (bl) -> Texp_bad (bl)
    | Types.Texp_unr (bl) -> Texp_unr (bl)
    | Types.Texp_Lambda (v,e) -> Texp_Lambda (v, f e)
    | Types.Texp_App (e, es) -> Texp_App (f e, List.map f es)
  in 
  { exp_desc =  expression_desc_from_iface expression_from_iface expr.Types.exp_desc;
    exp_loc  = expr.Types.exp_loc;
    exp_type = expr.Types.exp_type;
    exp_env  = Env.empty } 

and core_contract_from_iface ccntr = 
  let core_contract_desc_from_iface f desc = match desc with
      | Types.Tctr_pred (i, e, exnop) -> Tctr_pred(i, expression_from_iface e, 
			match exnop with
                          | None -> None
                          | Some pat_expr_list ->  
                    Some (List.map (fun (p, e) -> (pattern_from_iface p, 
                                                   expression_from_iface e)) 
                          pat_expr_list))
      | Types.Tctr_arrow (x, c1, c2) -> Tctr_arrow (x, f c1, f c2)
      | Types.Tctr_tuple (cs) -> Tctr_tuple (List.map (fun (x, c) -> (x, f c)) cs)
      | Types.Tctr_constr (i, cdesc, cs) -> 
          Tctr_constr (i, cdesc, List.map (fun (x, c) -> (x, f c)) cs)
      | Types.Tctr_and (c1, c2) -> Tctr_and (f c1, f c2)
      | Types.Tctr_or (c1, c2) -> Tctr_or (f c1, f c2)
      | Types.Tctr_typconstr (i, cs) -> Tctr_typconstr (i, List.map f cs)
      | Types.Tctr_var (v) -> Tctr_var (v)
      | Types.Tctr_poly (vs, c) -> Tctr_poly (vs, f c)
  in
  { contract_desc = core_contract_desc_from_iface core_contract_from_iface 
					      ccntr.Types.contract_desc;
    contract_loc  = ccntr.Types.contract_loc;
    contract_type = ccntr.Types.contract_type;
    contract_env  = Env.empty }

and contract_declaration_from_iface decl = 
  { ttopctr_id   = decl.Types.ttopctr_id;
    ttopctr_desc = core_contract_from_iface decl.Types.ttopctr_desc;
    ttopctr_type = decl.Types.ttopctr_type;
    ttopctr_loc  = decl.Types.ttopctr_loc }

and logical_formula_from_iface fml = 
 let tfml = match fml.Types.taxm_desc with
  | Types.Taxm_forall (vars, ty, f) -> 
      Taxm_forall (vars, ty, logical_formula_from_iface f)
  | Types.Taxm_exist (var, ty, f) -> 
      Taxm_exist (var, ty, logical_formula_from_iface f)
  | Types.Taxm_iff (f1, f2) -> 
      Taxm_iff (logical_formula_from_iface f1,
		logical_formula_from_iface f2)
  | Types.Taxm_imply (f1, f2) -> 
      Taxm_imply (logical_formula_from_iface f1,
		  logical_formula_from_iface f2)
  | Types.Taxm_and (f1, f2) -> 
      Taxm_and (logical_formula_from_iface f1,
		  logical_formula_from_iface f2)
  | Types.Taxm_or (f1, f2) -> 
      Taxm_or (logical_formula_from_iface f1,
		  logical_formula_from_iface f2)
  | Types.Taxm_atom (e) -> 
      Taxm_atom (expression_from_iface e)
 in { taxm_desc = tfml; taxm_loc = fml.Types.taxm_loc }

and axiom_declaration_from_iface decl = 
  { ttopaxm_id   = decl.Types.ttopaxm_id;
    ttopaxm_desc = logical_formula_from_iface decl.Types.ttopaxm_desc;
    ttopaxm_loc  = decl.Types.ttopaxm_loc }

and class_expr_from_iface cexpr = 
  let class_expr_desc_from_iface f ce_desc = match ce_desc with
      | Types.Tclass_ident (path) -> Tclass_ident (path)
      | Types.Tclass_structure (cstr) -> 
	  Tclass_structure (class_structure_from_iface cstr)
      | Types.Tclass_fun (p, ies, ce, pl) -> 
          Tclass_fun (pattern_from_iface p, 
                      List.map (fun (i,e) -> (i, expression_from_iface e)) ies,
                      f ce, 
                      pl)
      | Types.Tclass_apply (ce, expop_opls) ->
          Tclass_apply (f ce, 
		        List.map (fun (expop, opl) -> 
                         (option_from_iface expression_from_iface expop, opl)) 
                                   expop_opls)
      | Types.Tclass_let (rflag, patexps, ies, ce) -> 
          Tclass_let (rflag,
                            List.map (fun (p, e) -> (pattern_from_iface p, 
                                                expression_from_iface e)) patexps,
                            List.map (fun (i, e) -> (i, expression_from_iface e)) ies,
                            f ce)
      | Types.Tclass_constraint (ce, s1, s2, concr) -> 
          Tclass_constraint (f ce, s1, s2, concr)
   in
   { cl_desc = class_expr_desc_from_iface class_expr_from_iface cexpr.Types.cl_desc;
     cl_loc  = cexpr.Types.cl_loc;
     cl_type = cexpr.Types.cl_type;
     cl_env  = Env.empty }

and class_structure_from_iface cstr = 
   { cl_field = List.map class_field_from_iface cstr.Types.cl_field;
     cl_meths = cstr.Types.cl_meths }

and class_field_from_iface cf = match cf with
      | Types.Cf_inher (ce, s1, s2) -> Cf_inher (class_expr_from_iface ce, s1, s2)
      | Types.Cf_val (s, i, expop, b) -> 
	  Cf_val (s, i, option_from_iface expression_from_iface expop, b)
      | Types.Cf_meth (s, e) -> Cf_meth (s, expression_from_iface e)
      | Types.Cf_let (rflag, patexps, ies) -> 
          Cf_let (rflag, 
            List.map (fun (p, e) -> (pattern_from_iface p, expression_from_iface e)) patexps,
            List.map (fun (i, e) -> (i, expression_from_iface e)) ies)
      | Types.Cf_init (e) -> Cf_init (expression_from_iface e)

open Format

let print_expression type_decls exp = 
  let iface_e = expression_desc_to_iface 
                expression_to_iface exp.exp_desc in 
  fprintf std_formatter "%a" 
       !(Oprint.out_expression_desc type_decls) iface_e

let print_pattern type_decls pat = 
  let iface_e = pattern_desc_to_iface 
                pattern_to_iface pat.pat_desc in 
  fprintf std_formatter "%a" 
       !(Oprint.out_pattern_desc type_decls) iface_e
