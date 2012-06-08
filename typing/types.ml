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

(* Representation of types and declarations *)

open Misc
open Asttypes

(* Type expressions for the core language *)

type type_expr =
  { mutable desc: type_desc;
    mutable level: int;
    mutable id: int }

and type_desc =
    Tvar
  | Tarrow of label * type_expr * type_expr * commutable
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr         (* for copying *)
  | Tvariant of row_desc
  | Tunivar
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * string list * type_expr list

and row_desc =
    { row_fields: (label * row_field) list;
      row_more: type_expr;
      row_bound: unit;
      row_closed: bool;
      row_fixed: bool;
      row_name: (Path.t * type_expr list) option }

and row_field =
    Rpresent of type_expr option
  | Reither of bool * type_expr list * bool * row_field option ref
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
  | Rabsent

and abbrev_memo =
    Mnil
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

and field_kind =
    Fvar of field_kind option ref
  | Fpresent
  | Fabsent

and commutable =
    Cok
  | Cunknown
  | Clink of commutable ref

module TypeOps = struct
  type t = type_expr
  let compare t1 t2 = t1.id - t2.id
  let hash t = t.id
  let equal t1 t2 = t1 == t2
end

(* Maps of methods and instance variables *)

module OrderedString = struct type t = string let compare = compare end
module Meths = Map.Make(OrderedString)
module Vars = Meths

(* Value descriptions *)

type value_description =
  { val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind }

and value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of (Ident.t * type_expr) Meths.t ref *
                (Ident.t * Asttypes.mutable_flag *
                 Asttypes.virtual_flag * type_expr) Vars.t ref *
                string * type_expr
                                        (* Self *)
  | Val_anc of (string * Ident.t) list * string
                                        (* Ancestor *)
  | Val_unbound                         (* Unbound variable *)

(* Constructor descriptions *)

type constructor_description =
  { cstr_res: type_expr;                (* Type of the result *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;                (* Number of non-const constructors *)
    cstr_private: private_flag }        (* Read-only constructor? *)

and constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_exception of Path.t            (* Exception constructor *)

(* Record label descriptions *)

type label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation;  (* Representation for this record *)
    lbl_private: private_flag }         (* Read-only field? *)

and record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)

(* Type definitions *)

type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_kind;
    type_private: private_flag;
    type_manifest: type_expr option;
    type_variance: (bool * bool * bool) list }
            (* covariant, contravariant, weakly contravariant *)

and type_kind =
    Type_abstract
  | Type_variant of (string * type_expr list) list
  | Type_record of
      (string * mutable_flag * type_expr) list * record_representation

type exception_declaration = type_expr list

(* Type expressions for the class language *)

module Concr = Set.Make(OrderedString)

type class_type =
    Tcty_constr of Path.t * type_expr list * class_type
  | Tcty_signature of class_signature
  | Tcty_fun of label * type_expr * class_type

and class_signature =
  { cty_self: type_expr;
    cty_vars:
      (Asttypes.mutable_flag * Asttypes.virtual_flag * type_expr) Vars.t;
    cty_concr: Concr.t;
    cty_inher: (Path.t * type_expr list) list }

type class_declaration =
  { cty_params: type_expr list;
    mutable cty_type: class_type;
    cty_path: Path.t;
    cty_new: type_expr option;
    cty_variance: (bool * bool) list }

type cltype_declaration =
  { clty_params: type_expr list;
    clty_type: class_type;
    clty_path: Path.t;
    clty_variance: (bool * bool) list }

(* These patterns/expressions are almost the same as those in typedtree except without the field related to type and Env.t. 
   They are used in contracts that are in .mli file. *)
type pattern =
  { pat_desc: pattern_desc;
    pat_loc:  Location.t;
    pat_type: type_expr }


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

type partial = Partial | Total
type optional = Required | Optional

type expression = 
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_type: type_expr }

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
(* add below 3 for contract checking *)
  | Texp_local_contract of core_contract * expression 
  | Texp_contract of core_contract * expression * expression * expression
  | Texp_bad of blame 
  | Texp_unr of blame
  | Texp_Lambda of Ident.t list * expression
  | Texp_App of expression * expression list


and blame = 
    Caller of Location.t * Path.t option * Path.t
  | Callee of Location.t * Path.t
  | UnknownBlame

and meth =
    Tmeth_name of string
  | Tmeth_val of Ident.t

and core_contract = 
  { contract_desc: core_contract_desc;
    contract_loc:  Location.t;
    contract_type: type_expr;}

and core_contract_desc = 
    Tctr_pred of Ident.t * expression * ((pattern * expression) list) option
  | Tctr_arrow of Ident.t option * core_contract * core_contract
  | Tctr_tuple of (Ident.t option * core_contract) list
  | Tctr_constr of Path.t * constructor_description 
                          * (Ident.t option * core_contract) list
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

(* Value expressions for the class language *)

and class_expr =
  { cl_desc: class_expr_desc;
    cl_loc: Location.t;
    cl_type: class_type }

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

(* Type expressions for the module language *)

and module_type =
    Tmty_ident of Path.t
  | Tmty_signature of signature
  | Tmty_functor of Ident.t * module_type * module_type

and signature = signature_item list

and signature_item =
    Tsig_value of Ident.t * value_description
  | Tsig_type of Ident.t * type_declaration * rec_status
  | Tsig_exception of Ident.t * exception_declaration
  | Tsig_module of Ident.t * module_type * rec_status
  | Tsig_modtype of Ident.t * modtype_declaration
  | Tsig_class of Ident.t * class_declaration * rec_status
  | Tsig_cltype of Ident.t * cltype_declaration * rec_status
(* we use contract_declaration instead of core_contract because 
   we want to keep its Path.t info *)
  | Tsig_contract of Ident.t * contract_declaration * rec_status
  | Tsig_axiom of Ident.t * axiom_declaration

and modtype_declaration =
    Tmodtype_abstract
  | Tmodtype_manifest of module_type

and rec_status =
    Trec_not                            (* not recursive *)
  | Trec_first                          (* first in a recursive group *)
  | Trec_next                           (* not first in a recursive group *)


(* Value expressions for the module language *)

and module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: module_type }

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
  | Tstr_axiom of axiom_declaration
  | Tstr_contract 


and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of Primitive.description

(* checking equality *)

let eqMeth m1 m2 = 
  match (m1, m2) with
    (Tmeth_name(s1), Tmeth_name(s2)) -> s1 = s2
  | (Tmeth_val(id1), Tmeth_val(id2)) -> Ident.equal id1 id2
  | (_,_) -> false

let eqOpt cmp o1 o2 = 
  match (o1, o2) with
    (None, None) -> true
  | (Some e1, Some e2) -> cmp e1 e2
  | (_, _) -> false

let eqConstructor_tag t1 t2 = 
  match (t1, t2) with
    (Cstr_constant(i1), Cstr_constant(i2)) -> i1 = i2
  | (Cstr_block(b1), Cstr_block(b2)) -> b1 = b2
  | (Cstr_exception(p1), Cstr_exception(p2)) -> Path.equal p1 p2
  | (_, _) -> false

let eqConstructor_description c1 c2 = eqConstructor_tag c1.cstr_tag c2.cstr_tag

let rec eqPattern_desc pdesc1 pdesc2 = 
  match (pdesc1, pdesc2) with
    (Tpat_any, Tpat_any) -> true
  | (Tpat_var(id1), Tpat_var(id2)) -> Ident.equal id1 id2
  | (Tpat_alias(p1, id1), Tpat_alias(p2, id2)) -> 
      eqPattern p1 p2 && Ident.equal id1 id2
  | (Tpat_constant(c1), Tpat_constant(c2)) -> c1 = c2
  | (Tpat_tuple (pat_list1), Tpat_tuple (pat_list2)) -> 
       List.for_all2  (fun p1 p2 -> eqPattern p1 p2) pat_list1 pat_list2
  | (Tpat_construct(path1, constr_desc1, pat_list1),
     Tpat_construct(path2, constr_desc2, pat_list2)) ->
      Path.equal path1 path2 &&
      eqConstructor_description constr_desc1 constr_desc2 &&
      List.for_all2  (fun p1 p2 -> eqPattern p1 p2) pat_list1 pat_list2
  | (Tpat_variant(l1,patopt1,r_desc1), Tpat_variant(l2,patopt2,r_desc2)) ->
      eqOpt eqPattern patopt1 patopt2
  | (Tpat_record(ldesc_pat_list1), Tpat_record(ldesc_pat_list2)) ->
       List.for_all2  (fun (l1,p1) (l2,p2) -> eqPattern p1 p2) 
	              ldesc_pat_list1 ldesc_pat_list2
  | (Tpat_array(pat_list1), Tpat_array(pat_list2)) -> 
       List.for_all2  (fun p1 p2 -> eqPattern p1 p2) pat_list1 pat_list2
  | (Tpat_or(p1,p2,r_desc1), Tpat_or(p3,p4,r_desc2)) -> 
      eqPattern p1 p3 && eqPattern p2 p4 
  | (Tpat_lazy(p1), Tpat_lazy(p2)) -> eqPattern p1 p2
  | (_,_) -> false

and eqPattern p1 p2 = eqPattern_desc p1.pat_desc p2.pat_desc

let rec eqExpression_desc edesc1 edesc2 = 
  match (edesc1, edesc2) with
    (Texp_ident(path1, _), Texp_ident(path2, _)) -> Path.equal path1 path2
  | (Texp_constant(c1), Texp_constant(c2)) -> c1 = c2
  | (Texp_let (rec_flag1, pat_expr_list1, expr1), 
   Texp_let (rec_flag2, pat_expr_list2, expr2)) ->
     rec_flag1 = rec_flag2 && 
     List.for_all2  (fun (p1,e1) (p2,e2) -> 
			 eqPattern p1 p2 && eqExpression e1 e2) 
                    pat_expr_list1 pat_expr_list2 && 
     eqExpression expr1 expr2	   			
  | (Texp_function (pat_expr_list1, ptial1),
     Texp_function (pat_expr_list2, ptial2)) ->
     List.for_all2  (fun (p1,e1) (p2,e2) -> 
			 eqPattern p1 p2 && eqExpression e1 e2) 
                    pat_expr_list1 pat_expr_list2 &&
     ptial1 = ptial2
  | (Texp_apply (expr1, exprop_optnl_list1), 
     Texp_apply (expr2, exprop_optnl_list2)) ->
     eqExpression expr1 expr2 &&	   	
     List.for_all2  (fun (eo1,o1) (eo2,o2) -> 
			 eqOpt eqExpression eo1 eo2 && o1 = o2) 
                    exprop_optnl_list1 exprop_optnl_list2 
  | (Texp_match (expr1, pat_expr_list1, ptial1),
     Texp_match (expr2, pat_expr_list2, ptial2)) ->
     eqExpression expr1 expr2 &&	   	
     List.for_all2  (fun (p1,e1) (p2,e2) -> 
			 eqPattern p1 p2 && eqExpression e1 e2) 
                    pat_expr_list1 pat_expr_list2 &&
     ptial1 = ptial2
  | (Texp_try (expr1, pat_expr_list1), Texp_try (expr2, pat_expr_list2)) ->
     eqExpression expr1 expr2 &&	   	
     List.for_all2  (fun (p1,e1) (p2,e2) -> 
			 eqPattern p1 p2 && eqExpression e1 e2) 
                    pat_expr_list1 pat_expr_list2 
  | (Texp_tuple (expr_list1), Texp_tuple (expr_list2)) ->
      List.for_all2  (fun e1 e2 -> eqExpression e1 e2) expr_list1 expr_list2
  | (Texp_construct (path1, constr_desc1, expr_list1),
     Texp_construct (path2, constr_desc2, expr_list2)) ->
      Path.equal path1 path2 && 
      eqConstructor_description constr_desc1 constr_desc2 &&
      List.for_all2  (fun e1 e2 -> eqExpression e1 e2) expr_list1 expr_list2
  | (Texp_variant (l1, expropt1), Texp_variant (l2, expropt2)) ->
      l1 = l2 && eqOpt eqExpression expropt1 expropt2
  | (Texp_record (ldesc_expr_list1, expropt1), 
     Texp_record (ldesc_expr_list2, expropt2)) ->
      List.for_all2 (fun (l1, e1) (l2, e2) -> eqExpression e1 e2) 
                    ldesc_expr_list1  ldesc_expr_list2  &&
      eqOpt eqExpression expropt1 expropt2
  | (Texp_field (expr1, ldesc1), Texp_field (expr2, ldesc2)) ->
      eqExpression expr1 expr2 
  | (Texp_setfield (expr1, ldesc1, expr2), 
     Texp_setfield (expr3, ldesc2, expr4)) ->
      eqExpression expr1 expr3 && eqExpression expr2 expr4	   
  | (Texp_array (expr_list1), Texp_array (expr_list2)) ->
  	List.for_all2  (fun e1 e2 -> eqExpression e1 e2) expr_list1 expr_list2
  | (Texp_ifthenelse (expr1, then_expr1, expropt1),
     Texp_ifthenelse (expr2, then_expr2, expropt2)) ->
      eqExpression expr1 expr2 && eqExpression then_expr1 then_expr2 &&
      eqOpt eqExpression expropt1 expropt2 
  | (Texp_sequence (expr1, expr2), Texp_sequence (expr3, expr4)) ->
      eqExpression expr1 expr3 && eqExpression expr2 expr4	   
  | (Texp_while (expr1, expr2), Texp_while (expr3, expr4)) ->
      eqExpression expr1 expr3 && eqExpression expr2 expr4	   
  | (Texp_for (id1, expr1, expr2, dir_flag1, expr3),
     Texp_for (id2, expr4, expr5, dir_flag2, expr6)) -> 
      Ident.equal id1 id2 && eqExpression expr1 expr4 &&
      eqExpression expr2 expr5 && eqExpression expr3 expr6	   
  | (Texp_when (expr1, expr2), Texp_when (expr3, expr4)) ->
      eqExpression expr1 expr3 && eqExpression expr2 expr4	   
  | (Texp_send (expr1, meth1), Texp_send (expr2, meth2)) ->
      eqExpression expr1 expr2 && eqMeth meth1 meth2	   
  | (Texp_new (path1, class_decl1), Texp_new (path2, class_decl2)) -> 
      Path.equal path1 path2
  | (Texp_setinstvar (path1, path2, expr1), 
      Texp_setinstvar (path3, path4, expr2)) -> 
      Path.equal path1 path3 &&
      Path.equal path2 path4 &&
      eqExpression expr1 expr2	   
  | (Texp_override (path1, path_expr_list1),
     Texp_override (path2, path_expr_list2)) ->
  	Path.equal path1 path2 &&
  	List.for_all2 (fun (p1, e1) (p2,e2) -> 
  		       Path.equal p1 p2 && eqExpression e1 e2)
                       path_expr_list1 path_expr_list2
  | (Texp_letmodule (id1, modexpr1, expr1), 
     Texp_letmodule (id2, modexpr2, expr2)) ->
      Ident.equal id1 id2 && (* NOT DONE eqModexpr yet *)
      eqExpression expr1 expr2			
  | (Texp_assert (expr1), Texp_assert (expr2)) -> 
      eqExpression expr1 expr2	   
  | (Texp_lazy (expr1), Texp_lazy (expr2)) ->
      eqExpression expr1 expr2	   
  | (Texp_object (class_str1, class_sig1, string_list1),
      Texp_object (class_str2, class_sig2, string_list2)) ->
      class_str1 = class_str2 && class_sig1 = class_sig2 &&
  	string_list1 = string_list2						
  | (_,_) -> false
	    
and eqExpression expr1 expr2 = eqExpression_desc expr1.exp_desc expr2.exp_desc 

(* This is for checking contracts declared in .ml and contracts in .mli 
   are the same or not *)

let rec eqContract_desc c1 c2 = 
  match (c1, c2) with
  | (Tctr_pred (id1, exp1, exns1), Tctr_pred (id2, exp2, exns2)) ->       
      let eq_exns = match (exns1, exns2) with
      | (None, None) -> true
      | (Some exn1, Some exn2) -> List.for_all2 (fun (p1,e1) (p2,e2) ->
                                   eqPattern p1 p2 &&
                                   eqExpression e1 e2) exn1 exn2
      | (_, _) -> false
      in
      eq_exns && (* exception constructors *)
      Ident.name id1 = Ident.name id2 && 
      eqExpression_desc exp1.exp_desc exp2.exp_desc
  | (Tctr_arrow (id_opt1, cc11, cc12), Tctr_arrow (id_opt2, cc21, cc22)) ->
      begin
      match (id_opt1, id_opt2) with
      | (None, None) -> eqContract cc11 cc21 && eqContract cc12 cc22
      | (Some id1, Some id2) -> Ident.name id1 = Ident.name id2 && 
	                        eqContract cc11 cc21 && eqContract cc12 cc22
      | (_, _) -> false
      end
  | (Tctr_tuple (clist1), Tctr_tuple (clist2)) -> 
      List.for_all (fun ((vo1,c1), (vo2,c2)) -> 
          vo1 = vo2 &&  eqContract c1 c2) 
      (List.combine clist1 clist2)
  | (Tctr_constr (p1, cdesc1, cs1), Tctr_constr (p2, cdesc2, cs2)) ->
      p1 = p2 && 
      List.for_all (fun ((vo1, c1), (vo2,c2)) -> 
          vo1 = vo2 && eqContract c1 c2)
                   (List.combine cs1 cs2)  
  | (Tctr_and (c1, c2), Tctr_and (c3, c4)) -> 
      eqContract c1 c3 && eqContract c2 c4
  | (Tctr_or (c1, c2), Tctr_and (c3, c4)) -> 
      eqContract c1 c3 && eqContract c2 c4
  | (Tctr_typconstr (p1, cs1), Tctr_typconstr (p2, cs2)) -> 
      p1 = p2 && 
      List.for_all (fun (c1, c2) -> eqContract c1 c2)
                   (List.combine cs1 cs2)  
  | (Tctr_var (id1), Tctr_var (id2)) -> Ident.name id1 = Ident.name id2 
  | (Tctr_poly (ids1, c1), Tctr_poly (ids2, c2)) ->
      List.for_all (fun (id1, id2) -> Ident.name id1 = Ident.name id2)
                   (List.combine ids1 ids2)  &&
      eqContract c1 c2
  | (_, _) -> false

and eqContract c1 c2 = eqContract_desc c1.contract_desc c2.contract_desc

(* functions map_x traverse data structure x and apply function f to some nodes *)

let rec map_module_expr f modexp = 
     { mod_desc = map_module_expr_desc f modexp.mod_desc;
       mod_loc  = modexp.mod_loc;
       mod_type = modexp.mod_type }

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
   to the atomic expression, map_expression is called in subst.ml where
   we want to change all ident in Tsig_contract to their full path 
   (i.e. including module name). 

   Why f is not given type expression_desc -> expression_desc?
   Ans: in the function transl_str_contracts in translmod.ml, 
   Texp_contract expects a core_contract and an *expression*.

val map_expression: (expression -> expression) ->  expression -> expression  
val map_expression_aux: (expression -> expression_desc) ->  expression -> expression_desc
*)

and map_expression f (expr:expression) =
   let map_exprop f exprop = match exprop with
       | None -> None
       | Some e -> Some (map_expression f e) in
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
                                List.map (fun (eop, opt) -> (map_exprop f eop, opt))
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
        Texp_tuple (List.map (fun e -> map_expression f e) expr_list) 
    | Texp_construct (path, constr_desc, expr_list) ->
        Texp_construct (path, constr_desc, List.map (fun e -> map_expression f e) 
						    expr_list)
    | Texp_variant (l, exprop) -> 
        Texp_variant (l, map_exprop f exprop)
    | Texp_record (ldesc_expr_list, exprop) ->
        Texp_record (List.map (fun (l, e) -> (l, map_expression f e)) 
                                     ldesc_expr_list, 
                           map_exprop f exprop)
 (* | Texp_field (expr1, ldesc) -> Texp_field (map_expression f expr1, ldesc) 
    | Texp_setfield (expr1, ldesc, expr2) -> 
        Texp_setfield (map_expression f expr1, ldesc, map_expression f expr2) *)
    | Texp_array (expr_list) -> 
        Texp_array (List.map (fun e -> map_expression f e) expr_list)
    | Texp_ifthenelse (expr1, then_expr, exprop) -> 
        Texp_ifthenelse (map_expression f expr1, 
                         map_expression f then_expr, 
                         map_exprop f exprop) 
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
  f { expr with exp_desc = result_exp_desc }

