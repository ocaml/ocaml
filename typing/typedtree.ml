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

open Misc
open Asttypes

(* Type expressions for the core language *)

type type_expr =
  { mutable desc: type_desc; 
    mutable level: int }

and type_desc =
    Tvar
  | Tarrow of type_expr * type_expr
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * (Path.t * type_expr) list ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * type_expr * type_expr
  | Tnil
  | Tlink of type_expr

(* Value descriptions *)

type value_description =
  { val_type: type_expr;                       (* Type of the val *)
    val_kind: value_kind }

and value_kind =
    Val_reg				(* Regular value *)
  | Val_prim of Primitive.description	(* Primitive *)
  | Val_ivar of mutable_flag		(* Instance variable (mutable ?) *)
  | Val_anc of (string * Ident.t) list  (* Ancestor *)

(* Constructor descriptions *)

type constructor_description =
  { cstr_res: type_expr;                (* Type of the result *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int }               (* Number of non-const constructors *)

and constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_exception of Path.t            (* Exception constructor *)

(* Record label descriptions *)

type label_description =
  { lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation } (* Representation for this record *)

and record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)

(* Type expressions for classes *)

module Vars = Map.Make(struct type t = string let compare = compare end)

type class_type =
  { cty_params: type_expr list;
    cty_args: type_expr list;
    cty_vars: (Asttypes.mutable_flag * type_expr) Vars.t;
    cty_self: type_expr;
    cty_concr: string list;
    mutable cty_new: type_expr option }

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
    exp_type: type_expr }

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
  | Texp_send of expression * string
  | Texp_new of Path.t
  | Texp_instvar of Path.t * Path.t
  | Texp_setinstvar of Path.t * Path.t * expression
  | Texp_override of Path.t * (Path.t * expression) list

(* Type definitions *)

type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_kind;
    type_manifest: type_expr option }

and type_kind =
    Type_abstract
  | Type_variant of (string * type_expr list) list
  | Type_record of (string * mutable_flag * type_expr) list

type exception_declaration = type_expr list

(* Type expressions for the module language *)

type module_type =
    Tmty_ident of Path.t
  | Tmty_signature of signature
  | Tmty_functor of Ident.t * module_type * module_type

and signature = signature_item list

and signature_item =
    Tsig_value of Ident.t * value_description
  | Tsig_type of Ident.t * type_declaration
  | Tsig_exception of Ident.t * exception_declaration
  | Tsig_module of Ident.t * module_type
  | Tsig_modtype of Ident.t * modtype_declaration
  | Tsig_class of Ident.t * class_type

and modtype_declaration =
    Tmodtype_abstract
  | Tmodtype_manifest of module_type

(* Value expressions for classes *)

type class_field =
    Cf_inher of
      Path.t * expression list * (string * Ident.t) list *
      (string * Ident.t) list
  | Cf_val of string * Ident.t * private_flag * expression option
  | Cf_meth of string * expression

type class_def =
  { cl_args: pattern list;
    cl_field: class_field list;
    cl_loc: Location.t }

(* Value expressions for the module language *)

type module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: module_type }

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

(* List the identifiers bound by a pattern or a let *)

let idents = ref([]: Ident.t list)

let rec bound_idents pat =
  match pat.pat_desc with
    Tpat_any -> ()
  | Tpat_var id -> idents := id :: !idents
  | Tpat_alias(p, id) -> bound_idents p; idents := id :: !idents
  | Tpat_constant cst -> ()
  | Tpat_tuple patl -> List.iter bound_idents patl
  | Tpat_construct(cstr, patl) -> List.iter bound_idents patl
  | Tpat_record lbl_pat_list ->
      List.iter (fun (lbl, pat) -> bound_idents pat) lbl_pat_list
  | Tpat_or(p1, p2) -> bound_idents p1; bound_idents p2

let pat_bound_idents pat =
  idents := []; bound_idents pat; let res = !idents in idents := []; res

let rev_let_bound_idents pat_expr_list =
  idents := [];
  List.iter (fun (pat, expr) -> bound_idents pat) pat_expr_list;
  let res = !idents in idents := []; res

let let_bound_idents pat_expr_list =
  List.rev(rev_let_bound_idents pat_expr_list)
