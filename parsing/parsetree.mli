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

(* Abstract syntax tree produced by parsing *)

open Asttypes

(* Type expressions for the core language *)

type core_type =
  { ptyp_desc: core_type_desc;
    ptyp_loc: Location.t }

and core_type_desc =
    Ptyp_var of string
  | Ptyp_arrow of core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of Longident.t * core_type list * string option
  | Ptyp_object of core_field_type list * string option
  | Ptyp_class of Longident.t * core_type list * string option

and core_field_type =
  { pfield_desc: core_field_desc;
    pfield_loc: Location.t }

and core_field_desc =
    Pfield of string * core_type
  | Pfield_var

(* Value expressions for the core language *)

type pattern =
  { ppat_desc: pattern_desc;
    ppat_loc: Location.t }

and pattern_desc =
    Ppat_any
  | Ppat_var of string
  | Ppat_alias of pattern * string
  | Ppat_constant of constant
  | Ppat_tuple of pattern list
  | Ppat_construct of Longident.t * pattern option
  | Ppat_record of (Longident.t * pattern) list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type

type expression =
  { pexp_desc: expression_desc;
    pexp_loc: Location.t }

and expression_desc =
    Pexp_ident of Longident.t
  | Pexp_constant of constant
  | Pexp_let of rec_flag * (pattern * expression) list * expression
  | Pexp_function of (pattern * expression) list
  | Pexp_apply of expression * expression list
  | Pexp_match of expression * (pattern * expression) list
  | Pexp_try of expression * (pattern * expression) list
  | Pexp_tuple of expression list
  | Pexp_construct of Longident.t * expression option
  | Pexp_record of (Longident.t * expression) list
  | Pexp_field of expression * Longident.t
  | Pexp_setfield of expression * Longident.t * expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of string * expression * expression * direction_flag * expression
  | Pexp_constraint of expression * core_type option * core_type option
  | Pexp_when of expression * expression
  | Pexp_send of expression * string
  | Pexp_new of Longident.t
  | Pexp_setinstvar of string * expression
  | Pexp_override of (string * expression) list

(* Value descriptions *)

type value_description =
  { pval_type: core_type;
    pval_prim: string list }

(* Type declarations *)

type type_declaration =
  { ptype_params: string list;
    ptype_kind: type_kind;
    ptype_manifest: core_type option;
    ptype_loc: Location.t }

and type_kind =
    Ptype_abstract
  | Ptype_variant of (string * core_type list) list
  | Ptype_record of (string * mutable_flag * core_type) list

type exception_declaration = core_type list

(* Type expressions for the class language *)

type class_type_field =
    Pctf_inher of (Longident.t * core_type list * Location.t)
  | Pctf_val of
      (string * private_flag * mutable_flag * core_type option * Location.t)
  | Pctf_virt of (string * core_type * Location.t)
  | Pctf_meth of (string * core_type * Location.t)

type class_type =
  { pcty_name: string;
    pcty_param: string list * Location.t;
    pcty_args: core_type list;
    pcty_self: string option;
    pcty_cstr: (string * core_type * Location.t) list;
    pcty_field: class_type_field list;
    pcty_kind: virtual_flag;
    pcty_closed: closed_flag;
    pcty_loc: Location.t }

type class_field =
    Pcf_inher of (Longident.t * core_type list * expression list *
      	       	  string option * Location.t)
  | Pcf_val of
      (string * private_flag * mutable_flag * expression option * Location.t)
  | Pcf_virt of (string * core_type * Location.t)
  | Pcf_meth of (string * expression * Location.t)

type class_def =
  { pcl_name: string;
    pcl_param: string list * Location.t;
    pcl_args: pattern list;
    pcl_self: string option;
    pcl_self_ty: string option;
    pcl_cstr: (string * core_type * Location.t) list;
    pcl_field: class_field list;
    pcl_kind: virtual_flag;
    pcl_closed: closed_flag;
    pcl_loc: Location.t }

(* Type expressions for the module language *)

type module_type =
  { pmty_desc: module_type_desc;
    pmty_loc: Location.t }

and module_type_desc =
    Pmty_ident of Longident.t
  | Pmty_signature of signature
  | Pmty_functor of string * module_type * module_type
  | Pmty_with of module_type * (Longident.t * with_constraint) list

and signature = signature_item list

and signature_item =
  { psig_desc: signature_item_desc;
    psig_loc: Location.t }

and signature_item_desc =
    Psig_value of string * value_description
  | Psig_type of (string * type_declaration) list
  | Psig_exception of string * exception_declaration
  | Psig_module of string * module_type
  | Psig_modtype of string * modtype_declaration
  | Psig_open of Longident.t
  | Psig_include of module_type
  | Psig_class of class_type list

and modtype_declaration =
    Pmodtype_abstract
  | Pmodtype_manifest of module_type

and with_constraint =
    Pwith_type of type_declaration
  | Pwith_module of Longident.t

(* Value expressions for the module language *)

type module_expr =
  { pmod_desc: module_expr_desc;
    pmod_loc: Location.t }

and module_expr_desc =
    Pmod_ident of Longident.t
  | Pmod_structure of structure
  | Pmod_functor of string * module_type * module_expr
  | Pmod_apply of module_expr * module_expr
  | Pmod_constraint of module_expr * module_type

and structure = structure_item list

and structure_item =
  { pstr_desc: structure_item_desc;
    pstr_loc: Location.t }

and structure_item_desc =
    Pstr_eval of expression
  | Pstr_value of rec_flag * (pattern * expression) list
  | Pstr_primitive of string * value_description
  | Pstr_type of (string * type_declaration) list
  | Pstr_exception of string * exception_declaration
  | Pstr_module of string * module_expr
  | Pstr_modtype of string * module_type
  | Pstr_open of Longident.t
  | Pstr_class of class_def list

(* Toplevel phrases *)

type toplevel_phrase =
    Ptop_def of structure
  | Ptop_dir of string * directive_argument

and directive_argument =
    Pdir_none
  | Pdir_string of string
  | Pdir_int of int
  | Pdir_ident of Longident.t
