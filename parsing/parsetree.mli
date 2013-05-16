(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Abstract syntax tree produced by parsing *)

open Asttypes

(* Type expressions for the core language *)

type core_type =
  { ptyp_desc: core_type_desc;
    ptyp_loc: Location.t }

and core_type_desc =
    Ptyp_any
  | Ptyp_var of string
  | Ptyp_arrow of label * core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of Longident.t loc * core_type list
  | Ptyp_object of core_field_type list
  | Ptyp_class of Longident.t loc * core_type list * label list
  | Ptyp_alias of core_type * string
  | Ptyp_variant of row_field list * bool * label list option
  | Ptyp_poly of string list * core_type
  | Ptyp_package of package_type


and package_type = Longident.t loc * (Longident.t loc * core_type) list

and core_field_type =
  { pfield_desc: core_field_desc;
    pfield_loc: Location.t }

and core_field_desc =
    Pfield of string * core_type
  | Pfield_var

and row_field =
    Rtag of label * bool * core_type list
  | Rinherit of core_type

(* Type expressions for the class language *)

type 'a class_infos =
  { pci_virt: virtual_flag;
    pci_params: string loc list * Location.t;
    pci_name: string loc;
    pci_expr: 'a;
    pci_variance: (bool * bool) list;
    pci_loc: Location.t }

(* Value expressions for the core language *)

type pattern =
  { ppat_desc: pattern_desc;
    ppat_loc: Location.t }

and pattern_desc =
    Ppat_any
  | Ppat_var of string loc
  | Ppat_alias of pattern * string loc
  | Ppat_constant of constant
  | Ppat_tuple of pattern list
  | Ppat_construct of Longident.t loc * pattern option * bool
  | Ppat_variant of label * pattern option
  | Ppat_record of (Longident.t loc * pattern) list * closed_flag
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type
  | Ppat_type of Longident.t loc
  | Ppat_lazy of pattern
  | Ppat_unpack of string loc

type expression =
  { pexp_desc: expression_desc;
    pexp_loc: Location.t }

and expression_desc =
    Pexp_ident of Longident.t loc
  | Pexp_constant of constant
  | Pexp_let of rec_flag * (pattern * expression) list * expression
  | Pexp_function of label * expression option * (pattern * expression) list
  | Pexp_apply of expression * (label * expression) list
  | Pexp_match of expression * (pattern * expression) list
  | Pexp_try of expression * (pattern * expression) list
  | Pexp_tuple of expression list
  | Pexp_construct of Longident.t loc * expression option * bool
  | Pexp_variant of label * expression option
  | Pexp_record of (Longident.t loc * expression) list * expression option
  | Pexp_field of expression * Longident.t loc
  | Pexp_setfield of expression * Longident.t loc * expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of
      string loc *  expression * expression * direction_flag * expression
  | Pexp_constraint of expression * core_type option * core_type option
  | Pexp_when of expression * expression
  | Pexp_send of expression * string
  | Pexp_new of Longident.t loc
  | Pexp_setinstvar of string loc * expression
  | Pexp_override of (string loc * expression) list
  | Pexp_letmodule of string loc * module_expr * expression
  | Pexp_assert of expression
  | Pexp_assertfalse
  | Pexp_lazy of expression
  | Pexp_poly of expression * core_type option
  | Pexp_object of class_structure
  | Pexp_newtype of string * expression
  | Pexp_pack of module_expr
  | Pexp_open of override_flag * Longident.t loc * expression

(* Value descriptions *)

and value_description =
  { pval_type: core_type;
    pval_prim: string list;
    pval_loc: Location.t
    }

(* Type declarations *)

and type_declaration =
  { ptype_params: string loc option list;
    ptype_cstrs: (core_type * core_type * Location.t) list;
    ptype_kind: type_kind;
    ptype_private: private_flag;
    ptype_manifest: core_type option;
    ptype_variance: (bool * bool) list;
    ptype_loc: Location.t }

and type_kind =
    Ptype_abstract
  | Ptype_variant of
      (string loc * core_type list * core_type option * Location.t) list
  | Ptype_record of
      (string loc * mutable_flag * core_type * Location.t) list

and exception_declaration = core_type list

(* Type expressions for the class language *)

and class_type =
  { pcty_desc: class_type_desc;
    pcty_loc: Location.t }

and class_type_desc =
    Pcty_constr of Longident.t loc * core_type list
  | Pcty_signature of class_signature
  | Pcty_fun of label * core_type * class_type

and class_signature = {
    pcsig_self: core_type;
    pcsig_fields: class_type_field list;
    pcsig_loc: Location.t;
  }

and class_type_field = {
    pctf_desc: class_type_field_desc;
    pctf_loc: Location.t;
  }

and class_type_field_desc =
    Pctf_inher of class_type
  | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
  | Pctf_virt  of (string * private_flag * core_type)
  | Pctf_meth  of (string * private_flag * core_type)
  | Pctf_cstr  of (core_type * core_type)

and class_description = class_type class_infos

and class_type_declaration = class_type class_infos

(* Value expressions for the class language *)

and class_expr =
  { pcl_desc: class_expr_desc;
    pcl_loc: Location.t }

and class_expr_desc =
    Pcl_constr of Longident.t loc * core_type list
  | Pcl_structure of class_structure
  | Pcl_fun of label * expression option * pattern * class_expr
  | Pcl_apply of class_expr * (label * expression) list
  | Pcl_let of rec_flag * (pattern * expression) list * class_expr
  | Pcl_constraint of class_expr * class_type

and class_structure = {
    pcstr_pat: pattern;
    pcstr_fields: class_field list;
  }

and class_field = {
    pcf_desc: class_field_desc;
    pcf_loc: Location.t;
  }

and class_field_desc =
    Pcf_inher of override_flag * class_expr * string option
  | Pcf_valvirt of (string loc * mutable_flag * core_type)
  | Pcf_val of (string loc * mutable_flag * override_flag * expression)
  | Pcf_virt of (string loc * private_flag * core_type)
  | Pcf_meth of (string loc * private_flag * override_flag * expression)
  | Pcf_constr of (core_type * core_type)
  | Pcf_init of expression

and class_declaration = class_expr class_infos

(* Type expressions for the module language *)

and module_type =
  { pmty_desc: module_type_desc;
    pmty_loc: Location.t }

and module_type_desc =
    Pmty_ident of Longident.t loc
  | Pmty_signature of signature
  | Pmty_functor of string loc * module_type * module_type
  | Pmty_with of module_type * (Longident.t loc * with_constraint) list
  | Pmty_typeof of module_expr

and signature = signature_item list

and signature_item =
  { psig_desc: signature_item_desc;
    psig_loc: Location.t }

and signature_item_desc =
    Psig_value of string loc * value_description
  | Psig_type of (string loc * type_declaration) list
  | Psig_exception of string loc * exception_declaration
  | Psig_module of string loc * module_type
  | Psig_recmodule of (string loc * module_type) list
  | Psig_modtype of string loc * modtype_declaration
  | Psig_open of override_flag * Longident.t loc
  | Psig_include of module_type
  | Psig_class of class_description list
  | Psig_class_type of class_type_declaration list

and modtype_declaration =
    Pmodtype_abstract
  | Pmodtype_manifest of module_type

and with_constraint =
    Pwith_type of type_declaration
  | Pwith_module of Longident.t loc
  | Pwith_typesubst of type_declaration
  | Pwith_modsubst of Longident.t loc

(* Value expressions for the module language *)

and module_expr =
  { pmod_desc: module_expr_desc;
    pmod_loc: Location.t }

and module_expr_desc =
    Pmod_ident of Longident.t loc
  | Pmod_structure of structure
  | Pmod_functor of string loc * module_type * module_expr
  | Pmod_apply of module_expr * module_expr
  | Pmod_constraint of module_expr * module_type
  | Pmod_unpack of expression

and structure = structure_item list

and structure_item =
  { pstr_desc: structure_item_desc;
    pstr_loc: Location.t }

and structure_item_desc =
    Pstr_eval of expression
  | Pstr_value of rec_flag * (pattern * expression) list
  | Pstr_primitive of string loc * value_description
  | Pstr_type of (string loc * type_declaration) list
  | Pstr_exception of string loc * exception_declaration
  | Pstr_exn_rebind of string loc * Longident.t loc
  | Pstr_module of string loc * module_expr
  | Pstr_recmodule of (string loc * module_type * module_expr) list
  | Pstr_modtype of string loc * module_type
  | Pstr_open of override_flag * Longident.t loc
  | Pstr_class of class_declaration list
  | Pstr_class_type of class_type_declaration list
  | Pstr_include of module_expr

(* Toplevel phrases *)

type toplevel_phrase =
    Ptop_def of structure
  | Ptop_dir of string * directive_argument

and directive_argument =
    Pdir_none
  | Pdir_string of string
  | Pdir_int of int
  | Pdir_ident of Longident.t
  | Pdir_bool of bool
