(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                 Jeremie Dimino, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright 2015 Jane Street Group LLC                               *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Lesser General Public License version 2.1, with the        *)
(*  special exception on linking described in the file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

open Asttypes
open Parsetree
open Ast_mapper

let err = Syntaxerr.ill_formed_ast

let empty_record loc = err loc "Records cannot be empty."
let empty_variant loc = err loc "Variant types cannot be empty."
let invalid_tuple loc = err loc "Tuples must have at least 2 components."
let no_args loc = err loc "Function application with no argument."
let empty_let loc = err loc "Let with no bindings."
let empty_type loc = err loc "Type declarations cannot be empty."
let complex_id loc = err loc "Functor application not allowed here."

let simple_longident id =
  let rec is_simple = function
    | Longident.Lident _ -> true
    | Longident.Ldot (id, _) -> is_simple id
    | Longident.Lapply _ -> false
  in
  if not (is_simple id.txt) then complex_id id.loc

let mapper =
  let super = Ast_mapper.default_mapper in
  let type_declaration self td =
    let td = super.type_declaration self td in
    let loc = td.ptype_loc in
    match td.ptype_kind with
    | Ptype_record [] -> empty_record loc
    | Ptype_variant [] -> empty_variant loc
    | _ -> td
  in
  let typ self ty =
    let ty = super.typ self ty in
    let loc = ty.ptyp_loc in
    match ty.ptyp_desc with
    | Ptyp_tuple ([] | [_]) -> invalid_tuple loc
    | Ptyp_class (id, _) -> simple_longident id; ty
    | Ptyp_package (_, cstrs) ->
      List.iter (fun (id, _) -> simple_longident id) cstrs;
      ty
    | _ -> ty
  in
  let pat self pat =
    let pat = super.pat self pat in
    let loc = pat.ppat_loc in
    match pat.ppat_desc with
    | Ppat_tuple ([] | [_]) -> invalid_tuple loc
    | Ppat_record ([], _) -> empty_record loc
    | Ppat_construct (id, _) -> simple_longident id; pat
    | Ppat_record (fields, _) ->
      List.iter (fun (id, _) -> simple_longident id) fields;
      pat
    | _ -> pat
  in
  let expr self exp =
    let exp = super.expr self exp in
    let loc = exp.pexp_loc in
    match exp.pexp_desc with
    | Pexp_tuple ([] | [_]) -> invalid_tuple loc
    | Pexp_record ([], _) -> empty_record loc
    | Pexp_apply (_, []) -> no_args loc
    | Pexp_let (_, [], _) -> empty_let loc
    | Pexp_ident id
    | Pexp_construct (id, _)
    | Pexp_field (_, id)
    | Pexp_setfield (_, id, _)
    | Pexp_new id
    | Pexp_open (_, id, _) -> simple_longident id; exp
    | Pexp_record (fields, _) ->
      List.iter (fun (id, _) -> simple_longident id) fields;
      exp
    | _ -> exp
  in
  let extension_constructor self ec =
    let ec = super.extension_constructor self ec in
    match ec.pext_kind with
    | Pext_rebind id -> simple_longident id; ec
    | _ -> ec
  in
  let class_expr self ce =
    let ce = super.class_expr self ce in
    let loc = ce.pcl_loc in
    match ce.pcl_desc with
    | Pcl_apply (_, []) -> no_args loc
    | Pcl_constr (id, _) -> simple_longident id; ce
    | _ -> ce
  in
  let module_type self mty =
    let mty = super.module_type self mty in
    match mty.pmty_desc with
    | Pmty_alias id -> simple_longident id; mty
    | _ -> mty
  in
  let open_description self opn =
    let opn = super.open_description self opn in
    simple_longident opn.popen_lid;
    opn
  in
  let with_constraint self wc =
    let wc = super.with_constraint self wc in
    match wc with
    | Pwith_type (id, _)
    | Pwith_module (id, _) -> simple_longident id; wc
    | _ -> wc
  in
  let module_expr self me =
    let me = super.module_expr self me in
    match me.pmod_desc with
    | Pmod_ident id -> simple_longident id; me
    | _ -> me
  in
  let structure_item self st =
    let st = super.structure_item self st in
    let loc = st.pstr_loc in
    match st.pstr_desc with
    | Pstr_type (_, []) -> empty_type loc
    | Pstr_value (_, []) -> empty_let loc
    | _ -> st
  in
  let signature_item self sg =
    let sg = super.signature_item self sg in
    let loc = sg.psig_loc in
    match sg.psig_desc with
    | Psig_type (_, []) -> empty_type loc
    | _ -> sg
  in
  { super with
    type_declaration
  ; typ
  ; pat
  ; expr
  ; extension_constructor
  ; class_expr
  ; module_expr
  ; module_type
  ; open_description
  ; with_constraint
  ; structure_item
  ; signature_item
  }

let structure st = ignore (mapper.structure mapper st : structure)
let signature sg = ignore (mapper.signature mapper sg : signature)
