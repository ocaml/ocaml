(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Asttypes
open Format
open Location
open Longident
open Parsetree

module StringSet = Set.Make(struct type t = string let compare = compare end)

(* Collect free module identifiers in the a.s.t. *)

let fst3 (x, _, _) = x

let free_structure_names = ref StringSet.empty

let rec addmodule bv lid =
  match lid with
    Lident s ->
      if not (StringSet.mem s bv)
      then free_structure_names := StringSet.add s !free_structure_names
  | Ldot(l, s) -> addmodule bv l
  | Lapply(l1, l2) -> addmodule bv l1; addmodule bv l2

let add bv lid =
  match lid.txt with
    Ldot(l, s) -> addmodule bv l
  | _ -> ()

let addmodule bv lid = addmodule bv lid.txt

let rec add_type bv ty =
  match ty.ptyp_desc with
    Ptyp_any -> ()
  | Ptyp_var v -> ()
  | Ptyp_arrow(_, t1, t2) -> add_type bv t1; add_type bv t2
  | Ptyp_tuple tl -> List.iter (add_type bv) tl
  | Ptyp_constr(c, tl) -> add bv c; List.iter (add_type bv) tl
  | Ptyp_object fl -> List.iter (add_field_type bv) fl
  | Ptyp_class(c, tl, _) -> add bv c; List.iter (add_type bv) tl
  | Ptyp_alias(t, s) -> add_type bv t
  | Ptyp_variant(fl, _, _) ->
      List.iter
        (function Rtag(_,_,stl) -> List.iter (add_type bv) stl
          | Rinherit sty -> add_type bv sty)
        fl
  | Ptyp_poly(_, t) -> add_type bv t
  | Ptyp_package pt -> add_package_type bv pt

and add_package_type bv (lid, l) =
  add bv lid;
  List.iter (add_type bv) (List.map (fun (_, e) -> e) l)

and add_field_type bv ft =
  match ft.pfield_desc with
    Pfield(name, ty) -> add_type bv ty
  | Pfield_var -> ()

let add_opt add_fn bv = function
    None -> ()
  | Some x -> add_fn bv x

let add_type_declaration bv td =
  List.iter
    (fun (ty1, ty2, _) -> add_type bv ty1; add_type bv ty2)
    td.ptype_cstrs;
  add_opt add_type bv td.ptype_manifest;
  let rec add_tkind = function
    Ptype_abstract -> ()
  | Ptype_variant cstrs ->
      List.iter (fun (c, args, rty, _) -> List.iter (add_type bv) args; Misc.may (add_type bv) rty) cstrs
  | Ptype_record lbls ->
      List.iter (fun (l, mut, ty, _) -> add_type bv ty) lbls in
  add_tkind td.ptype_kind

let rec add_class_type bv cty =
  match cty.pcty_desc with
    Pcty_constr(l, tyl) ->
      add bv l; List.iter (add_type bv) tyl
  | Pcty_signature { pcsig_self = ty; pcsig_fields = fieldl } ->
      add_type bv ty;
      List.iter (add_class_type_field bv) fieldl
  | Pcty_fun(_, ty1, cty2) ->
      add_type bv ty1; add_class_type bv cty2

and add_class_type_field bv pctf =
  match pctf.pctf_desc with
    Pctf_inher cty -> add_class_type bv cty
  | Pctf_val(_, _, _, ty) -> add_type bv ty
  | Pctf_virt(_, _, ty) -> add_type bv ty
  | Pctf_meth(_, _, ty) -> add_type bv ty
  | Pctf_cstr(ty1, ty2) -> add_type bv ty1; add_type bv ty2

let add_class_description bv infos =
  add_class_type bv infos.pci_expr

let add_class_type_declaration = add_class_description

let pattern_bv = ref StringSet.empty

let rec add_pattern bv pat =
  match pat.ppat_desc with
    Ppat_any -> ()
  | Ppat_var _ -> ()
  | Ppat_alias(p, _) -> add_pattern bv p
  | Ppat_constant _ -> ()
  | Ppat_tuple pl -> List.iter (add_pattern bv) pl
  | Ppat_construct(c, op, _) -> add bv c; add_opt add_pattern bv op
  | Ppat_record(pl, _) ->
      List.iter (fun (lbl, p) -> add bv lbl; add_pattern bv p) pl
  | Ppat_array pl -> List.iter (add_pattern bv) pl
  | Ppat_or(p1, p2) -> add_pattern bv p1; add_pattern bv p2
  | Ppat_constraint(p, ty) -> add_pattern bv p; add_type bv ty
  | Ppat_variant(_, op) -> add_opt add_pattern bv op
  | Ppat_type li -> add bv li
  | Ppat_lazy p -> add_pattern bv p
  | Ppat_unpack id -> pattern_bv := StringSet.add id.txt !pattern_bv

let add_pattern bv pat =
  pattern_bv := bv;
  add_pattern bv pat;
  !pattern_bv

let rec add_expr bv exp =
  match exp.pexp_desc with
    Pexp_ident l -> add bv l
  | Pexp_constant _ -> ()
  | Pexp_let(rf, pel, e) ->
      let bv = add_bindings rf bv pel in add_expr bv e
  | Pexp_function (_, opte, pel) ->
      add_opt add_expr bv opte; add_pat_expr_list bv pel
  | Pexp_apply(e, el) ->
      add_expr bv e; List.iter (fun (_,e) -> add_expr bv e) el
  | Pexp_match(e, pel) -> add_expr bv e; add_pat_expr_list bv pel
  | Pexp_try(e, pel) -> add_expr bv e; add_pat_expr_list bv pel
  | Pexp_tuple el -> List.iter (add_expr bv) el
  | Pexp_construct(c, opte, _) -> add bv c; add_opt add_expr bv opte
  | Pexp_variant(_, opte) -> add_opt add_expr bv opte
  | Pexp_record(lblel, opte) ->
      List.iter (fun (lbl, e) -> add bv lbl; add_expr bv e) lblel;
      add_opt add_expr bv opte
  | Pexp_field(e, fld) -> add_expr bv e; add bv fld
  | Pexp_setfield(e1, fld, e2) -> add_expr bv e1; add bv fld; add_expr bv e2
  | Pexp_array el -> List.iter (add_expr bv) el
  | Pexp_ifthenelse(e1, e2, opte3) ->
      add_expr bv e1; add_expr bv e2; add_opt add_expr bv opte3
  | Pexp_sequence(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_while(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_for( _, e1, e2, _, e3) ->
      add_expr bv e1; add_expr bv e2; add_expr bv e3
  | Pexp_constraint(e1, oty2, oty3) ->
      add_expr bv e1;
      add_opt add_type bv oty2;
      add_opt add_type bv oty3
  | Pexp_when(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_send(e, m) -> add_expr bv e
  | Pexp_new li -> add bv li
  | Pexp_setinstvar(v, e) -> add_expr bv e
  | Pexp_override sel -> List.iter (fun (s, e) -> add_expr bv e) sel
  | Pexp_letmodule(id, m, e) ->
      add_module bv m; add_expr (StringSet.add id.txt bv) e
  | Pexp_assert (e) -> add_expr bv e
  | Pexp_assertfalse -> ()
  | Pexp_lazy (e) -> add_expr bv e
  | Pexp_poly (e, t) -> add_expr bv e; add_opt add_type bv t
  | Pexp_object { pcstr_pat = pat; pcstr_fields = fieldl } ->
      let bv = add_pattern bv pat in List.iter (add_class_field bv) fieldl
(*> JOCAML *)
  | Pexp_spawn (e) -> add_expr bv e
  | Pexp_par (e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_reply (e,_) -> add_expr bv e
  | Pexp_def (d, e) ->
      List.iter (add_joinautomaton bv) d ; add_expr bv e
  | Pexp_newtype (_, e) -> add_expr bv e
  | Pexp_pack m -> add_module bv m
  | Pexp_open (m, e) -> addmodule bv m; add_expr bv e

and add_joinautomaton bv jauto =
  let cls = jauto.pjauto_desc in
  List.iter (add_joinclause bv) cls

and add_joinclause bv cl =
  let (_,e) = cl.pjclause_desc in
  add_expr bv e
(*< JOCAML *)

and add_pat_expr_list bv pel =
  List.iter (fun (p, e) -> let bv = add_pattern bv p in add_expr bv e) pel

and add_bindings recf bv pel =
  let bv' = List.fold_left (fun bv (p, _) -> add_pattern bv p) bv pel in
  let bv = if recf = Recursive then bv' else bv in
  List.iter (fun (_, e) -> add_expr bv e) pel;
  bv'

and add_modtype bv mty =
  match mty.pmty_desc with
    Pmty_ident l -> add bv l
  | Pmty_signature s -> add_signature bv s
  | Pmty_functor(id, mty1, mty2) ->
      add_modtype bv mty1; add_modtype (StringSet.add id.txt bv) mty2
  | Pmty_with(mty, cstrl) ->
      add_modtype bv mty;
      List.iter
        (function (_, Pwith_type td) -> add_type_declaration bv td
                | (_, Pwith_module (lid)) -> addmodule bv lid
                | (_, Pwith_typesubst td) -> add_type_declaration bv td
                | (_, Pwith_modsubst (lid)) -> addmodule bv lid)
        cstrl
  | Pmty_typeof m -> add_module bv m

and add_signature bv = function
    [] -> ()
  | item :: rem -> add_signature (add_sig_item bv item) rem

and add_sig_item bv item =
  match item.psig_desc with
    Psig_value(id, vd) ->
      add_type bv vd.pval_type; bv
  | Psig_type dcls ->
      List.iter (fun (id, td) -> add_type_declaration bv td) dcls; bv
  | Psig_exception(id, args) ->
      List.iter (add_type bv) args; bv
  | Psig_module(id, mty) ->
      add_modtype bv mty; StringSet.add id.txt bv
  | Psig_recmodule decls ->
      let bv' = List.fold_right StringSet.add (List.map (fun (x,_) -> x.txt) decls) bv in
      List.iter (fun (id, mty) -> add_modtype bv' mty) decls;
      bv'
  | Psig_modtype(id,mtyd) ->
      begin match mtyd with
        Pmodtype_abstract -> ()
      | Pmodtype_manifest mty -> add_modtype bv mty
      end;
      bv
  | Psig_open lid ->
      addmodule bv lid; bv
  | Psig_include mty ->
      add_modtype bv mty; bv
  | Psig_class cdl ->
      List.iter (add_class_description bv) cdl; bv
  | Psig_class_type cdtl ->
      List.iter (add_class_type_declaration bv) cdtl; bv

and add_module bv modl =
  match modl.pmod_desc with
    Pmod_ident l -> addmodule bv l
  | Pmod_structure s -> ignore (add_structure bv s)
  | Pmod_functor(id, mty, modl) ->
      add_modtype bv mty;
      add_module (StringSet.add id.txt bv) modl
  | Pmod_apply(mod1, mod2) ->
      add_module bv mod1; add_module bv mod2
  | Pmod_constraint(modl, mty) ->
      add_module bv modl; add_modtype bv mty
  | Pmod_unpack(e) ->
      add_expr bv e

and add_structure bv item_list =
  List.fold_left add_struct_item bv item_list

and add_struct_item bv item =
  match item.pstr_desc with
    Pstr_eval e ->
      add_expr bv e; bv
  | Pstr_value(rf, pel) ->
      let bv = add_bindings rf bv pel in bv
  | Pstr_primitive(id, vd) ->
      add_type bv vd.pval_type; bv
  | Pstr_type dcls ->
      List.iter (fun (id, td) -> add_type_declaration bv td) dcls; bv
  | Pstr_exception(id, args) ->
      List.iter (add_type bv) args; bv
  | Pstr_exn_rebind(id, l) ->
      add bv l; bv
  | Pstr_module(id, modl) ->
      add_module bv modl; StringSet.add id.txt bv
  | Pstr_recmodule bindings ->
      let bv' =
        List.fold_right StringSet.add
          (List.map (fun (id,_,_) -> id.txt) bindings) bv in
      List.iter
        (fun (id, mty, modl) -> add_modtype bv' mty; add_module bv' modl)
        bindings;
      bv'
  | Pstr_modtype(id, mty) ->
      add_modtype bv mty; bv
  | Pstr_open l ->
      addmodule bv l; bv
  | Pstr_class cdl ->
      List.iter (add_class_declaration bv) cdl; bv
  | Pstr_class_type cdtl ->
      List.iter (add_class_type_declaration bv) cdtl; bv
  | Pstr_include modl ->
      add_module bv modl; bv
(*> JOCAML *)
  | Pstr_def d ->
      List.iter (add_joinautomaton bv) d ; bv
  | Pstr_exn_global l ->
      add bv l; bv
(*< JOCAML *)

and add_use_file bv top_phrs =
  ignore (List.fold_left add_top_phrase bv top_phrs)

and add_top_phrase bv = function
  | Ptop_def str -> add_structure bv str
  | Ptop_dir (_, _) -> bv

and add_class_expr bv ce =
  match ce.pcl_desc with
    Pcl_constr(l, tyl) ->
      add bv l; List.iter (add_type bv) tyl
  | Pcl_structure { pcstr_pat = pat; pcstr_fields = fieldl } ->
      let bv = add_pattern bv pat in List.iter (add_class_field bv) fieldl
  | Pcl_fun(_, opte, pat, ce) ->
      add_opt add_expr bv opte;
      let bv = add_pattern bv pat in add_class_expr bv ce
  | Pcl_apply(ce, exprl) ->
      add_class_expr bv ce; List.iter (fun (_,e) -> add_expr bv e) exprl
  | Pcl_let(rf, pel, ce) ->
      let bv = add_bindings rf bv pel in add_class_expr bv ce
  | Pcl_constraint(ce, ct) ->
      add_class_expr bv ce; add_class_type bv ct

and add_class_field bv pcf =
  match pcf.pcf_desc with
    Pcf_inher(_, ce, _) -> add_class_expr bv ce
  | Pcf_val(_, _, _, e) -> add_expr bv e
  | Pcf_valvirt(_, _, ty)
  | Pcf_virt(_, _, ty) -> add_type bv ty
  | Pcf_meth(_, _, _, e) -> add_expr bv e
  | Pcf_constr(ty1, ty2) -> add_type bv ty1; add_type bv ty2
  | Pcf_init e -> add_expr bv e

and add_class_declaration bv decl =
  add_class_expr bv decl.pci_expr
