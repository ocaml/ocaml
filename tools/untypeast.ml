(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*    Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)     *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Typedtree
open Parsetree

(*
Some notes:

   * For Pexp_function, we cannot go back to the exact original version
   when there is a default argument, because the default argument is
   translated in the typer. The code, if printed, will not be parsable because
   new generated identifiers are not correct.

   * For Pexp_apply, it is unclear whether arguments are reordered, especially
    when there are optional arguments.

  * TODO: check Ttype_variant -> Ptype_variant (stub None)

*)


let rec lident_of_path path =
  match path with
      Path.Pident id -> Longident.Lident (Ident.name id)
    | Path.Pdot (p, s, _) -> Longident.Ldot (lident_of_path p, s)
    | Path.Papply (p1, p2) ->
        Longident.Lapply (lident_of_path p1, lident_of_path p2)

let rec untype_structure str =
  List.map untype_structure_item str.str_items

and untype_structure_item item =
  let desc =
    match item.str_desc with
      Tstr_eval exp -> Pstr_eval (untype_expression exp)
    | Tstr_value (rec_flag, list) ->
        Pstr_value (rec_flag, List.map (fun (pat, exp) ->
              untype_pattern pat, untype_expression exp) list)
    | Tstr_primitive (_id, name, v) ->
        Pstr_primitive (name, untype_value_description v)
    | Tstr_type list ->
        Pstr_type (List.map (fun (_id, name, decl) ->
              name, untype_type_declaration decl) list)
    | Tstr_exception (_id, name, decl) ->
        Pstr_exception (name, untype_exception_declaration decl)
    | Tstr_exn_rebind (_id, name, _p, lid) ->
        Pstr_exn_rebind (name, lid)
    | Tstr_module (_id, name, mexpr) ->
        Pstr_module (name, untype_module_expr mexpr)
    | Tstr_recmodule list ->
        Pstr_recmodule (List.map (fun (_id, name, mtype, mexpr) ->
              name, untype_module_type mtype,
              untype_module_expr mexpr) list)
    | Tstr_modtype (_id, name, mtype) ->
        Pstr_modtype (name, untype_module_type mtype)
    | Tstr_open (_path, lid) -> Pstr_open (lid, [])
    | Tstr_class list ->
        Pstr_class (List.map (fun (ci, _, _) ->
              { pci_virt = ci.ci_virt;
                pci_params = ci.ci_params;
                pci_name = ci.ci_id_name;
                pci_expr = untype_class_expr ci.ci_expr;
                pci_variance = ci.ci_variance;
                pci_loc = ci.ci_loc;
              }
          ) list)
    | Tstr_class_type list ->
        Pstr_class_type (List.map (fun (_id, _name, ct) ->
              {
                pci_virt = ct.ci_virt;
                pci_params = ct.ci_params;
                pci_name = ct.ci_id_name;
                pci_expr = untype_class_type ct.ci_expr;
                pci_variance = ct.ci_variance;
                pci_loc = ct.ci_loc;
              }
          ) list)
    | Tstr_include (mexpr, _) ->
        Pstr_include (untype_module_expr mexpr, [])
  in
  { pstr_desc = desc; pstr_loc = item.str_loc; }

and untype_value_description v =
  {
    pval_prim = v.val_prim;
    pval_type = untype_core_type v.val_desc;
    pval_loc = v.val_loc }

and untype_type_declaration decl =
  {
    ptype_params = decl.typ_params;
    ptype_cstrs = List.map (fun (ct1, ct2, loc) ->
        (untype_core_type ct1,
          untype_core_type ct2, loc)
    ) decl.typ_cstrs;
    ptype_kind = (match decl.typ_kind with
        Ttype_abstract -> Ptype_abstract
      | Ttype_variant list ->
          Ptype_variant (List.map (fun (_s, name, cts, loc) ->
                {pcd_name = name; pcd_args = List.map untype_core_type cts; pcd_res = None; pcd_loc = loc; pcd_attributes = []}) list)
      | Ttype_record list ->
          Ptype_record (List.map (fun (_s, name, mut, ct, loc) ->
                (name, mut, untype_core_type ct, loc)
            ) list)
    );
    ptype_private = decl.typ_private;
    ptype_manifest = (match decl.typ_manifest with
        None -> None
      | Some ct -> Some (untype_core_type ct));
    ptype_variance = decl.typ_variance;
    ptype_attributes = []; (* TODO *)
    ptype_loc = decl.typ_loc;
  }

and untype_exception_declaration decl =
  List.map untype_core_type decl.exn_params

and untype_pattern pat =
  let desc =
  match pat with
      { pat_extra=[Tpat_unpack, _]; pat_desc = Tpat_var (_,name); _ } -> Ppat_unpack name
    | { pat_extra=[Tpat_type (_path, lid), _]; _ } -> Ppat_type lid
    | { pat_extra= (Tpat_constraint ct, _) :: rem; _ } ->
        Ppat_constraint (untype_pattern { pat with pat_extra=rem }, untype_core_type ct)
    | _ ->
    match pat.pat_desc with
      Tpat_any -> Ppat_any
    | Tpat_var (id, name) ->
        begin
          match (Ident.name id).[0] with
            'A'..'Z' ->
              Ppat_unpack name
          | _ ->
              Ppat_var name
        end
    | Tpat_alias (pat, _id, name) ->
        Ppat_alias (untype_pattern pat, name)
    | Tpat_constant cst -> Ppat_constant cst
    | Tpat_tuple list ->
        Ppat_tuple (List.map untype_pattern list)
    | Tpat_construct (lid, _, args, explicit_arity) ->
        Ppat_construct (lid,
          (match args with
              [] -> None
            | args -> Some
                  { ppat_desc = Ppat_tuple (List.map untype_pattern args);
                  ppat_loc = pat.pat_loc; }
          ), explicit_arity)
    | Tpat_variant (label, pato, _) ->
        Ppat_variant (label, match pato with
            None -> None
          | Some pat -> Some (untype_pattern pat))
    | Tpat_record (list, closed) ->
        Ppat_record (List.map (fun (lid, _, pat) ->
              lid, untype_pattern pat) list, closed)
    | Tpat_array list -> Ppat_array (List.map untype_pattern list)
    | Tpat_or (p1, p2, _) -> Ppat_or (untype_pattern p1, untype_pattern p2)
    | Tpat_lazy p -> Ppat_lazy (untype_pattern p)
  in
  {
    ppat_desc = desc;
    ppat_loc = pat.pat_loc;
  }

and option f x = match x with None -> None | Some e -> Some (f e)

and untype_extra (extra, loc) sexp =
  let desc =
    match extra with
      Texp_constraint (cty1, cty2) ->
        Pexp_constraint (sexp,
                         option untype_core_type cty1,
                         option untype_core_type cty2)
    | Texp_open (_path, lid, _) -> Pexp_open (lid, sexp)
    | Texp_poly cto -> Pexp_poly (sexp, option untype_core_type cto)
    | Texp_newtype s -> Pexp_newtype (s, sexp)
  in
  { pexp_desc = desc;
    pexp_loc = loc }

and untype_expression exp =
  let desc =
    match exp.exp_desc with
      Texp_ident (_path, lid, _) -> Pexp_ident (lid)
    | Texp_constant cst -> Pexp_constant cst
    | Texp_let (rec_flag, list, exp) ->
        Pexp_let (rec_flag,
          List.map (fun (pat, exp) ->
              untype_pattern pat, untype_expression exp) list,
          untype_expression exp)
    | Texp_function (label, cases, _) ->
        Pexp_function (label, None,
          List.map (fun (pat, exp) ->
              (untype_pattern pat, untype_expression exp)) cases)
    | Texp_apply (exp, list) ->
        Pexp_apply (untype_expression exp,
          List.fold_right (fun (label, expo, _) list ->
              match expo with
                None -> list
              | Some exp -> (label, untype_expression exp) :: list
          ) list [])
    | Texp_match (exp, list, _) ->
        Pexp_match (untype_expression exp,
          List.map (fun (pat, exp) ->
              untype_pattern pat, untype_expression exp) list)
    | Texp_try (exp, list) ->
        Pexp_try (untype_expression exp,
          List.map (fun (pat, exp) ->
              untype_pattern pat, untype_expression exp) list)
    | Texp_tuple list ->
        Pexp_tuple (List.map untype_expression list)
    | Texp_construct (lid, _, args, explicit_arity) ->
        Pexp_construct (lid,
          (match args with
              [] -> None
          | [ arg ] -> Some (untype_expression arg)
          | args -> Some
            { pexp_desc = Pexp_tuple (List.map untype_expression args);
              pexp_loc = exp.exp_loc; }
          ), explicit_arity)
    | Texp_variant (label, expo) ->
        Pexp_variant (label, match expo with
            None -> None
          | Some exp -> Some (untype_expression exp))
    | Texp_record (list, expo) ->
        Pexp_record (List.map (fun (lid, _, exp) ->
              lid, untype_expression exp
          ) list,
          match expo with
            None -> None
          | Some exp -> Some (untype_expression exp))
    | Texp_field (exp, lid, _label) ->
        Pexp_field (untype_expression exp, lid)
    | Texp_setfield (exp1, lid, _label, exp2) ->
        Pexp_setfield (untype_expression exp1, lid,
          untype_expression exp2)
    | Texp_array list ->
        Pexp_array (List.map untype_expression list)
    | Texp_ifthenelse (exp1, exp2, expo) ->
        Pexp_ifthenelse (untype_expression exp1,
          untype_expression exp2,
          match expo with
            None -> None
          | Some exp -> Some (untype_expression exp))
    | Texp_sequence (exp1, exp2) ->
        Pexp_sequence (untype_expression exp1, untype_expression exp2)
    | Texp_while (exp1, exp2) ->
        Pexp_while (untype_expression exp1, untype_expression exp2)
    | Texp_for (_id, name, exp1, exp2, dir, exp3) ->
        Pexp_for (name,
          untype_expression exp1, untype_expression exp2,
          dir, untype_expression exp3)
    | Texp_when (exp1, exp2) ->
        Pexp_when (untype_expression exp1, untype_expression exp2)
    | Texp_send (exp, meth, _) ->
        Pexp_send (untype_expression exp, match meth with
            Tmeth_name name -> name
          | Tmeth_val id -> Ident.name id)
    | Texp_new (_path, lid, _) -> Pexp_new (lid)
    | Texp_instvar (_, path, name) ->
      Pexp_ident ({name with txt = lident_of_path path})
    | Texp_setinstvar (_, _path, lid, exp) ->
        Pexp_setinstvar (lid, untype_expression exp)
    | Texp_override (_, list) ->
        Pexp_override (List.map (fun (_path, lid, exp) ->
              lid, untype_expression exp
          ) list)
    | Texp_letmodule (_id, name, mexpr, exp) ->
        Pexp_letmodule (name, untype_module_expr mexpr,
          untype_expression exp)
    | Texp_assert exp -> Pexp_assert (untype_expression exp)
    | Texp_assertfalse -> Pexp_assertfalse
    | Texp_lazy exp -> Pexp_lazy (untype_expression exp)
    | Texp_object (cl, _) ->
        Pexp_object (untype_class_structure cl)
    | Texp_pack (mexpr) ->
        Pexp_pack (untype_module_expr mexpr)
  in
  List.fold_right untype_extra exp.exp_extra
    { pexp_loc = exp.exp_loc;
      pexp_desc = desc }

and untype_package_type pack =
  (pack.pack_txt,
    List.map (fun (s, ct) ->
        (s, untype_core_type ct)) pack.pack_fields)

and untype_signature sg =
  List.map untype_signature_item sg.sig_items

and untype_signature_item item =
  let desc =
    match item.sig_desc with
      Tsig_value (_id, name, v) ->
        Psig_value (name, untype_value_description v)
    | Tsig_type list ->
        Psig_type (List.map (fun (_id, name, decl) ->
              name, untype_type_declaration decl
          ) list)
    | Tsig_exception (_id, name, decl) ->
        Psig_exception (name, untype_exception_declaration decl)
    | Tsig_module (_id, name, mtype) ->
        Psig_module (name, untype_module_type mtype)
    | Tsig_recmodule list ->
        Psig_recmodule (List.map (fun (_id, name, mtype) ->
              name, untype_module_type mtype) list)
    | Tsig_modtype (_id, name, mdecl) ->
        Psig_modtype (name, untype_modtype_declaration mdecl)
    | Tsig_open (_path, lid) -> Psig_open (lid)
    | Tsig_include (mty, _lid) -> Psig_include (untype_module_type mty)
    | Tsig_class list ->
        Psig_class (List.map untype_class_description list)
    | Tsig_class_type list ->
        Psig_class_type (List.map untype_class_type_declaration list)
  in
  { psig_desc = desc;
    psig_loc = item.sig_loc;
  }

and untype_modtype_declaration mdecl =
  match mdecl with
    Tmodtype_abstract -> Pmodtype_abstract
  | Tmodtype_manifest mtype -> Pmodtype_manifest (untype_module_type mtype)

and untype_class_description cd =
  {
    pci_virt = cd.ci_virt;
    pci_params = cd.ci_params;
    pci_name = cd.ci_id_name;
    pci_expr = untype_class_type cd.ci_expr;
    pci_variance = cd.ci_variance;
    pci_loc = cd.ci_loc;
  }

and untype_class_type_declaration cd =
  {
    pci_virt = cd.ci_virt;
    pci_params = cd.ci_params;
    pci_name = cd.ci_id_name;
    pci_expr = untype_class_type cd.ci_expr;
    pci_variance = cd.ci_variance;
    pci_loc = cd.ci_loc;
  }

and untype_module_type mty =
  let desc = match mty.mty_desc with
      Tmty_ident (_path, lid) -> Pmty_ident (lid)
    | Tmty_signature sg -> Pmty_signature (untype_signature sg)
    | Tmty_functor (_id, name, mtype1, mtype2) ->
        Pmty_functor (name, untype_module_type mtype1,
          untype_module_type mtype2)
    | Tmty_with (mtype, list) ->
        Pmty_with (untype_module_type mtype,
          List.map (fun (_path, lid, withc) ->
              lid, untype_with_constraint withc
          ) list)
    | Tmty_typeof mexpr ->
        Pmty_typeof (untype_module_expr mexpr)
  in
  {
    pmty_desc = desc;
    pmty_loc = mty.mty_loc;
  }

and untype_with_constraint cstr =
  match cstr with
    Twith_type decl -> Pwith_type (untype_type_declaration decl)
  | Twith_module (_path, lid) -> Pwith_module (lid)
  | Twith_typesubst decl -> Pwith_typesubst (untype_type_declaration decl)
  | Twith_modsubst (_path, lid) -> Pwith_modsubst (lid)

and untype_module_expr mexpr =
  match mexpr.mod_desc with
    Tmod_constraint (m, _, Tmodtype_implicit, _ ) ->
      untype_module_expr m
  | _ ->
      let desc = match mexpr.mod_desc with
          Tmod_ident (_p, lid) -> Pmod_ident (lid)
        | Tmod_structure st -> Pmod_structure (untype_structure st)
        | Tmod_functor (_id, name, mtype, mexpr) ->
            Pmod_functor (name, untype_module_type mtype,
              untype_module_expr mexpr)
        | Tmod_apply (mexp1, mexp2, _) ->
            Pmod_apply (untype_module_expr mexp1, untype_module_expr mexp2)
        | Tmod_constraint (mexpr, _, Tmodtype_explicit mtype, _) ->
            Pmod_constraint (untype_module_expr mexpr,
              untype_module_type mtype)
        | Tmod_constraint (_mexpr, _, Tmodtype_implicit, _) ->
            assert false
        | Tmod_unpack (exp, _pack) ->
        Pmod_unpack (untype_expression exp)
        (* TODO , untype_package_type pack) *)

  in
  {
    pmod_desc = desc;
    pmod_loc = mexpr.mod_loc;
  }

and untype_class_expr cexpr =
  let desc = match cexpr.cl_desc with
    | Tcl_constraint ( { cl_desc = Tcl_ident (_path, lid, tyl); _ }, None, _, _, _ ) ->
        Pcl_constr (lid,
          List.map untype_core_type tyl)
    | Tcl_structure clstr -> Pcl_structure (untype_class_structure clstr)

    | Tcl_fun (label, pat, _pv, cl, _partial) ->
        Pcl_fun (label, None, untype_pattern pat, untype_class_expr cl)

    | Tcl_apply (cl, args) ->
        Pcl_apply (untype_class_expr cl,
          List.fold_right (fun (label, expo, _) list ->
              match expo with
                None -> list
              | Some exp -> (label, untype_expression exp) :: list
          ) args [])

    | Tcl_let (rec_flat, bindings, _ivars, cl) ->
        Pcl_let (rec_flat,
          List.map (fun (pat, exp) ->
              (untype_pattern pat, untype_expression exp)) bindings,
          untype_class_expr cl)

    | Tcl_constraint (cl, Some clty, _vals, _meths, _concrs) ->
        Pcl_constraint (untype_class_expr cl,  untype_class_type clty)

    | Tcl_ident _ -> assert false
    | Tcl_constraint (_, None, _, _, _) -> assert false
  in
  { pcl_desc = desc;
    pcl_loc = cexpr.cl_loc;
  }

and untype_class_type ct =
  let desc = match ct.cltyp_desc with
      Tcty_signature csg -> Pcty_signature (untype_class_signature csg)
    | Tcty_constr (_path, lid, list) ->
        Pcty_constr (lid, List.map untype_core_type list)
    | Tcty_fun (label, ct, cl) ->
        Pcty_fun (label, untype_core_type ct, untype_class_type cl)
  in
  { pcty_desc = desc;
    pcty_loc = ct.cltyp_loc }

and untype_class_signature cs =
  {
    pcsig_self = untype_core_type cs.csig_self;
    pcsig_fields = List.map untype_class_type_field cs.csig_fields;
    pcsig_loc = cs.csig_loc;
  }

and untype_class_type_field ctf =
  let desc = match ctf.ctf_desc with
      Tctf_inher ct -> Pctf_inher (untype_class_type ct)
    | Tctf_val (s, mut, virt, ct) ->
        Pctf_val (s, mut, virt, untype_core_type ct)
    | Tctf_virt  (s, priv, ct) ->
        Pctf_virt (s, priv, untype_core_type ct)
    | Tctf_meth  (s, priv, ct) ->
        Pctf_meth  (s, priv, untype_core_type ct)
    | Tctf_cstr  (ct1, ct2) ->
        Pctf_cstr (untype_core_type ct1, untype_core_type ct2)
  in
  {
    pctf_desc = desc;
    pctf_loc = ctf.ctf_loc;
  }

and untype_core_type ct =
  let desc = match ct.ctyp_desc with
      Ttyp_any -> Ptyp_any
    | Ttyp_var s -> Ptyp_var s
    | Ttyp_arrow (label, ct1, ct2) ->
        Ptyp_arrow (label, untype_core_type ct1, untype_core_type ct2)
  | Ttyp_tuple list -> Ptyp_tuple (List.map untype_core_type list)
    | Ttyp_constr (_path, lid, list) ->
        Ptyp_constr (lid,
          List.map untype_core_type list)
    | Ttyp_object list ->
        Ptyp_object (List.map untype_core_field_type list)
    | Ttyp_class (_path, lid, list, labels) ->
        Ptyp_class (lid,
          List.map untype_core_type list, labels)
    | Ttyp_alias (ct, s) ->
        Ptyp_alias (untype_core_type ct, s)
    | Ttyp_variant (list, bool, labels) ->
        Ptyp_variant (List.map untype_row_field list, bool, labels)
    | Ttyp_poly (list, ct) -> Ptyp_poly (list, untype_core_type ct)
    | Ttyp_package pack -> Ptyp_package (untype_package_type pack)
  in
  { ptyp_desc = desc; ptyp_loc = ct.ctyp_loc }

and untype_core_field_type cft =
  { pfield_desc = (match cft.field_desc with
        Tcfield_var -> Pfield_var
      | Tcfield (s, ct) -> Pfield (s, untype_core_type ct));
    pfield_loc = cft.field_loc; }

and untype_class_structure cs =
  { pcstr_pat = untype_pattern cs.cstr_pat;
    pcstr_fields = List.map untype_class_field cs.cstr_fields;
  }

and untype_row_field rf =
  match rf with
    Ttag (label, bool, list) ->
      Rtag (label, bool, List.map untype_core_type list)
  | Tinherit ct -> Rinherit (untype_core_type ct)

and untype_class_field cf =
  let desc = match cf.cf_desc with
      Tcf_inher (ovf, cl, super, _vals, _meths) ->
        Pcf_inher (ovf, untype_class_expr cl, super)
    | Tcf_constr (cty, cty') ->
        Pcf_constr (untype_core_type cty, untype_core_type cty')
    | Tcf_val (_lab, name, mut, _, Tcfk_virtual cty, _override) ->
        Pcf_valvirt (name, mut, untype_core_type cty)
    | Tcf_val (_lab, name, mut, _, Tcfk_concrete exp, override) ->
        Pcf_val (name, mut,
          (if override then Override else Fresh),
          untype_expression exp)
    | Tcf_meth (_lab, name, priv, Tcfk_virtual cty, _override) ->
        Pcf_virt (name, priv, untype_core_type cty)
    | Tcf_meth (_lab, name, priv, Tcfk_concrete exp, override) ->
        Pcf_meth (name, priv,
          (if override then Override else Fresh),
          untype_expression exp)
(*    | Tcf_let (rec_flag, bindings, _) ->
        Pcf_let (rec_flag, List.map (fun (pat, exp) ->
              untype_pattern pat, untype_expression exp) bindings)
*)
  | Tcf_init exp -> Pcf_init (untype_expression exp)
  in
  { pcf_desc = desc; pcf_loc = cf.cf_loc }
