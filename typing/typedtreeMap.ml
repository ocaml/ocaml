(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Fabrice Le Fessant, INRIA Saclay                   *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Typedtree

module type MapArgument = sig
  val enter_structure : structure -> structure
  val enter_value_description : value_description -> value_description
  val enter_type_declaration : type_declaration -> type_declaration
  val enter_type_extension : type_extension -> type_extension
  val enter_extension_constructor :
    extension_constructor -> extension_constructor
  val enter_exception_declaration :
    exception_declaration -> exception_declaration
  val enter_pattern : pattern -> pattern
  val enter_expression : expression -> expression
  val enter_package_type : package_type -> package_type
  val enter_signature : signature -> signature
  val enter_signature_item : signature_item -> signature_item
  val enter_modtype_declaration : modtype_declaration -> modtype_declaration
  val enter_module_type : module_type -> module_type
  val enter_module_expr : module_expr -> module_expr
  val enter_with_constraint : with_constraint -> with_constraint
  val enter_class_expr : class_expr -> class_expr
  val enter_class_signature : class_signature -> class_signature
  val enter_class_description : class_description -> class_description
  val enter_class_type_declaration :
    class_type_declaration -> class_type_declaration
  val enter_class_infos : 'a class_infos -> 'a class_infos
  val enter_class_type : class_type -> class_type
  val enter_class_type_field : class_type_field -> class_type_field
  val enter_core_type : core_type -> core_type
  val enter_core_field_type : core_field_type -> core_field_type
  val enter_class_structure : class_structure -> class_structure
  val enter_class_field : class_field -> class_field
  val enter_structure_item : structure_item -> structure_item

  val leave_structure : structure -> structure
  val leave_value_description : value_description -> value_description
  val leave_type_declaration : type_declaration -> type_declaration
  val leave_type_extension : type_extension -> type_extension
  val leave_extension_constructor :
    extension_constructor -> extension_constructor
  val leave_exception_declaration :
    exception_declaration -> exception_declaration
  val leave_pattern : pattern -> pattern
  val leave_expression : expression -> expression
  val leave_package_type : package_type -> package_type
  val leave_signature : signature -> signature
  val leave_signature_item : signature_item -> signature_item
  val leave_modtype_declaration : modtype_declaration -> modtype_declaration
  val leave_module_type : module_type -> module_type
  val leave_module_expr : module_expr -> module_expr
  val leave_with_constraint : with_constraint -> with_constraint
  val leave_class_expr : class_expr -> class_expr
  val leave_class_signature : class_signature -> class_signature
  val leave_class_description : class_description -> class_description
  val leave_class_type_declaration :
    class_type_declaration -> class_type_declaration
  val leave_class_infos : 'a class_infos -> 'a class_infos
  val leave_class_type : class_type -> class_type
  val leave_class_type_field : class_type_field -> class_type_field
  val leave_core_type : core_type -> core_type
  val leave_core_field_type : core_field_type -> core_field_type
  val leave_class_structure : class_structure -> class_structure
  val leave_class_field : class_field -> class_field
  val leave_structure_item : structure_item -> structure_item

end


module MakeMap(Map : MapArgument) = struct

  let may_map f v =
    match v with
        None -> v
      | Some x -> Some (f x)


  open Misc
  open Asttypes

  let rec map_structure str =
    let str = Map.enter_structure str in
    let str_items = List.map map_structure_item str.str_items in
    Map.leave_structure { str with str_items = str_items }

  and map_binding (pat, exp) = (map_pattern pat, map_expression exp)

  and map_bindings rec_flag list =
    List.map map_binding list

  and map_structure_item item =
    let item = Map.enter_structure_item item in
    let str_desc =
      match item.str_desc with
          Tstr_eval exp -> Tstr_eval (map_expression exp)
        | Tstr_value (rec_flag, list) ->
          Tstr_value (rec_flag, map_bindings rec_flag list)
        | Tstr_primitive (id, name, v) ->
          Tstr_primitive (id, name, map_value_description v)
        | Tstr_type list ->
          Tstr_type (List.map (
            fun (id, name, decl) ->
              (id, name, map_type_declaration decl) ) list)
        | Tstr_extension tyext ->
          Tstr_extension (map_type_extension tyext)
        | Tstr_exception (id, name, decl) ->
          Tstr_exception (id, name, map_exception_declaration decl)
        | Tstr_exn_rebind (id, name, path, lid) ->
          Tstr_exn_rebind (id, name, path, lid)
        | Tstr_module (id, name, mexpr) ->
          Tstr_module (id, name, map_module_expr mexpr)
        | Tstr_recmodule list ->
          let list =
            List.map (fun (id, name, mtype, mexpr) ->
              (id, name, map_module_type mtype, map_module_expr mexpr)
            ) list
          in
          Tstr_recmodule list
        | Tstr_modtype (id, name, mtype) ->
          Tstr_modtype (id, name, map_module_type mtype)
        | Tstr_open (ovf, path, lid) -> Tstr_open (ovf, path, lid)
        | Tstr_class list ->
          let list =
            List.map (fun (ci, string_list, virtual_flag) ->
              let ci = Map.enter_class_infos ci in
              let ci_params = List.map map_core_type ci.ci_params in
              let ci_expr = map_class_expr ci.ci_expr in
              (Map.leave_class_infos
                 { ci with ci_params = ci_params; ci_expr = ci_expr},
               string_list, virtual_flag)
            ) list
          in
          Tstr_class list
        | Tstr_class_type list ->
          let list = List.map (fun (id, name, ct) ->
            let ct = Map.enter_class_infos ct in
            let ci_params = List.map map_core_type ct.ci_params in
            let ci_expr = map_class_type ct.ci_expr in
            (id, name, Map.leave_class_infos
                         { ct with ci_params = ci_params; ci_expr = ci_expr})
          ) list in
          Tstr_class_type list
        | Tstr_include (mexpr, sg) ->
          Tstr_include (map_module_expr mexpr, sg)
    in
    Map.leave_structure_item { item with str_desc = str_desc}

  and map_value_description v =
    let v = Map.enter_value_description v in
    let val_desc = map_core_type v.val_desc in
    Map.leave_value_description { v with val_desc = val_desc }

  and map_type_declaration decl =
    let decl = Map.enter_type_declaration decl in
    let typ_params = List.map map_core_type decl.typ_params in
    let typ_cstrs = List.map (fun (ct1, ct2, loc) ->
      (map_core_type ct1,
       map_core_type ct2,
       loc)
    ) decl.typ_cstrs in
    let typ_kind = match decl.typ_kind with
        Ttype_abstract -> Ttype_abstract
      | Ttype_variant list ->
        let list = List.map (fun (s, name, cts, loc) ->
          (s, name, List.map map_core_type cts, loc)
        ) list in
        Ttype_variant list
      | Ttype_record list ->
        let list =
          List.map (fun (s, name, mut, ct, loc) ->
            (s, name, mut, map_core_type ct, loc)
          ) list in
        Ttype_record list
      | Ttype_open -> Ttype_open
    in
    let typ_manifest = may_map map_core_type decl.typ_manifest in
    Map.leave_type_declaration { decl with typ_params = typ_params;
      typ_cstrs = typ_cstrs; typ_kind = typ_kind; typ_manifest = typ_manifest }

    and map_type_extension tyext =
      let tyext = Map.enter_type_extension tyext in
      let tyext_params = List.map map_core_type tyext.tyext_params in
      let tyext_constructors  =
        List.map map_extension_constructor tyext.tyext_constructors
      in
      Map.leave_type_extension { tyext with tyext_params = tyext_params;
        tyext_constructors = tyext_constructors }

    and map_extension_constructor ext =
      let ext = Map.enter_extension_constructor ext in
      let ext_kind = match ext.ext_kind with
          Text_decl(args, ret) ->
            let args = List.map map_core_type args in
            let ret = may_map map_core_type ret in
              Text_decl(args, ret)
        | Text_rebind(p, lid) -> Text_rebind(p, lid)
      in
      Map.leave_extension_constructor {ext with ext_kind = ext_kind}

  and map_exception_declaration decl =
    let decl = Map.enter_exception_declaration decl in
    let exn_args = List.map map_core_type decl.exn_args in
    let decl =       { exn_args = exn_args;
                       exn_exn = decl.exn_exn;
                       exn_loc = decl.exn_loc } in
    Map.leave_exception_declaration decl;

  and map_pattern pat =
    let pat = Map.enter_pattern pat in
    let pat_desc =
      match pat.pat_desc with
        | Tpat_alias (pat1, p, text) ->
          let pat1 = map_pattern pat1 in
          Tpat_alias (pat1, p, text)
        | Tpat_tuple list -> Tpat_tuple (List.map map_pattern list)
        | Tpat_construct (lid, cstr_decl, args, arity) ->
          Tpat_construct (lid, cstr_decl,
                          List.map map_pattern args, arity)
        | Tpat_variant (label, pato, rowo) ->
          let pato = match pato with
              None -> pato
            | Some pat -> Some (map_pattern pat)
          in
          Tpat_variant (label, pato, rowo)
        | Tpat_record (list, closed) ->
          Tpat_record (List.map (fun (lid, lab_desc, pat) ->
            (lid, lab_desc, map_pattern pat) ) list, closed)
        | Tpat_array list -> Tpat_array (List.map map_pattern list)
        | Tpat_or (p1, p2, rowo) ->
          Tpat_or (map_pattern p1, map_pattern p2, rowo)
        | Tpat_lazy p -> Tpat_lazy (map_pattern p)
        | Tpat_constant _
        | Tpat_any
        | Tpat_var _ -> pat.pat_desc

    in
    let pat_extra = List.map map_pat_extra pat.pat_extra in
    Map.leave_pattern { pat with pat_desc = pat_desc; pat_extra = pat_extra }

  and map_pat_extra pat_extra =
    match pat_extra with
      | Tpat_constraint ct, loc -> (Tpat_constraint (map_core_type  ct), loc)
      | (Tpat_type _ | Tpat_unpack), _ -> pat_extra

  and map_expression exp =
    let exp = Map.enter_expression exp in
    let exp_desc =
      match exp.exp_desc with
          Texp_ident (_, _, _)
        | Texp_constant _ -> exp.exp_desc
        | Texp_let (rec_flag, list, exp) ->
          Texp_let (rec_flag,
                    map_bindings rec_flag list,
                    map_expression exp)
        | Texp_function (label, cases, partial) ->
          Texp_function (label, map_bindings Nonrecursive cases, partial)
        | Texp_apply (exp, list) ->
          Texp_apply (map_expression exp,
                      List.map (fun (label, expo, optional) ->
                        let expo =
                          match expo with
                              None -> expo
                            | Some exp -> Some (map_expression exp)
                        in
                        (label, expo, optional)
                      ) list )
        | Texp_match (exp, list, partial) ->
          Texp_match (
            map_expression exp,
            map_bindings Nonrecursive list,
            partial
          )
        | Texp_try (exp, list) ->
          Texp_try (
            map_expression exp,
            map_bindings Nonrecursive list
          )
        | Texp_tuple list ->
          Texp_tuple (List.map map_expression list)
        | Texp_construct (lid, cstr_desc, args, arity) ->
          Texp_construct (lid, cstr_desc,
                          List.map map_expression args, arity )
        | Texp_variant (label, expo) ->
          let expo =match expo with
              None -> expo
            | Some exp -> Some (map_expression exp)
          in
          Texp_variant (label, expo)
        | Texp_record (list, expo) ->
          let list =
            List.map (fun (lid, lab_desc, exp) ->
              (lid, lab_desc, map_expression exp)
            ) list in
          let expo = match expo with
              None -> expo
            | Some exp -> Some (map_expression exp)
          in
          Texp_record (list, expo)
        | Texp_field (exp, lid, label) ->
          Texp_field (map_expression exp, lid, label)
        | Texp_setfield (exp1, lid, label, exp2) ->
          Texp_setfield (
            map_expression exp1,
            lid,
            label,
            map_expression exp2)
        | Texp_array list ->
          Texp_array (List.map map_expression list)
        | Texp_ifthenelse (exp1, exp2, expo) ->
          Texp_ifthenelse (
            map_expression exp1,
            map_expression exp2,
            match expo with
                None -> expo
              | Some exp -> Some (map_expression exp)
          )
        | Texp_sequence (exp1, exp2) ->
          Texp_sequence (
            map_expression exp1,
            map_expression exp2
          )
        | Texp_while (exp1, exp2) ->
          Texp_while (
            map_expression exp1,
            map_expression exp2
          )
        | Texp_for (id, name, exp1, exp2, dir, exp3) ->
          Texp_for (
            id, name,
            map_expression exp1,
            map_expression exp2,
            dir,
            map_expression exp3
          )
        | Texp_when (exp1, exp2) ->
          Texp_when (
            map_expression exp1,
            map_expression exp2
          )
        | Texp_send (exp, meth, expo) ->
          Texp_send (map_expression exp, meth, may_map map_expression expo)
        | Texp_new (path, lid, cl_decl) -> exp.exp_desc
        | Texp_instvar (_, path, _) -> exp.exp_desc
        | Texp_setinstvar (path, lid, path2, exp) ->
          Texp_setinstvar (path, lid, path2, map_expression exp)
        | Texp_override (path, list) ->
          Texp_override (
            path,
            List.map (fun (path, lid, exp) ->
              (path, lid, map_expression exp)
            ) list
          )
        | Texp_letmodule (id, name, mexpr, exp) ->
          Texp_letmodule (
            id, name,
            map_module_expr mexpr,
            map_expression exp
          )
        | Texp_assert exp -> Texp_assert (map_expression exp)
        | Texp_assertfalse -> exp.exp_desc
        | Texp_lazy exp -> Texp_lazy (map_expression exp)
        | Texp_object (cl, string_list) ->
          Texp_object (map_class_structure cl, string_list)
        | Texp_pack (mexpr) ->
          Texp_pack (map_module_expr mexpr)
    in
    let exp_extra = List.map map_exp_extra exp.exp_extra in
    Map.leave_expression {
      exp with
        exp_desc = exp_desc;
        exp_extra = exp_extra }

  and map_exp_extra exp_extra =
    let loc = snd exp_extra in
    match fst exp_extra with
      | Texp_constraint (Some ct, None) ->
        Texp_constraint (Some (map_core_type ct), None), loc
      | Texp_constraint (None, Some ct) ->
        Texp_constraint (None, Some (map_core_type ct)), loc
      | Texp_constraint (Some ct1, Some ct2) ->
        Texp_constraint (Some (map_core_type ct1),
                         Some (map_core_type ct2)), loc
      | Texp_poly (Some ct) ->
        Texp_poly (Some ( map_core_type ct )), loc
      | Texp_newtype _
      | Texp_constraint (None, None)
      | Texp_open _
      | Texp_poly None -> exp_extra


  and map_package_type pack =
    let pack = Map.enter_package_type pack in
    let pack_fields = List.map (
      fun (s, ct) -> (s, map_core_type ct) ) pack.pack_fields in
    Map.leave_package_type { pack with pack_fields = pack_fields }

  and map_signature sg =
    let sg = Map.enter_signature sg in
    let sig_items = List.map map_signature_item sg.sig_items in
    Map.leave_signature { sg with sig_items = sig_items }

  and map_signature_item item =
    let item = Map.enter_signature_item item in
    let sig_desc =
      match item.sig_desc with
          Tsig_value (id, name, v) ->
            Tsig_value (id, name, map_value_description v)
        | Tsig_type list -> Tsig_type (
          List.map (fun (id, name, decl) ->
            (id, name, map_type_declaration decl)
          ) list
        )
        | Tsig_extension tyext ->
          Tsig_extension (map_type_extension tyext)
        | Tsig_exception (id, name, decl) ->
          Tsig_exception (id, name, map_exception_declaration decl)
        | Tsig_module (id, name, mtype) ->
          Tsig_module (id, name, map_module_type mtype)
        | Tsig_recmodule list ->
          Tsig_recmodule (List.map (
            fun (id, name, mtype) ->
              (id, name, map_module_type mtype) ) list)
        | Tsig_modtype (id, name, mdecl) ->
          Tsig_modtype (id, name, map_modtype_declaration mdecl)
        | Tsig_open _ -> item.sig_desc
        | Tsig_include (mty, sg) -> Tsig_include (map_module_type mty, sg)
        | Tsig_class list -> Tsig_class (List.map map_class_description list)
        | Tsig_class_type list ->
          Tsig_class_type (List.map map_class_type_declaration list)
    in
    Map.leave_signature_item { item with sig_desc = sig_desc }

  and map_modtype_declaration mdecl =
    let mdecl = Map.enter_modtype_declaration mdecl in
    let mdecl =
      match mdecl with
          Tmodtype_abstract -> Tmodtype_abstract
        | Tmodtype_manifest mtype ->
          Tmodtype_manifest (map_module_type mtype)
    in
    Map.leave_modtype_declaration mdecl

  and map_class_description cd =
    let cd = Map.enter_class_description cd in
    let ci_params = List.map map_core_type cd.ci_params in
    let ci_expr = map_class_type cd.ci_expr in
    Map.leave_class_description
      { cd with ci_params = ci_params; ci_expr = ci_expr}

  and map_class_type_declaration cd =
    let cd = Map.enter_class_type_declaration cd in
    let ci_params = List.map map_core_type cd.ci_params in
    let ci_expr = map_class_type cd.ci_expr in
    Map.leave_class_type_declaration
      { cd with ci_params = ci_params; ci_expr = ci_expr }

  and map_module_type mty =
    let mty = Map.enter_module_type mty in
    let mty_desc =
      match mty.mty_desc with
          Tmty_ident (path, lid) -> mty.mty_desc
        | Tmty_signature sg -> Tmty_signature (map_signature sg)
        | Tmty_functor (id, name, mtype1, mtype2) ->
          Tmty_functor (id, name, map_module_type mtype1,
                        map_module_type mtype2)
        | Tmty_with (mtype, list) ->
          Tmty_with (map_module_type mtype,
                     List.map (fun (path, lid, withc) ->
                       (path, lid, map_with_constraint withc)
                     ) list)
        | Tmty_typeof mexpr ->
          Tmty_typeof (map_module_expr mexpr)
    in
    Map.leave_module_type { mty with mty_desc = mty_desc}

  and map_with_constraint cstr =
    let cstr = Map.enter_with_constraint cstr in
    let cstr =
      match cstr with
          Twith_type decl -> Twith_type (map_type_declaration decl)
        | Twith_typesubst decl -> Twith_typesubst (map_type_declaration decl)
        | Twith_module (path, lid) -> cstr
        | Twith_modsubst (path, lid) -> cstr
    in
    Map.leave_with_constraint cstr

  and map_module_expr mexpr =
    let mexpr = Map.enter_module_expr mexpr in
    let mod_desc =
      match mexpr.mod_desc with
          Tmod_ident (p, lid) -> mexpr.mod_desc
        | Tmod_structure st -> Tmod_structure (map_structure st)
        | Tmod_functor (id, name, mtype, mexpr) ->
          Tmod_functor (id, name, map_module_type mtype,
                        map_module_expr mexpr)
        | Tmod_apply (mexp1, mexp2, coercion) ->
          Tmod_apply (map_module_expr mexp1, map_module_expr mexp2, coercion)
        | Tmod_constraint (mexpr, mod_type, Tmodtype_implicit, coercion ) ->
          Tmod_constraint (map_module_expr mexpr, mod_type,
                           Tmodtype_implicit, coercion)
        | Tmod_constraint (mexpr, mod_type,
                           Tmodtype_explicit mtype, coercion) ->
          Tmod_constraint (map_module_expr mexpr, mod_type,
                           Tmodtype_explicit (map_module_type mtype),
                           coercion)
        | Tmod_unpack (exp, mod_type) ->
          Tmod_unpack (map_expression exp, mod_type)
    in
    Map.leave_module_expr { mexpr with mod_desc = mod_desc }

  and map_class_expr cexpr =
    let cexpr = Map.enter_class_expr cexpr in
    let cl_desc =
      match cexpr.cl_desc with
        | Tcl_constraint (cl, None, string_list1, string_list2, concr ) ->
          Tcl_constraint (map_class_expr cl, None, string_list1,
                          string_list2, concr)
        | Tcl_structure clstr -> Tcl_structure (map_class_structure clstr)
        | Tcl_fun (label, pat, priv, cl, partial) ->
          Tcl_fun (label, map_pattern pat,
                   List.map (fun (id, name, exp) ->
                     (id, name, map_expression exp)) priv,
                   map_class_expr cl, partial)

        | Tcl_apply (cl, args) ->
          Tcl_apply (map_class_expr cl,
                     List.map (fun (label, expo, optional) ->
                       (label, may_map map_expression expo,
                        optional)
                     ) args)
        | Tcl_let (rec_flat, bindings, ivars, cl) ->
          Tcl_let (rec_flat, map_bindings rec_flat bindings,
                   List.map (fun (id, name, exp) ->
                     (id, name, map_expression exp)) ivars,
                   map_class_expr cl)

        | Tcl_constraint (cl, Some clty, vals, meths, concrs) ->
          Tcl_constraint ( map_class_expr cl,
                           Some (map_class_type clty), vals, meths, concrs)

        | Tcl_ident (id, name, tyl) ->
          Tcl_ident (id, name, List.map map_core_type tyl)
    in
    Map.leave_class_expr { cexpr with cl_desc = cl_desc }

  and map_class_type ct =
    let ct = Map.enter_class_type ct in
    let cltyp_desc =
      match ct.cltyp_desc with
          Tcty_signature csg -> Tcty_signature (map_class_signature csg)
        | Tcty_constr (path, lid, list) ->
          Tcty_constr (path, lid, List.map map_core_type list)
        | Tcty_fun (label, ct, cl) ->
          Tcty_fun (label, map_core_type ct, map_class_type cl)
    in
    Map.leave_class_type { ct with cltyp_desc = cltyp_desc }

  and map_class_signature cs =
    let cs = Map.enter_class_signature cs in
    let csig_self = map_core_type cs.csig_self in
    let csig_fields = List.map map_class_type_field cs.csig_fields in
    Map.leave_class_signature { cs with
      csig_self = csig_self; csig_fields = csig_fields }


  and map_class_type_field ctf =
    let ctf = Map.enter_class_type_field ctf in
    let ctf_desc =
      match ctf.ctf_desc with
          Tctf_inher ct -> Tctf_inher (map_class_type ct)
        | Tctf_val (s, mut, virt, ct) ->
          Tctf_val (s, mut, virt, map_core_type ct)
        | Tctf_virt  (s, priv, ct) ->
          Tctf_virt (s, priv, map_core_type ct)
        | Tctf_meth  (s, priv, ct) ->
          Tctf_meth (s, priv, map_core_type ct)
        | Tctf_cstr  (ct1, ct2) ->
          Tctf_cstr (map_core_type ct1, map_core_type ct2)
    in
    Map.leave_class_type_field { ctf with ctf_desc = ctf_desc }

  and map_core_type ct =
    let ct = Map.enter_core_type ct in
    let ctyp_desc =
      match ct.ctyp_desc with
          Ttyp_any
        | Ttyp_var _ -> ct.ctyp_desc
        | Ttyp_arrow (label, ct1, ct2) ->
          Ttyp_arrow (label, map_core_type ct1, map_core_type ct2)
        | Ttyp_tuple list -> Ttyp_tuple (List.map map_core_type list)
        | Ttyp_constr (path, lid, list) ->
          Ttyp_constr (path, lid, List.map map_core_type list)
        | Ttyp_object list -> Ttyp_object (List.map map_core_field_type list)
        | Ttyp_class (path, lid, list, labels) ->
          Ttyp_class (path, lid, List.map map_core_type list, labels)
        | Ttyp_alias (ct, s) -> Ttyp_alias (map_core_type ct, s)
        | Ttyp_variant (list, bool, labels) ->
          Ttyp_variant (List.map map_row_field list, bool, labels)
        | Ttyp_poly (list, ct) -> Ttyp_poly (list, map_core_type ct)
        | Ttyp_package pack -> Ttyp_package (map_package_type pack)
    in
    Map.leave_core_type { ct with ctyp_desc = ctyp_desc }

  and map_core_field_type cft =
    let cft = Map.enter_core_field_type cft in
    let field_desc = match cft.field_desc with
        Tcfield_var -> Tcfield_var
      | Tcfield (s, ct) -> Tcfield (s, map_core_type ct)
    in
    Map.leave_core_field_type { cft with field_desc = field_desc }

  and map_class_structure cs =
    let cs = Map.enter_class_structure cs in
    let cstr_pat = map_pattern cs.cstr_pat in
    let cstr_fields = List.map map_class_field cs.cstr_fields in
    Map.leave_class_structure { cs with cstr_pat = cstr_pat;
      cstr_fields = cstr_fields }

  and map_row_field rf =
    match rf with
        Ttag (label, bool, list) ->
          Ttag (label, bool, List.map map_core_type list)
      | Tinherit ct -> Tinherit (map_core_type ct)

  and map_class_field cf =
    let cf = Map.enter_class_field cf in
    let cf_desc =
      match cf.cf_desc with
          Tcf_inher (ovf, cl, super, vals, meths) ->
            Tcf_inher (ovf, map_class_expr cl, super, vals, meths)
        | Tcf_constr (cty, cty') ->
          Tcf_constr (map_core_type cty, map_core_type cty')
        | Tcf_val (lab, name, mut, ident, Tcfk_virtual cty, override) ->
          Tcf_val (lab, name, mut, ident, Tcfk_virtual (map_core_type cty),
                   override)
        | Tcf_val (lab, name, mut, ident, Tcfk_concrete exp, override) ->
          Tcf_val (lab, name, mut, ident, Tcfk_concrete (map_expression exp),
                   override)
        | Tcf_meth (lab, name, priv, Tcfk_virtual cty, override) ->
          Tcf_meth (lab, name, priv, Tcfk_virtual (map_core_type cty),
                    override)
        | Tcf_meth (lab, name, priv, Tcfk_concrete exp, override) ->
          Tcf_meth (lab, name, priv, Tcfk_concrete (map_expression exp),
                    override)
        | Tcf_init exp -> Tcf_init (map_expression exp)
    in
    Map.leave_class_field { cf with cf_desc = cf_desc }
end


module DefaultMapArgument = struct

  let enter_structure t = t
  let enter_value_description t = t
  let enter_type_declaration t = t
  let enter_type_extension t = t
  let enter_extension_constructor t = t
  let enter_exception_declaration t = t
  let enter_pattern t = t
  let enter_expression t = t
  let enter_package_type t = t
  let enter_signature t = t
  let enter_signature_item t = t
  let enter_modtype_declaration t = t
  let enter_module_type t = t
  let enter_module_expr t = t
  let enter_with_constraint t = t
  let enter_class_expr t = t
  let enter_class_signature t = t
  let enter_class_description t = t
  let enter_class_type_declaration t = t
  let enter_class_infos t = t
  let enter_class_type t = t
  let enter_class_type_field t = t
  let enter_core_type t = t
  let enter_core_field_type t = t
  let enter_class_structure t = t
  let enter_class_field t = t
  let enter_structure_item t = t


  let leave_structure t = t
  let leave_value_description t = t
  let leave_type_declaration t = t
  let leave_type_extension t = t
  let leave_extension_constructor t = t

  let leave_exception_declaration t = t
  let leave_pattern t = t
  let leave_expression t = t
  let leave_package_type t = t
  let leave_signature t = t
  let leave_signature_item t = t
  let leave_modtype_declaration t = t
  let leave_module_type t = t
  let leave_module_expr t = t
  let leave_with_constraint t = t
  let leave_class_expr t = t
  let leave_class_signature t = t
  let leave_class_description t = t
  let leave_class_type_declaration t = t
  let leave_class_infos t = t
  let leave_class_type t = t
  let leave_class_type_field t = t
  let leave_core_type t = t
  let leave_core_field_type t = t
  let leave_class_structure t = t
  let leave_class_field t = t
  let leave_structure_item t = t

end
