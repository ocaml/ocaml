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

(*
TODO:
 - 2012/05/10: Follow camlp4 way of building map and iter using classes
     and inheritance ?
*)

open Asttypes
open Typedtree

module type IteratorArgument = sig

    val enter_structure : structure -> unit
    val enter_value_description : value_description -> unit
    val enter_type_declaration : type_declaration -> unit
    val enter_type_extension : type_extension -> unit
    val enter_extension_constructor : extension_constructor -> unit
    val enter_exception_declaration :
      exception_declaration -> unit
    val enter_pattern : pattern -> unit
    val enter_expression : expression -> unit
    val enter_package_type : package_type -> unit
    val enter_signature : signature -> unit
    val enter_signature_item : signature_item -> unit
    val enter_modtype_declaration : modtype_declaration -> unit
    val enter_module_type : module_type -> unit
    val enter_module_expr : module_expr -> unit
    val enter_with_constraint : with_constraint -> unit
    val enter_class_expr : class_expr -> unit
    val enter_class_signature : class_signature -> unit
    val enter_class_declaration : class_declaration -> unit
    val enter_class_description : class_description -> unit
    val enter_class_type_declaration : class_type_declaration -> unit
    val enter_class_type : class_type -> unit
    val enter_class_type_field : class_type_field -> unit
    val enter_core_type : core_type -> unit
    val enter_core_field_type : core_field_type -> unit
    val enter_class_structure : class_structure -> unit
    val enter_class_field : class_field -> unit
    val enter_structure_item : structure_item -> unit


    val leave_structure : structure -> unit
    val leave_value_description : value_description -> unit
    val leave_type_declaration : type_declaration -> unit
    val leave_type_extension : type_extension -> unit
    val leave_extension_constructor : extension_constructor -> unit
    val leave_exception_declaration :
      exception_declaration -> unit
    val leave_pattern : pattern -> unit
    val leave_expression : expression -> unit
    val leave_package_type : package_type -> unit
    val leave_signature : signature -> unit
    val leave_signature_item : signature_item -> unit
    val leave_modtype_declaration : modtype_declaration -> unit
    val leave_module_type : module_type -> unit
    val leave_module_expr : module_expr -> unit
    val leave_with_constraint : with_constraint -> unit
    val leave_class_expr : class_expr -> unit
    val leave_class_signature : class_signature -> unit
    val leave_class_declaration : class_declaration -> unit
    val leave_class_description : class_description -> unit
    val leave_class_type_declaration : class_type_declaration -> unit
    val leave_class_type : class_type -> unit
    val leave_class_type_field : class_type_field -> unit
    val leave_core_type : core_type -> unit
    val leave_core_field_type : core_field_type -> unit
    val leave_class_structure : class_structure -> unit
    val leave_class_field : class_field -> unit
    val leave_structure_item : structure_item -> unit

    val enter_bindings : rec_flag -> unit
    val enter_binding : pattern -> expression -> unit
    val leave_binding : pattern -> expression -> unit
    val leave_bindings : rec_flag -> unit

      end

module MakeIterator(Iter : IteratorArgument) : sig

    val iter_structure : structure -> unit
    val iter_signature : signature -> unit
    val iter_structure_item : structure_item -> unit
    val iter_signature_item : signature_item -> unit
    val iter_expression : expression -> unit
    val iter_module_type : module_type -> unit
    val iter_pattern : pattern -> unit
    val iter_class_expr : class_expr -> unit

  end = struct

    let may_iter f v =
      match v with
        None -> ()
      | Some x -> f x


    open Asttypes

    let rec iter_structure str =
      Iter.enter_structure str;
      List.iter iter_structure_item str.str_items;
      Iter.leave_structure str


    and iter_binding (pat, exp) =
      Iter.enter_binding pat exp;
      iter_pattern pat;
      iter_expression exp;
      Iter.leave_binding pat exp

    and iter_bindings rec_flag list =
      Iter.enter_bindings rec_flag;
      List.iter iter_binding list;
      Iter.leave_bindings rec_flag

    and iter_structure_item item =
      Iter.enter_structure_item item;
      begin
        match item.str_desc with
          Tstr_eval exp -> iter_expression exp
        | Tstr_value (rec_flag, list) ->
            iter_bindings rec_flag list
        | Tstr_primitive (id, _, v) -> iter_value_description v
        | Tstr_type list ->
            List.iter (fun (id, _, decl) -> iter_type_declaration decl) list
        | Tstr_extension tyext ->
            iter_type_extension tyext
        | Tstr_exception (id, _, decl) -> iter_exception_declaration decl
        | Tstr_exn_rebind (id, _, p, _) -> ()
        | Tstr_module (id, _, mexpr) ->
            iter_module_expr mexpr
        | Tstr_recmodule list ->
            List.iter (fun (id, _, mtype, mexpr) ->
                iter_module_type mtype;
                iter_module_expr mexpr) list
        | Tstr_modtype (id, _, mtype) ->
            iter_module_type mtype
        | Tstr_open _ -> ()
        | Tstr_class list ->
            List.iter (fun (ci, _, _) -> iter_class_declaration ci) list
        | Tstr_class_type list ->
            List.iter
              (fun (id, _, ct) -> iter_class_type_declaration ct)
              list
        | Tstr_include (mexpr, _) ->
            iter_module_expr mexpr
      end;
      Iter.leave_structure_item item

    and iter_value_description v =
      Iter.enter_value_description v;
      iter_core_type v.val_desc;
      Iter.leave_value_description v

    and iter_type_declaration decl =
      Iter.enter_type_declaration decl;
      List.iter iter_core_type decl.typ_params;
      List.iter (fun (ct1, ct2, loc) ->
          iter_core_type ct1;
          iter_core_type ct2
      ) decl.typ_cstrs;
      begin match decl.typ_kind with
          Ttype_abstract -> ()
        | Ttype_variant list ->
            List.iter (fun (s, _, cts, loc) ->
                List.iter iter_core_type cts
            ) list
        | Ttype_record list ->
            List.iter (fun (s, _, mut, ct, loc) ->
                iter_core_type ct
            ) list
        | Ttype_open -> ()
      end;
      option iter_core_type decl.typ_manifest;
      Iter.leave_type_declaration decl

    and iter_type_extension tyext =
      Iter.enter_type_extension tyext;
      List.iter iter_core_type tyext.tyext_params;
      List.iter iter_extension_constructor tyext.tyext_constructors;
      Iter.leave_type_extension tyext

    and iter_extension_constructor ext =
      Iter.enter_extension_constructor ext;
      begin match ext.ext_kind with
          Text_decl(args, ret) ->
            List.iter iter_core_type args;
            option iter_core_type ret
        | Text_rebind _ -> ()
      end;
      Iter.leave_extension_constructor ext;

    and iter_exception_declaration decl =
      Iter.enter_exception_declaration decl;
      List.iter iter_core_type decl.exn_args;
      Iter.leave_exception_declaration decl;

    and iter_pattern pat =
      Iter.enter_pattern pat;
      List.iter (fun (cstr, _) -> match cstr with
              | Tpat_type _ -> ()
              | Tpat_unpack -> ()
              | Tpat_constraint ct -> iter_core_type ct) pat.pat_extra;
      begin
        match pat.pat_desc with
          Tpat_any -> ()
        | Tpat_var (id, _) -> ()
        | Tpat_alias (pat1, _, _) -> iter_pattern pat1
        | Tpat_constant cst -> ()
        | Tpat_tuple list ->
            List.iter iter_pattern list
        | Tpat_construct (_, _, args, _) ->
            List.iter iter_pattern args
        | Tpat_variant (label, pato, _) ->
            begin match pato with
                None -> ()
              | Some pat -> iter_pattern pat
            end
        | Tpat_record (list, closed) ->
            List.iter (fun (_, _, pat) -> iter_pattern pat) list
        | Tpat_array list -> List.iter iter_pattern list
        | Tpat_or (p1, p2, _) -> iter_pattern p1; iter_pattern p2
        | Tpat_lazy p -> iter_pattern p
      end;
      Iter.leave_pattern pat

    and option f x = match x with None -> () | Some e -> f e

    and iter_expression exp =
      Iter.enter_expression exp;
      List.iter (function (cstr, _) ->
        match cstr with
          Texp_constraint (cty1, cty2) ->
            option iter_core_type cty1; option iter_core_type cty2
        | Texp_open (_, path, _, _) -> ()
        | Texp_poly cto -> option iter_core_type cto
        | Texp_newtype s -> ())
        exp.exp_extra;
      begin
        match exp.exp_desc with
          Texp_ident (path, _, _) -> ()
        | Texp_constant cst -> ()
        | Texp_let (rec_flag, list, exp) ->
            iter_bindings rec_flag list;
            iter_expression exp
        | Texp_function (label, cases, _) ->
            iter_bindings Nonrecursive cases
        | Texp_apply (exp, list) ->
            iter_expression exp;
            List.iter (fun (label, expo, _) ->
                match expo with
                  None -> ()
                | Some exp -> iter_expression exp
            ) list
        | Texp_match (exp, list, _) ->
            iter_expression exp;
            iter_bindings Nonrecursive list
        | Texp_try (exp, list) ->
            iter_expression exp;
            iter_bindings Nonrecursive list
        | Texp_tuple list ->
            List.iter iter_expression list
        | Texp_construct (_, _, args, _) ->
            List.iter iter_expression args
        | Texp_variant (label, expo) ->
            begin match expo with
                None -> ()
              | Some exp -> iter_expression exp
            end
        | Texp_record (list, expo) ->
            List.iter (fun (_, _, exp) -> iter_expression exp) list;
            begin match expo with
                None -> ()
              | Some exp -> iter_expression exp
            end
        | Texp_field (exp, _, label) ->
            iter_expression exp
        | Texp_setfield (exp1, _, label, exp2) ->
            iter_expression exp1;
            iter_expression exp2
        | Texp_array list ->
            List.iter iter_expression list
        | Texp_ifthenelse (exp1, exp2, expo) ->
            iter_expression exp1;
            iter_expression exp2;
            begin match expo with
                None -> ()
              | Some exp -> iter_expression exp
            end
        | Texp_sequence (exp1, exp2) ->
            iter_expression exp1;
            iter_expression exp2
        | Texp_while (exp1, exp2) ->
            iter_expression exp1;
            iter_expression exp2
        | Texp_for (id, _, exp1, exp2, dir, exp3) ->
            iter_expression exp1;
            iter_expression exp2;
            iter_expression exp3
        | Texp_when (exp1, exp2) ->
            iter_expression exp1;
            iter_expression exp2
        | Texp_send (exp, meth, expo) ->
            iter_expression exp;
          begin
            match expo with
                None -> ()
              | Some exp -> iter_expression exp
          end
        | Texp_new (path, _, _) -> ()
        | Texp_instvar (_, path, _) -> ()
        | Texp_setinstvar (_, _, _, exp) ->
            iter_expression exp
        | Texp_override (_, list) ->
            List.iter (fun (path, _, exp) ->
                iter_expression exp
            ) list
        | Texp_letmodule (id, _, mexpr, exp) ->
            iter_module_expr mexpr;
            iter_expression exp
        | Texp_assert exp -> iter_expression exp
        | Texp_assertfalse -> ()
        | Texp_lazy exp -> iter_expression exp
        | Texp_object (cl, _) ->
            iter_class_structure cl
        | Texp_pack (mexpr) ->
            iter_module_expr mexpr
      end;
      Iter.leave_expression exp;

    and iter_package_type pack =
      Iter.enter_package_type pack;
      List.iter (fun (s, ct) -> iter_core_type ct) pack.pack_fields;
      Iter.leave_package_type pack;

    and iter_signature sg =
      Iter.enter_signature sg;
      List.iter iter_signature_item sg.sig_items;
      Iter.leave_signature sg;

    and iter_signature_item item =
      Iter.enter_signature_item item;
      begin
        match item.sig_desc with
          Tsig_value (id, _, v) ->
            iter_value_description v
        | Tsig_type list ->
            List.iter (fun (id, _, decl) ->
                iter_type_declaration decl
            ) list
        | Tsig_extension tyext ->
            iter_type_extension tyext
        | Tsig_exception (id, _, decl) ->
            iter_exception_declaration decl
        | Tsig_module (id, _, mtype) ->
            iter_module_type mtype
        | Tsig_recmodule list ->
            List.iter (fun (id, _, mtype) -> iter_module_type mtype) list
        | Tsig_modtype (id, _, mdecl) ->
            iter_modtype_declaration mdecl
        | Tsig_open _ -> ()
        | Tsig_include (mty,_) -> iter_module_type mty
        | Tsig_class list ->
            List.iter iter_class_description list
        | Tsig_class_type list ->
            List.iter iter_class_type_declaration list
      end;
      Iter.leave_signature_item item;

    and iter_modtype_declaration mdecl =
      Iter.enter_modtype_declaration mdecl;
      begin
        match mdecl with
          Tmodtype_abstract -> ()
        | Tmodtype_manifest mtype -> iter_module_type mtype
      end;
      Iter.leave_modtype_declaration mdecl;

    and iter_class_declaration cd =
      Iter.enter_class_declaration cd;
      List.iter iter_core_type cd.ci_params;
      iter_class_expr cd.ci_expr;
      Iter.leave_class_declaration cd;

    and iter_class_description cd =
      Iter.enter_class_description cd;
      List.iter iter_core_type cd.ci_params;
      iter_class_type cd.ci_expr;
      Iter.leave_class_description cd;

    and iter_class_type_declaration cd =
      Iter.enter_class_type_declaration cd;
      List.iter iter_core_type cd.ci_params;
      iter_class_type cd.ci_expr;
      Iter.leave_class_type_declaration cd;

    and iter_module_type mty =
      Iter.enter_module_type mty;
      begin
        match mty.mty_desc with
          Tmty_ident (path, _) -> ()
        | Tmty_signature sg -> iter_signature sg
        | Tmty_functor (id, _, mtype1, mtype2) ->
            iter_module_type mtype1; iter_module_type mtype2
        | Tmty_with (mtype, list) ->
            iter_module_type mtype;
            List.iter (fun (path, _, withc) ->
                iter_with_constraint withc
            ) list
        | Tmty_typeof mexpr ->
            iter_module_expr mexpr
      end;
      Iter.leave_module_type mty;

    and iter_with_constraint cstr =
      Iter.enter_with_constraint cstr;
      begin
        match cstr with
          Twith_type decl -> iter_type_declaration decl
        | Twith_module _ -> ()
        | Twith_typesubst decl -> iter_type_declaration decl
        | Twith_modsubst _ -> ()
      end;
      Iter.leave_with_constraint cstr;

    and iter_module_expr mexpr =
      Iter.enter_module_expr mexpr;
      begin
        match mexpr.mod_desc with
          Tmod_ident (p, _) -> ()
        | Tmod_structure st -> iter_structure st
        | Tmod_functor (id, _, mtype, mexpr) ->
            iter_module_type mtype;
            iter_module_expr mexpr
        | Tmod_apply (mexp1, mexp2, _) ->
            iter_module_expr mexp1;
            iter_module_expr mexp2
        | Tmod_constraint (mexpr, _, Tmodtype_implicit, _ ) ->
            iter_module_expr mexpr
        | Tmod_constraint (mexpr, _, Tmodtype_explicit mtype, _) ->
            iter_module_expr mexpr;
            iter_module_type mtype
        | Tmod_unpack (exp, mty) ->
            iter_expression exp
(*          iter_module_type mty *)
      end;
      Iter.leave_module_expr mexpr;

    and iter_class_expr cexpr =
      Iter.enter_class_expr cexpr;
      begin
        match cexpr.cl_desc with
        | Tcl_constraint (cl, None, _, _, _ ) ->
            iter_class_expr cl;
        | Tcl_structure clstr -> iter_class_structure clstr
        | Tcl_fun (label, pat, priv, cl, partial) ->
          iter_pattern pat;
          List.iter (fun (id, _, exp) -> iter_expression exp) priv;
          iter_class_expr cl

        | Tcl_apply (cl, args) ->
            iter_class_expr cl;
            List.iter (fun (label, expo, _) ->
                match expo with
                  None -> ()
                | Some exp -> iter_expression exp
            ) args

        | Tcl_let (rec_flat, bindings, ivars, cl) ->
          iter_bindings rec_flat bindings;
          List.iter (fun (id, _, exp) -> iter_expression exp) ivars;
            iter_class_expr cl

        | Tcl_constraint (cl, Some clty, vals, meths, concrs) ->
            iter_class_expr cl;
            iter_class_type clty

        | Tcl_ident (_, _, tyl) ->
            List.iter iter_core_type tyl
      end;
      Iter.leave_class_expr cexpr;

    and iter_class_type ct =
      Iter.enter_class_type ct;
      begin
        match ct.cltyp_desc with
          Tcty_signature csg -> iter_class_signature csg
        | Tcty_constr (path, _, list) ->
            List.iter iter_core_type list
        | Tcty_fun (label, ct, cl) ->
            iter_core_type ct;
            iter_class_type cl
      end;
      Iter.leave_class_type ct;

    and iter_class_signature cs =
      Iter.enter_class_signature cs;
      iter_core_type cs.csig_self;
      List.iter iter_class_type_field cs.csig_fields;
      Iter.leave_class_signature cs


    and iter_class_type_field ctf =
      Iter.enter_class_type_field ctf;
      begin
        match ctf.ctf_desc with
          Tctf_inher ct -> iter_class_type ct
        | Tctf_val (s, mut, virt, ct) ->
            iter_core_type ct
        | Tctf_virt  (s, priv, ct) ->
            iter_core_type ct
        | Tctf_meth  (s, priv, ct) ->
            iter_core_type ct
        | Tctf_cstr  (ct1, ct2) ->
            iter_core_type ct1;
            iter_core_type ct2
      end;
      Iter.leave_class_type_field ctf

    and iter_core_type ct =
      Iter.enter_core_type ct;
      begin
        match ct.ctyp_desc with
          Ttyp_any -> ()
        | Ttyp_var s -> ()
        | Ttyp_arrow (label, ct1, ct2) ->
            iter_core_type ct1;
            iter_core_type ct2
        | Ttyp_tuple list -> List.iter iter_core_type list
        | Ttyp_constr (path, _, list) ->
            List.iter iter_core_type list
        | Ttyp_object list ->
            List.iter iter_core_field_type list
        | Ttyp_class (path, _, list, labels) ->
            List.iter iter_core_type list
        | Ttyp_alias (ct, s) ->
            iter_core_type ct
        | Ttyp_variant (list, bool, labels) ->
            List.iter iter_row_field list
        | Ttyp_poly (list, ct) -> iter_core_type ct
        | Ttyp_package pack -> iter_package_type pack
      end;
      Iter.leave_core_type ct;

    and iter_core_field_type cft =
      Iter.enter_core_field_type cft;
      begin match cft.field_desc with
          Tcfield_var -> ()
        | Tcfield (s, ct) -> iter_core_type ct
      end;
      Iter.leave_core_field_type cft;

    and iter_class_structure cs =
      Iter.enter_class_structure cs;
      iter_pattern cs.cstr_pat;
      List.iter iter_class_field cs.cstr_fields;
      Iter.leave_class_structure cs;


    and iter_row_field rf =
      match rf with
        Ttag (label, bool, list) ->
          List.iter iter_core_type list
      | Tinherit ct -> iter_core_type ct

    and iter_class_field cf =
      Iter.enter_class_field cf;
      begin
        match cf.cf_desc with
          Tcf_inher (ovf, cl, super, _vals, _meths) ->
          iter_class_expr cl
      | Tcf_constr (cty, cty') ->
          iter_core_type cty;
          iter_core_type cty'
      | Tcf_val (lab, _, _, mut, Tcfk_virtual cty, override) ->
          iter_core_type cty
      | Tcf_val (lab, _, _, mut, Tcfk_concrete exp, override) ->
          iter_expression exp
      | Tcf_meth (lab, _, priv, Tcfk_virtual cty, override) ->
          iter_core_type cty
      | Tcf_meth (lab, _, priv, Tcfk_concrete exp, override) ->
          iter_expression exp
(*      | Tcf_let (rec_flag, bindings, exps) ->
          iter_bindings rec_flag bindings;
        List.iter (fun (id, _, exp) -> iter_expression exp) exps; *)
      | Tcf_init exp ->
          iter_expression exp
      end;
      Iter.leave_class_field cf;

  end

module DefaultIteratorArgument = struct

      let enter_structure _ = ()
      let enter_value_description _ = ()
      let enter_type_declaration _ = ()
      let enter_type_extension _ = ()
      let enter_extension_constructor _ = ()
      let enter_exception_declaration _ = ()
      let enter_pattern _ = ()
      let enter_expression _ = ()
      let enter_package_type _ = ()
      let enter_signature _ = ()
      let enter_signature_item _ = ()
      let enter_modtype_declaration _ = ()
      let enter_module_type _ = ()
      let enter_module_expr _ = ()
      let enter_with_constraint _ = ()
      let enter_class_expr _ = ()
      let enter_class_signature _ = ()
      let enter_class_declaration _ = ()
      let enter_class_description _ = ()
      let enter_class_type_declaration _ = ()
      let enter_class_type _ = ()
      let enter_class_type_field _ = ()
      let enter_core_type _ = ()
      let enter_core_field_type _ = ()
      let enter_class_structure _ = ()
    let enter_class_field _ = ()
    let enter_structure_item _ = ()


      let leave_structure _ = ()
      let leave_value_description _ = ()
      let leave_type_declaration _ = ()
      let leave_type_extension _ = ()
      let leave_extension_constructor _ = ()
      let leave_exception_declaration _ = ()
      let leave_pattern _ = ()
      let leave_expression _ = ()
      let leave_package_type _ = ()
      let leave_signature _ = ()
      let leave_signature_item _ = ()
      let leave_modtype_declaration _ = ()
      let leave_module_type _ = ()
      let leave_module_expr _ = ()
      let leave_with_constraint _ = ()
      let leave_class_expr _ = ()
      let leave_class_signature _ = ()
      let leave_class_declaration _ = ()
      let leave_class_description _ = ()
      let leave_class_type_declaration _ = ()
      let leave_class_type _ = ()
      let leave_class_type_field _ = ()
      let leave_core_type _ = ()
      let leave_core_field_type _ = ()
      let leave_class_structure _ = ()
    let leave_class_field _ = ()
    let leave_structure_item _ = ()

    let enter_binding _ _ = ()
    let leave_binding _ _ = ()

    let enter_bindings _ = ()
    let leave_bindings _ = ()

  end
