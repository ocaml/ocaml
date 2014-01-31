(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Asttypes
open Typedtree

let opt f = function None -> () | Some x -> f x

let structure sub str =
  List.iter (sub # structure_item) str.str_items

let structure_item sub x =
  match x.str_desc with
  | Tstr_eval exp -> sub # expression exp
  | Tstr_value (rec_flag, list) -> sub # bindings (rec_flag, list)
  | Tstr_primitive (_id, _, v) -> sub # value_description v
  | Tstr_type list ->
      List.iter (fun (_id, _, decl) -> sub # type_declaration decl) list
  | Tstr_extension te -> sub # type_extension te
  | Tstr_exception (_id, _, decl) -> sub # exception_declaration decl
  | Tstr_exn_rebind (_id, _, _p, _) -> ()
  | Tstr_module (_id, _, mexpr) -> sub # module_expr mexpr
  | Tstr_recmodule list ->
      List.iter
        (fun (_id, _, mtype, mexpr) ->
          sub # module_type mtype;
          sub # module_expr mexpr
        )
        list
  | Tstr_modtype (_id, _, mtype) -> sub # module_type mtype
  | Tstr_open _ -> ()
  | Tstr_class list ->
      List.iter (fun (ci, _, _) -> sub # class_expr ci.ci_expr) list
  | Tstr_class_type list ->
      List.iter (fun (_id, _, ct) -> sub # class_type ct.ci_expr) list
  | Tstr_include (mexpr, _) -> sub # module_expr mexpr

let value_description sub x =
  sub # core_type x.val_desc

let type_declaration sub decl =
  List.iter
    (fun (ct1, ct2, _loc) -> sub # core_type ct1; sub # core_type ct2)
    decl.typ_cstrs;
  begin match decl.typ_kind with
  | Ttype_abstract -> ()
  | Ttype_variant list ->
      List.iter (fun (_s, _, cts, _loc) -> List.iter (sub # core_type) cts) list
  | Ttype_record list ->
      List.iter (fun (_s, _, _mut, ct, _loc) -> sub # core_type ct) list
  | Ttype_open -> ()
  end;
  opt (sub # core_type) decl.typ_manifest

let type_extension sub te =
  let extension_constructors ext =
    match ext.ext_kind with
      Text_decl(ctl, cto) ->
        List.iter (sub # core_type) ctl;
        opt (sub # core_type) cto
    | Text_rebind _ -> ()
  in
    List.iter extension_constructors te.tyext_constructors

let exception_declaration sub decl =
  List.iter (sub # core_type) decl.exn_args

let pattern sub pat =
  let extra = function
    | Tpat_type _
    | Tpat_unpack -> ()
    | Tpat_constraint ct -> sub # core_type ct
  in
  List.iter (fun (c, _) -> extra c) pat.pat_extra;
  match pat.pat_desc with
  | Tpat_any
  | Tpat_var _
  | Tpat_constant _ -> ()
  | Tpat_tuple l
  | Tpat_construct (_, _, l, _) -> List.iter (sub # pattern) l
  | Tpat_variant (_, po, _) -> opt (sub # pattern) po
  | Tpat_record (l, _) -> List.iter (fun (_, _, pat) -> sub # pattern pat) l
  | Tpat_array l -> List.iter (sub # pattern) l
  | Tpat_or (p1, p2, _) -> sub # pattern p1; sub # pattern p2
  | Tpat_alias (p, _, _)
  | Tpat_lazy p -> sub # pattern p

let expression sub exp =
  let extra = function
    | Texp_constraint (cty1, cty2) ->
        opt (sub # core_type) cty1; opt (sub # core_type) cty2
    | Texp_open _
    | Texp_newtype _ -> ()
    | Texp_poly cto -> opt (sub # core_type) cto
  in
  List.iter (function (c, _) -> extra c) exp.exp_extra;
  match exp.exp_desc with
  | Texp_ident _
  | Texp_constant _ -> ()
  | Texp_let (rec_flag, list, exp) ->
      sub # bindings (rec_flag, list);
      sub # expression exp
  | Texp_function (_, cases, _) ->
      sub # bindings (Nonrecursive, cases)
  | Texp_apply (exp, list) ->
      sub # expression exp;
      List.iter (fun (_, expo, _) -> opt (sub # expression) expo) list
  | Texp_match (exp, list, _) ->
      sub # expression exp;
      sub # bindings (Nonrecursive, list)
  | Texp_try (exp, list) ->
      sub # expression exp;
      sub # bindings (Nonrecursive, list)
  | Texp_tuple list ->
      List.iter (sub # expression) list
  | Texp_construct (_, _, args, _) ->
      List.iter (sub # expression) args
  | Texp_variant (_, expo) ->
      opt (sub # expression) expo
  | Texp_record (list, expo) ->
      List.iter (fun (_, _, exp) -> sub # expression exp) list;
      opt (sub # expression) expo
  | Texp_field (exp, _, _label) ->
      sub # expression exp
  | Texp_setfield (exp1, _, _label, exp2) ->
      sub # expression exp1;
      sub # expression exp2
  | Texp_array list ->
      List.iter (sub # expression) list
  | Texp_ifthenelse (exp1, exp2, expo) ->
      sub # expression exp1;
      sub # expression exp2;
      opt (sub # expression) expo
  | Texp_sequence (exp1, exp2) ->
      sub # expression exp1;
      sub # expression exp2
  | Texp_while (exp1, exp2) ->
      sub # expression exp1;
      sub # expression exp2
  | Texp_for (_id, _, exp1, exp2, _dir, exp3) ->
      sub # expression exp1;
      sub # expression exp2;
      sub # expression exp3
  | Texp_when (exp1, exp2) ->
      sub # expression exp1;
      sub # expression exp2
  | Texp_send (exp, _meth, expo) ->
      sub # expression exp;
      opt (sub # expression) expo
  | Texp_new (_path, _, _) -> ()
  | Texp_instvar (_, _path, _) -> ()
  | Texp_setinstvar (_, _, _, exp) ->
      sub # expression exp
  | Texp_override (_, list) ->
      List.iter (fun (_path, _, exp) -> sub # expression exp) list
  | Texp_letmodule (_id, _, mexpr, exp) ->
      sub # module_expr mexpr;
      sub # expression exp
  | Texp_assert exp -> sub # expression exp
  | Texp_assertfalse -> ()
  | Texp_lazy exp -> sub # expression exp
  | Texp_object (cl, _) ->
      sub # class_structure cl
  | Texp_pack (mexpr) ->
      sub # module_expr mexpr


let package_type sub pack =
  List.iter (fun (_s, ct) -> sub # core_type ct) pack.pack_fields

let signature sub sg =
  List.iter (sub # signature_item) sg.sig_items

let signature_item sub item =
  match item.sig_desc with
  | Tsig_value (_id, _, v) ->
      sub # value_description v
  | Tsig_type list ->
      List.iter (fun (_id, _, decl) -> sub # type_declaration decl) list
  | Tsig_extension te ->
      sub # type_extension te
  | Tsig_exception (_id, _, decl) ->
      sub # exception_declaration decl
  | Tsig_module (_id, _, mtype) ->
      sub # module_type mtype
  | Tsig_recmodule list ->
      List.iter (fun (_id, _, mtype) -> sub # module_type mtype) list
  | Tsig_modtype (_id, _, mdecl) ->
      sub # modtype_declaration mdecl
  | Tsig_open _ -> ()
  | Tsig_include (mty,_) -> sub # module_type mty
  | Tsig_class list ->
      List.iter (sub # class_description) list
  | Tsig_class_type list ->
      List.iter (sub # class_type_declaration) list

let modtype_declaration sub mdecl =
  match mdecl with
  | Tmodtype_abstract -> ()
  | Tmodtype_manifest mtype -> sub # module_type mtype

let class_description sub cd =
  sub # class_type cd.ci_expr

let class_type_declaration sub cd =
  sub # class_type cd.ci_expr

let module_type sub mty =
  match mty.mty_desc with
  | Tmty_ident (_path, _) -> ()
  | Tmty_signature sg -> sub # signature sg
  | Tmty_functor (_id, _, mtype1, mtype2) ->
      sub # module_type mtype1; sub # module_type mtype2
  | Tmty_with (mtype, list) ->
      sub # module_type mtype;
      List.iter (fun (_, _, withc) -> sub # with_constraint withc) list
  | Tmty_typeof mexpr ->
      sub # module_expr mexpr

let with_constraint sub cstr =
  match cstr with
  | Twith_type decl -> sub # type_declaration decl
  | Twith_module _ -> ()
  | Twith_typesubst decl -> sub # type_declaration decl
  | Twith_modsubst _ -> ()

let module_expr sub mexpr =
  match mexpr.mod_desc with
  | Tmod_ident (_p, _) -> ()
  | Tmod_structure st -> sub # structure st
  | Tmod_functor (_id, _, mtype, mexpr) ->
      sub # module_type mtype;
      sub # module_expr mexpr
  | Tmod_apply (mexp1, mexp2, _) ->
      sub # module_expr mexp1;
      sub # module_expr mexp2
  | Tmod_constraint (mexpr, _, Tmodtype_implicit, _ ) ->
      sub # module_expr mexpr
  | Tmod_constraint (mexpr, _, Tmodtype_explicit mtype, _) ->
      sub # module_expr mexpr;
      sub # module_type mtype
  | Tmod_unpack (exp, _mty) ->
      sub # expression exp
(*          sub # module_type mty *)

let class_expr sub cexpr =
  match cexpr.cl_desc with
  | Tcl_constraint (cl, None, _, _, _ ) ->
      sub # class_expr cl;
  | Tcl_structure clstr -> sub # class_structure clstr
  | Tcl_fun (_label, pat, priv, cl, _partial) ->
      sub # pattern pat;
      List.iter (fun (_id, _, exp) -> sub # expression exp) priv;
      sub # class_expr cl
  | Tcl_apply (cl, args) ->
      sub # class_expr cl;
      List.iter (fun (_label, expo, _) -> opt (sub # expression) expo) args
  | Tcl_let (rec_flat, bindings, ivars, cl) ->
      sub # bindings (rec_flat, bindings);
      List.iter (fun (_id, _, exp) -> sub # expression exp) ivars;
      sub # class_expr cl
  | Tcl_constraint (cl, Some clty, _vals, _meths, _concrs) ->
      sub # class_expr cl;
      sub # class_type clty
  | Tcl_ident (_, _, tyl) ->
      List.iter (sub # core_type) tyl

let class_type sub ct =
  match ct.cltyp_desc with
  | Tcty_signature csg -> sub # class_signature csg
  | Tcty_constr (_path, _, list) -> List.iter (sub # core_type) list
  | Tcty_fun (_label, ct, cl) ->
      sub # core_type ct;
      sub # class_type cl

let class_signature sub cs =
  sub # core_type cs.csig_self;
  List.iter (sub # class_type_field) cs.csig_fields

let class_type_field sub ctf =
  match ctf.ctf_desc with
  | Tctf_inher ct -> sub # class_type ct
  | Tctf_val (_s, _mut, _virt, ct) ->
      sub # core_type ct
  | Tctf_virt  (_s, _priv, ct) ->
      sub # core_type ct
  | Tctf_meth  (_s, _priv, ct) ->
      sub # core_type ct
  | Tctf_cstr  (ct1, ct2) ->
      sub # core_type ct1;
      sub # core_type ct2

let core_type sub ct =
  match ct.ctyp_desc with
  | Ttyp_any -> ()
  | Ttyp_var _s -> ()
  | Ttyp_arrow (_label, ct1, ct2) ->
      sub # core_type ct1;
      sub # core_type ct2
  | Ttyp_tuple list -> List.iter (sub # core_type) list
  | Ttyp_constr (_path, _, list) ->
      List.iter (sub # core_type) list
  | Ttyp_object list ->
      List.iter (sub # core_field_type) list
  | Ttyp_class (_path, _, list, _labels) ->
      List.iter (sub # core_type) list
  | Ttyp_alias (ct, _s) ->
      sub # core_type ct
  | Ttyp_variant (list, _bool, _labels) ->
      List.iter (sub # row_field) list
  | Ttyp_poly (_list, ct) -> sub # core_type ct
  | Ttyp_package pack -> sub # package_type pack

let core_field_type sub cft =
  match cft.field_desc with
  | Tcfield_var -> ()
  | Tcfield (_s, ct) -> sub # core_type ct

let class_structure sub cs =
  sub # pattern cs.cstr_pat;
  List.iter (sub # class_field) cs.cstr_fields

let row_field sub rf =
  match rf with
  | Ttag (_label, _bool, list) -> List.iter (sub # core_type) list
  | Tinherit ct -> sub # core_type ct

let class_field sub cf =
  match cf.cf_desc with
  | Tcf_inher (_ovf, cl, _super, _vals, _meths) ->
      sub # class_expr cl
  | Tcf_constr (cty, cty') ->
      sub # core_type cty;
      sub # core_type cty'
  | Tcf_val (_lab, _, _, _mut, Tcfk_virtual cty, _override) ->
      sub # core_type cty
  | Tcf_val (_lab, _, _, _mut, Tcfk_concrete exp, _override) ->
      sub # expression exp
  | Tcf_meth (_lab, _, _priv, Tcfk_virtual cty, _override) ->
      sub # core_type cty
  | Tcf_meth (_lab, _, _priv, Tcfk_concrete exp, _override) ->
      sub # expression exp
  | Tcf_init exp ->
      sub # expression exp

let bindings sub (_rec_flag, list) =
  List.iter (sub # binding) list

let binding sub (pat, exp) =
  sub # pattern pat;
  sub # expression exp

class iter = object(this)
  method binding = binding this
  method bindings = bindings this
  method class_description = class_description this
  method class_expr = class_expr this
  method class_field = class_field this
  method class_signature = class_signature this
  method class_structure = class_structure this
  method class_type = class_type this
  method class_type_declaration = class_type_declaration this
  method class_type_field = class_type_field this
  method core_field_type = core_field_type this
  method core_type = core_type this
  method exception_declaration = exception_declaration this
  method expression = expression this
  method modtype_declaration = modtype_declaration this
  method module_expr = module_expr this
  method module_type = module_type this
  method package_type = package_type this
  method pattern = pattern this
  method row_field = row_field this
  method signature = signature this
  method signature_item = signature_item this
  method structure = structure this
  method structure_item = structure_item this
  method type_declaration = type_declaration this
  method type_extension = type_extension this
  method value_description = value_description this
  method with_constraint = with_constraint this
end
