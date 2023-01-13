(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Alain Frisch, LexiFi                            *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Typedtree

(* TODO: add 'methods' for extension,
   include_declaration, include_description *)

type mapper =
  {
    attribute : mapper -> attribute -> attribute;
    attributes : mapper -> attributes -> attributes;
    binding_op: mapper -> binding_op -> binding_op;
    case: 'k . mapper -> 'k case -> 'k case;
    class_declaration: mapper -> class_declaration -> class_declaration;
    class_description: mapper -> class_description -> class_description;
    class_expr: mapper -> class_expr -> class_expr;
    class_field: mapper -> class_field -> class_field;
    class_signature: mapper -> class_signature -> class_signature;
    class_structure: mapper -> class_structure -> class_structure;
    class_type: mapper -> class_type -> class_type;
    class_type_declaration: mapper -> class_type_declaration ->
      class_type_declaration;
    class_type_field: mapper -> class_type_field -> class_type_field;
    env: mapper -> Env.t -> Env.t;
    expr: mapper -> expression -> expression;
    extension_constructor: mapper -> extension_constructor ->
      extension_constructor;
    location: mapper -> Location.t -> Location.t;
    module_binding: mapper -> module_binding -> module_binding;
    module_coercion: mapper -> module_coercion -> module_coercion;
    module_declaration: mapper -> module_declaration -> module_declaration;
    module_substitution: mapper -> module_substitution -> module_substitution;
    module_expr: mapper -> module_expr -> module_expr;
    module_type: mapper -> module_type -> module_type;
    module_type_declaration:
      mapper -> module_type_declaration -> module_type_declaration;
    package_type: mapper -> package_type -> package_type;
    pat: 'k . mapper -> 'k general_pattern -> 'k general_pattern;
    row_field: mapper -> row_field -> row_field;
    object_field: mapper -> object_field -> object_field;
    open_declaration: mapper -> open_declaration -> open_declaration;
    open_description: mapper -> open_description -> open_description;
    signature: mapper -> signature -> signature;
    signature_item: mapper -> signature_item -> signature_item;
    structure: mapper -> structure -> structure;
    structure_item: mapper -> structure_item -> structure_item;
    typ: mapper -> core_type -> core_type;
    type_declaration: mapper -> type_declaration -> type_declaration;
    type_declarations: mapper -> (rec_flag * type_declaration list)
      -> (rec_flag * type_declaration list);
    type_extension: mapper -> type_extension -> type_extension;
    type_exception: mapper -> type_exception -> type_exception;
    type_kind: mapper -> type_kind -> type_kind;
    value_binding: mapper -> value_binding -> value_binding;
    value_bindings: mapper -> (rec_flag * value_binding list) ->
      (rec_flag * value_binding list);
    value_description: mapper -> value_description -> value_description;
    with_constraint: mapper -> with_constraint -> with_constraint;
  }

let id x = x
let tuple2 f1 f2 (x, y) = (f1 x, f2 y)
let tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
let map_loc sub {loc; txt} = {loc=sub.location sub loc; txt}

let location _sub l = l

let attribute sub x =
  let mapper = {
    Ast_mapper.default_mapper
    with location = fun _this x -> sub.location sub x
  } in
  Parsetree.{
    attr_name = map_loc sub x.attr_name;
    attr_payload = mapper.payload mapper x.attr_payload;
    attr_loc = sub.location sub x.attr_loc
  }

let attributes sub l = List.map (attribute sub) l

let structure sub {str_items; str_type; str_final_env} =
  {
    str_items = List.map (sub.structure_item sub) str_items;
    str_final_env = sub.env sub str_final_env;
    str_type;
  }

let class_infos sub f x =
  {x with
   ci_loc = sub.location sub x.ci_loc;
   ci_id_name = map_loc sub x.ci_id_name;
   ci_params = List.map (tuple2 (sub.typ sub) id) x.ci_params;
   ci_expr = f x.ci_expr;
   ci_attributes = sub.attributes sub x.ci_attributes;
  }

let module_type_declaration sub x =
  let mtd_loc = sub.location sub x.mtd_loc in
  let mtd_name = map_loc sub x.mtd_name in
  let mtd_type = Option.map (sub.module_type sub) x.mtd_type in
  let mtd_attributes = sub.attributes sub x.mtd_attributes in
  {x with mtd_loc; mtd_name; mtd_type; mtd_attributes}

let module_declaration sub x =
  let md_loc = sub.location sub x.md_loc in
  let md_name = map_loc sub x.md_name in
  let md_type = sub.module_type sub x.md_type in
  let md_attributes = sub.attributes sub x.md_attributes in
  {x with md_loc; md_name; md_type; md_attributes}

let module_substitution sub x =
  let ms_loc = sub.location sub x.ms_loc in
  let ms_name = map_loc sub x.ms_name in
  let ms_txt = map_loc sub x.ms_txt in
  let ms_attributes = sub.attributes sub x.ms_attributes in
  {x with ms_loc; ms_name; ms_txt; ms_attributes}

let include_infos sub f x =
  let incl_loc = sub.location sub x.incl_loc in
  let incl_attributes = sub.attributes sub x.incl_attributes in
  {x with incl_loc; incl_attributes; incl_mod = f x.incl_mod}

let class_type_declaration sub x =
  class_infos sub (sub.class_type sub) x

let class_declaration sub x =
  class_infos sub (sub.class_expr sub) x

let structure_item sub {str_loc; str_desc; str_env} =
  let str_loc = sub.location sub str_loc in
  let str_env = sub.env sub str_env in
  let str_desc =
    match str_desc with
    | Tstr_eval (exp, attrs) ->
        Tstr_eval (sub.expr sub exp, sub.attributes sub attrs)
    | Tstr_value (rec_flag, list) ->
        let (rec_flag, list) = sub.value_bindings sub (rec_flag, list) in
        Tstr_value (rec_flag, list)
    | Tstr_primitive v -> Tstr_primitive (sub.value_description sub v)
    | Tstr_type (rec_flag, list) ->
        let (rec_flag, list) = sub.type_declarations sub (rec_flag, list) in
        Tstr_type (rec_flag, list)
    | Tstr_typext te -> Tstr_typext (sub.type_extension sub te)
    | Tstr_exception ext -> Tstr_exception (sub.type_exception sub ext)
    | Tstr_module mb -> Tstr_module (sub.module_binding sub mb)
    | Tstr_recmodule list ->
        Tstr_recmodule (List.map (sub.module_binding sub) list)
    | Tstr_modtype x -> Tstr_modtype (sub.module_type_declaration sub x)
    | Tstr_class list ->
        Tstr_class
          (List.map (tuple2 (sub.class_declaration sub) id) list)
    | Tstr_class_type list ->
        Tstr_class_type
          (List.map (tuple3
            id (map_loc sub) (sub.class_type_declaration sub)) list)
    | Tstr_include incl ->
        Tstr_include (include_infos sub (sub.module_expr sub) incl)
    | Tstr_open od -> Tstr_open (sub.open_declaration sub od)
    | Tstr_attribute attr -> Tstr_attribute (sub.attribute sub attr)
  in
  {str_desc; str_env; str_loc}

let value_description sub x =
  let val_loc = sub.location sub x.val_loc in
  let val_name = map_loc sub x.val_name in
  let val_desc = sub.typ sub x.val_desc in
  let val_attributes = sub.attributes sub x.val_attributes in
  {x with val_loc; val_name; val_desc; val_attributes}

let label_decl sub x =
  let ld_loc = sub.location sub x.ld_loc in
  let ld_name = map_loc sub x.ld_name in
  let ld_type = sub.typ sub x.ld_type in
  let ld_attributes = sub.attributes sub x.ld_attributes in
  {x with ld_loc; ld_name; ld_type; ld_attributes}

let constructor_args sub = function
  | Cstr_tuple l -> Cstr_tuple (List.map (sub.typ sub) l)
  | Cstr_record l -> Cstr_record (List.map (label_decl sub) l)

let constructor_decl sub cd =
  let cd_loc = sub.location sub cd.cd_loc in
  let cd_name = map_loc sub cd.cd_name in
  let cd_vars = List.map (map_loc sub) cd.cd_vars in
  let cd_args = constructor_args sub cd.cd_args in
  let cd_res = Option.map (sub.typ sub) cd.cd_res in
  let cd_attributes = sub.attributes sub cd.cd_attributes in
  {cd with cd_loc; cd_name; cd_vars; cd_args; cd_res; cd_attributes}

let type_kind sub = function
  | Ttype_abstract -> Ttype_abstract
  | Ttype_variant list -> Ttype_variant (List.map (constructor_decl sub) list)
  | Ttype_record list -> Ttype_record (List.map (label_decl sub) list)
  | Ttype_open -> Ttype_open

let type_declaration sub x =
  let typ_loc = sub.location sub x.typ_loc in
  let typ_name = map_loc sub x.typ_name in
  let typ_cstrs =
    List.map
      (tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
      x.typ_cstrs
  in
  let typ_kind = sub.type_kind sub x.typ_kind in
  let typ_manifest = Option.map (sub.typ sub) x.typ_manifest in
  let typ_params = List.map (tuple2 (sub.typ sub) id) x.typ_params in
  let typ_attributes = sub.attributes sub x.typ_attributes in
  {x with typ_loc; typ_name; typ_cstrs; typ_kind; typ_manifest; typ_params;
          typ_attributes}

let type_declarations sub (rec_flag, list) =
  (rec_flag, List.map (sub.type_declaration sub) list)

let type_extension sub x =
  let tyext_loc = sub.location sub x.tyext_loc in
  let tyext_txt = map_loc sub x.tyext_txt in
  let tyext_params = List.map (tuple2 (sub.typ sub) id) x.tyext_params in
  let tyext_constructors =
    List.map (sub.extension_constructor sub) x.tyext_constructors
  in
  let tyext_attributes = sub.attributes sub x.tyext_attributes in
  {x with tyext_loc; tyext_txt; tyext_constructors; tyext_params;
          tyext_attributes}

let type_exception sub x =
  let tyexn_loc = sub.location sub x.tyexn_loc in
  let tyexn_constructor =
    sub.extension_constructor sub x.tyexn_constructor
  in
  let tyexn_attributes = sub.attributes sub x.tyexn_attributes in
  {tyexn_loc; tyexn_constructor; tyexn_attributes}

let extension_constructor sub x =
  let ext_loc = sub.location sub x.ext_loc in
  let ext_name = map_loc sub x.ext_name in
  let ext_kind =
    match x.ext_kind with
      Text_decl(ids, ctl, cto) ->
        Text_decl(
          List.map (map_loc sub) ids,
          constructor_args sub ctl,
          Option.map (sub.typ sub) cto
        )
    | Text_rebind (path, lid) ->
        Text_rebind (path, map_loc sub lid)
  in
  let ext_attributes = sub.attributes sub x.ext_attributes in
  {x with ext_loc; ext_name; ext_kind; ext_attributes}

let pat_extra sub = function
  | Tpat_unpack as d -> d
  | Tpat_type (path,loc) -> Tpat_type (path, map_loc sub loc)
  | Tpat_open (path,loc,env) ->
      Tpat_open (path, map_loc sub loc, sub.env sub env)
  | Tpat_constraint ct -> Tpat_constraint (sub.typ sub ct)

let pat
  : type k . mapper -> k general_pattern -> k general_pattern
  = fun sub x ->
  let pat_loc = sub.location sub x.pat_loc in
  let pat_env = sub.env sub x.pat_env in
  let pat_extra =
    List.map (tuple3 (pat_extra sub) id (sub.attributes sub)) x.pat_extra in
  let pat_desc : k pattern_desc =
    match x.pat_desc with
    | Tpat_any
    | Tpat_constant _ -> x.pat_desc
    | Tpat_var (id, s) -> Tpat_var (id, map_loc sub s)
    | Tpat_tuple l -> Tpat_tuple (List.map (sub.pat sub) l)
    | Tpat_construct (loc, cd, l, vto) ->
        let vto = Option.map (fun (vl,cty) ->
          List.map (map_loc sub) vl, sub.typ sub cty) vto in
        Tpat_construct (map_loc sub loc, cd, List.map (sub.pat sub) l, vto)
    | Tpat_variant (l, po, rd) ->
        Tpat_variant (l, Option.map (sub.pat sub) po, rd)
    | Tpat_record (l, closed) ->
        Tpat_record (List.map (tuple3 (map_loc sub) id (sub.pat sub)) l, closed)
    | Tpat_array l -> Tpat_array (List.map (sub.pat sub) l)
    | Tpat_alias (p, id, s) -> Tpat_alias (sub.pat sub p, id, map_loc sub s)
    | Tpat_lazy p -> Tpat_lazy (sub.pat sub p)
    | Tpat_value p ->
       (as_computation_pattern (sub.pat sub (p :> pattern))).pat_desc
    | Tpat_exception p ->
       Tpat_exception (sub.pat sub p)
    | Tpat_or (p1, p2, rd) ->
        Tpat_or (sub.pat sub p1, sub.pat sub p2, rd)
  in
  let pat_attributes = sub.attributes sub x.pat_attributes in
  {x with pat_loc; pat_extra; pat_desc; pat_env; pat_attributes}

let expr sub x =
  let extra = function
    | Texp_constraint cty ->
        Texp_constraint (sub.typ sub cty)
    | Texp_coerce (cty1, cty2) ->
        Texp_coerce (Option.map (sub.typ sub) cty1, sub.typ sub cty2)
    | Texp_newtype _ as d -> d
    | Texp_poly cto -> Texp_poly (Option.map (sub.typ sub) cto)
  in
  let exp_loc = sub.location sub x.exp_loc in
  let exp_extra = List.map (tuple3 extra (sub.location sub) id) x.exp_extra in
  let exp_env = sub.env sub x.exp_env in
  let exp_desc =
    match x.exp_desc with
    | Texp_ident (path, lid, vd) ->
        Texp_ident (path, map_loc sub lid, vd)
    | Texp_constant _ as d -> d
    | Texp_let (rec_flag, list, exp) ->
        let (rec_flag, list) = sub.value_bindings sub (rec_flag, list) in
        Texp_let (rec_flag, list, sub.expr sub exp)
    | Texp_function { arg_label; param; cases; partial; } ->
        let cases = List.map (sub.case sub) cases in
        Texp_function { arg_label; param; cases; partial; }
    | Texp_apply (exp, list) ->
        Texp_apply (
          sub.expr sub exp,
          List.map (tuple2 id (Option.map (sub.expr sub))) list
        )
    | Texp_match (exp, cases, p) ->
        Texp_match (
          sub.expr sub exp,
          List.map (sub.case sub) cases,
          p
        )
    | Texp_try (exp, cases) ->
        Texp_try (
          sub.expr sub exp,
          List.map (sub.case sub) cases
        )
    | Texp_tuple list ->
        Texp_tuple (List.map (sub.expr sub) list)
    | Texp_construct (lid, cd, args) ->
        Texp_construct (map_loc sub lid, cd, List.map (sub.expr sub) args)
    | Texp_variant (l, expo) ->
        Texp_variant (l, Option.map (sub.expr sub) expo)
    | Texp_record { fields; representation; extended_expression } ->
        let fields = Array.map (function
            | label, Kept (t, mut) -> label, Kept (t, mut)
            | label, Overridden (lid, exp) ->
                label, Overridden (map_loc sub lid, sub.expr sub exp))
            fields
        in
        Texp_record {
          fields; representation;
          extended_expression = Option.map (sub.expr sub) extended_expression;
        }
    | Texp_field (exp, lid, ld) ->
        Texp_field (sub.expr sub exp, map_loc sub lid, ld)
    | Texp_setfield (exp1, lid, ld, exp2) ->
        Texp_setfield (
          sub.expr sub exp1,
          map_loc sub lid,
          ld,
          sub.expr sub exp2
        )
    | Texp_array list ->
        Texp_array (List.map (sub.expr sub) list)
    | Texp_ifthenelse (exp1, exp2, expo) ->
        Texp_ifthenelse (
          sub.expr sub exp1,
          sub.expr sub exp2,
          Option.map (sub.expr sub) expo
        )
    | Texp_sequence (exp1, exp2) ->
        Texp_sequence (
          sub.expr sub exp1,
          sub.expr sub exp2
        )
    | Texp_while (exp1, exp2) ->
        Texp_while (
          sub.expr sub exp1,
          sub.expr sub exp2
        )
    | Texp_for (id, p, exp1, exp2, dir, exp3) ->
        Texp_for (
          id,
          p,
          sub.expr sub exp1,
          sub.expr sub exp2,
          dir,
          sub.expr sub exp3
        )
    | Texp_send (exp, meth) ->
        Texp_send
          (
            sub.expr sub exp,
            meth
          )
    | Texp_new (path, lid, cd) ->
        Texp_new (
          path,
          map_loc sub lid,
          cd
        )
    | Texp_instvar (path1, path2, id) ->
        Texp_instvar (
          path1,
          path2,
          map_loc sub id
        )
    | Texp_setinstvar (path1, path2, id, exp) ->
        Texp_setinstvar (
          path1,
          path2,
          map_loc sub id,
          sub.expr sub exp
        )
    | Texp_override (path, list) ->
        Texp_override (
          path,
          List.map (tuple3 id (map_loc sub) (sub.expr sub)) list
        )
    | Texp_letmodule (id, s, pres, mexpr, exp) ->
        Texp_letmodule (
          id,
          map_loc sub s,
          pres,
          sub.module_expr sub mexpr,
          sub.expr sub exp
        )
    | Texp_letexception (cd, exp) ->
        Texp_letexception (
          sub.extension_constructor sub cd,
          sub.expr sub exp
        )
    | Texp_assert (exp, loc) ->
        Texp_assert (sub.expr sub exp, loc)
    | Texp_lazy exp ->
        Texp_lazy (sub.expr sub exp)
    | Texp_object (cl, sl) ->
        Texp_object (sub.class_structure sub cl, sl)
    | Texp_pack mexpr ->
        Texp_pack (sub.module_expr sub mexpr)
    | Texp_letop {let_; ands; param; body; partial} ->
        Texp_letop{
          let_ = sub.binding_op sub let_;
          ands = List.map (sub.binding_op sub) ands;
          param;
          body = sub.case sub body;
          partial;
        }
    | Texp_unreachable ->
        Texp_unreachable
    | Texp_extension_constructor (lid, path) ->
        Texp_extension_constructor (map_loc sub lid, path)
    | Texp_open (od, e) ->
        Texp_open (sub.open_declaration sub od, sub.expr sub e)
  in
  let exp_attributes = sub.attributes sub x.exp_attributes in
  {x with exp_loc; exp_extra; exp_desc; exp_env; exp_attributes}


let package_type sub x =
  let pack_txt = map_loc sub x.pack_txt in
  let pack_fields = List.map
    (tuple2 (map_loc sub) (sub.typ sub)) x.pack_fields in
  {x with pack_txt; pack_fields}

let binding_op sub x =
  let bop_loc = sub.location sub x.bop_loc in
  let bop_op_name = map_loc sub x.bop_op_name in
  { x with bop_loc; bop_op_name; bop_exp = sub.expr sub x.bop_exp }

let signature sub x =
  let sig_final_env = sub.env sub x.sig_final_env in
  let sig_items = List.map (sub.signature_item sub) x.sig_items in
  {x with sig_items; sig_final_env}

let signature_item sub x =
  let sig_loc = sub.location sub x.sig_loc in
  let sig_env = sub.env sub x.sig_env in
  let sig_desc =
    match x.sig_desc with
    | Tsig_value v ->
        Tsig_value (sub.value_description sub v)
    | Tsig_type (rec_flag, list) ->
        let (rec_flag, list) = sub.type_declarations sub (rec_flag, list) in
        Tsig_type (rec_flag, list)
    | Tsig_typesubst list ->
        let (_, list) = sub.type_declarations sub (Nonrecursive, list) in
        Tsig_typesubst list
    | Tsig_typext te ->
        Tsig_typext (sub.type_extension sub te)
    | Tsig_exception ext ->
        Tsig_exception (sub.type_exception sub ext)
    | Tsig_module x ->
        Tsig_module (sub.module_declaration sub x)
    | Tsig_modsubst x ->
        Tsig_modsubst (sub.module_substitution sub x)
    | Tsig_recmodule list ->
        Tsig_recmodule (List.map (sub.module_declaration sub) list)
    | Tsig_modtype x ->
        Tsig_modtype (sub.module_type_declaration sub x)
   | Tsig_modtypesubst x ->
        Tsig_modtypesubst (sub.module_type_declaration sub x)
   | Tsig_include incl ->
        Tsig_include (include_infos sub (sub.module_type sub) incl)
    | Tsig_class list ->
        Tsig_class (List.map (sub.class_description sub) list)
    | Tsig_class_type list ->
        Tsig_class_type
          (List.map (sub.class_type_declaration sub) list)
    | Tsig_open od -> Tsig_open (sub.open_description sub od)
    | Tsig_attribute attr -> Tsig_attribute (sub.attribute sub attr)
  in
  {sig_loc; sig_desc; sig_env}

let class_description sub x =
  class_infos sub (sub.class_type sub) x

let functor_parameter sub = function
  | Unit -> Unit
  | Named (id, s, mtype) -> Named (id, map_loc sub s, sub.module_type sub mtype)

let module_type sub x =
  let mty_loc = sub.location sub x.mty_loc in
  let mty_env = sub.env sub x.mty_env in
  let mty_desc =
    match x.mty_desc with
    | Tmty_ident (path, lid) -> Tmty_ident (path, map_loc sub lid)
    | Tmty_alias (path, lid) -> Tmty_alias (path, map_loc sub lid)
    | Tmty_signature sg -> Tmty_signature (sub.signature sub sg)
    | Tmty_functor (arg, mtype2) ->
        Tmty_functor (functor_parameter sub arg, sub.module_type sub mtype2)
    | Tmty_with (mtype, list) ->
        Tmty_with (
          sub.module_type sub mtype,
          List.map (tuple3 id (map_loc sub) (sub.with_constraint sub)) list
        )
    | Tmty_typeof mexpr ->
        Tmty_typeof (sub.module_expr sub mexpr)
  in
  let mty_attributes = sub.attributes sub x.mty_attributes in
  {x with mty_loc; mty_desc; mty_env; mty_attributes}

let with_constraint sub = function
  | Twith_type decl -> Twith_type (sub.type_declaration sub decl)
  | Twith_typesubst decl -> Twith_typesubst (sub.type_declaration sub decl)
  | Twith_modtype mty -> Twith_modtype (sub.module_type sub mty)
  | Twith_modtypesubst mty -> Twith_modtypesubst (sub.module_type sub mty)
  | Twith_module (path, lid) -> Twith_module (path, map_loc sub lid)
  | Twith_modsubst (path, lid) -> Twith_modsubst (path, map_loc sub lid)

let open_description sub od =
  {od with open_loc = sub.location sub od.open_loc;
           open_expr = tuple2 id (map_loc sub) od.open_expr;
           open_env = sub.env sub od.open_env;
           open_attributes = sub.attributes sub od.open_attributes}

let open_declaration sub od =
  {od with open_loc = sub.location sub od.open_loc;
           open_expr = sub.module_expr sub od.open_expr;
           open_env = sub.env sub od.open_env;
           open_attributes = sub.attributes sub od.open_attributes}

let module_coercion sub = function
  | Tcoerce_none -> Tcoerce_none
  | Tcoerce_functor (c1,c2) ->
      Tcoerce_functor (sub.module_coercion sub c1, sub.module_coercion sub c2)
  | Tcoerce_alias (env, p, c1) ->
      Tcoerce_alias (sub.env sub env, p, sub.module_coercion sub c1)
  | Tcoerce_structure (l1, l2) ->
      let l1' = List.map (fun (i,c) -> i, sub.module_coercion sub c) l1 in
      let l2' =
        List.map (fun (id,i,c) -> id, i, sub.module_coercion sub c) l2
      in
      Tcoerce_structure (l1', l2')
  | Tcoerce_primitive pc ->
      Tcoerce_primitive {pc with pc_loc = sub.location sub pc.pc_loc;
                                 pc_env = sub.env sub pc.pc_env}

let module_expr sub x =
  let mod_loc = sub.location sub x.mod_loc in
  let mod_env = sub.env sub x.mod_env in
  let mod_desc =
    match x.mod_desc with
    | Tmod_ident (path, lid) -> Tmod_ident (path, map_loc sub lid)
    | Tmod_structure st -> Tmod_structure (sub.structure sub st)
    | Tmod_functor (arg, mexpr) ->
        Tmod_functor (functor_parameter sub arg, sub.module_expr sub mexpr)
    | Tmod_apply (mexp1, mexp2, c) ->
        Tmod_apply (
          sub.module_expr sub mexp1,
          sub.module_expr sub mexp2,
          sub.module_coercion sub c
        )
    | Tmod_constraint (mexpr, mt, Tmodtype_implicit, c) ->
        Tmod_constraint (sub.module_expr sub mexpr, mt, Tmodtype_implicit,
                         sub.module_coercion sub c)
    | Tmod_constraint (mexpr, mt, Tmodtype_explicit mtype, c) ->
        Tmod_constraint (
          sub.module_expr sub mexpr,
          mt,
          Tmodtype_explicit (sub.module_type sub mtype),
          sub.module_coercion sub c
        )
    | Tmod_unpack (exp, mty) ->
        Tmod_unpack
          (
            sub.expr sub exp,
            mty
          )
  in
  let mod_attributes = sub.attributes sub x.mod_attributes in
  {x with mod_loc; mod_desc; mod_env; mod_attributes}

let module_binding sub x =
  let mb_loc = sub.location sub x.mb_loc in
  let mb_name = map_loc sub x.mb_name in
  let mb_expr = sub.module_expr sub x.mb_expr in
  let mb_attributes = sub.attributes sub x.mb_attributes in
  {x with mb_loc; mb_name; mb_expr; mb_attributes}

let class_expr sub x =
  let cl_loc = sub.location sub x.cl_loc in
  let cl_env = sub.env sub x.cl_env in
  let cl_desc =
    match x.cl_desc with
    | Tcl_constraint (cl, clty, vals, meths, concrs) ->
        Tcl_constraint (
          sub.class_expr sub cl,
          Option.map (sub.class_type sub) clty,
          vals,
          meths,
          concrs
        )
    | Tcl_structure clstr ->
        Tcl_structure (sub.class_structure sub clstr)
    | Tcl_fun (label, pat, priv, cl, partial) ->
        Tcl_fun (
          label,
          sub.pat sub pat,
          List.map (tuple2 id (sub.expr sub)) priv,
          sub.class_expr sub cl,
          partial
        )
    | Tcl_apply (cl, args) ->
        Tcl_apply (
          sub.class_expr sub cl,
          List.map (tuple2 id (Option.map (sub.expr sub))) args
        )
    | Tcl_let (rec_flag, value_bindings, ivars, cl) ->
        let (rec_flag, value_bindings) =
          sub.value_bindings sub (rec_flag, value_bindings)
        in
        Tcl_let (
          rec_flag,
          value_bindings,
          List.map (tuple2 id (sub.expr sub)) ivars,
          sub.class_expr sub cl
        )
    | Tcl_ident (path, lid, tyl) ->
        Tcl_ident (path, map_loc sub lid, List.map (sub.typ sub) tyl)
    | Tcl_open (od, e) ->
        Tcl_open (sub.open_description sub od, sub.class_expr sub e)
  in
  let cl_attributes = sub.attributes sub x.cl_attributes in
  {x with cl_loc; cl_desc; cl_env; cl_attributes}

let class_type sub x =
  let cltyp_loc = sub.location sub x.cltyp_loc in
  let cltyp_env = sub.env sub x.cltyp_env in
  let cltyp_desc =
    match x.cltyp_desc with
    | Tcty_signature csg -> Tcty_signature (sub.class_signature sub csg)
    | Tcty_constr (path, lid, list) ->
        Tcty_constr (
          path,
          map_loc sub lid,
          List.map (sub.typ sub) list
        )
    | Tcty_arrow (label, ct, cl) ->
        Tcty_arrow
          (label,
           sub.typ sub ct,
           sub.class_type sub cl
          )
    | Tcty_open (od, e) ->
        Tcty_open (sub.open_description sub od, sub.class_type sub e)
  in
  let cltyp_attributes = sub.attributes sub x.cltyp_attributes in
  {x with cltyp_loc; cltyp_desc; cltyp_env; cltyp_attributes}

let class_signature sub x =
  let csig_self = sub.typ sub x.csig_self in
  let csig_fields = List.map (sub.class_type_field sub) x.csig_fields in
  {x with csig_self; csig_fields}

let class_type_field sub x =
  let ctf_loc = sub.location sub x.ctf_loc in
  let ctf_desc =
    match x.ctf_desc with
    | Tctf_inherit ct ->
        Tctf_inherit (sub.class_type sub ct)
    | Tctf_val (s, mut, virt, ct) ->
        Tctf_val (s, mut, virt, sub.typ sub ct)
    | Tctf_method (s, priv, virt, ct) ->
        Tctf_method (s, priv, virt, sub.typ sub ct)
    | Tctf_constraint  (ct1, ct2) ->
        Tctf_constraint (sub.typ sub ct1, sub.typ sub ct2)
    | Tctf_attribute attr ->
        Tctf_attribute (sub.attribute sub attr)
  in
  let ctf_attributes = sub.attributes sub x.ctf_attributes in
  {ctf_loc; ctf_desc; ctf_attributes}

let typ sub x =
  let ctyp_loc = sub.location sub x.ctyp_loc in
  let ctyp_env = sub.env sub x.ctyp_env in
  let ctyp_desc =
    match x.ctyp_desc with
    | Ttyp_any
    | Ttyp_var _ as d -> d
    | Ttyp_arrow (label, ct1, ct2) ->
        Ttyp_arrow (label, sub.typ sub ct1, sub.typ sub ct2)
    | Ttyp_tuple list -> Ttyp_tuple (List.map (sub.typ sub) list)
    | Ttyp_constr (path, lid, list) ->
        Ttyp_constr (path, map_loc sub lid, List.map (sub.typ sub) list)
    | Ttyp_object (list, closed) ->
        Ttyp_object ((List.map (sub.object_field sub) list), closed)
    | Ttyp_class (path, lid, list) ->
        Ttyp_class
          (path,
           map_loc sub lid,
           List.map (sub.typ sub) list
          )
    | Ttyp_alias (ct, s) ->
        Ttyp_alias (sub.typ sub ct, s)
    | Ttyp_variant (list, closed, labels) ->
        Ttyp_variant (List.map (sub.row_field sub) list, closed, labels)
    | Ttyp_poly (sl, ct) ->
        Ttyp_poly (sl, sub.typ sub ct)
    | Ttyp_package pack ->
        Ttyp_package (sub.package_type sub pack)
  in
  let ctyp_attributes = sub.attributes sub x.ctyp_attributes in
  {x with ctyp_loc; ctyp_desc; ctyp_env; ctyp_attributes}

let class_structure sub x =
  let cstr_self = sub.pat sub x.cstr_self in
  let cstr_fields = List.map (sub.class_field sub) x.cstr_fields in
  {x with cstr_self; cstr_fields}

let row_field sub x =
  let rf_loc = sub.location sub x.rf_loc in
  let rf_desc = match x.rf_desc with
    | Ttag (label, b, list) ->
        Ttag (map_loc sub label, b, List.map (sub.typ sub) list)
    | Tinherit ct -> Tinherit (sub.typ sub ct)
  in
  let rf_attributes = sub.attributes sub x.rf_attributes in
  {rf_loc; rf_desc; rf_attributes}

let object_field sub x =
  let of_loc = sub.location sub x.of_loc in
  let of_desc = match x.of_desc with
    | OTtag (label, ct) ->
        OTtag (map_loc sub label, (sub.typ sub ct))
    | OTinherit ct -> OTinherit (sub.typ sub ct)
  in
  let of_attributes = sub.attributes sub x.of_attributes in
  {of_loc; of_desc; of_attributes}

let class_field_kind sub = function
  | Tcfk_virtual ct -> Tcfk_virtual (sub.typ sub ct)
  | Tcfk_concrete (ovf, e) -> Tcfk_concrete (ovf, sub.expr sub e)

let class_field sub x =
  let cf_loc = sub.location sub x.cf_loc in
  let cf_desc =
    match x.cf_desc with
    | Tcf_inherit (ovf, cl, super, vals, meths) ->
        Tcf_inherit (ovf, sub.class_expr sub cl, super, vals, meths)
    | Tcf_constraint (cty, cty') ->
        Tcf_constraint (
          sub.typ sub cty,
          sub.typ sub cty'
        )
    | Tcf_val (s, mf, id, k, b) ->
        Tcf_val (map_loc sub s, mf, id, class_field_kind sub k, b)
    | Tcf_method (s, priv, k) ->
        Tcf_method (map_loc sub s, priv, class_field_kind sub k)
    | Tcf_initializer exp ->
        Tcf_initializer (sub.expr sub exp)
    | Tcf_attribute attr ->
        Tcf_attribute (sub.attribute sub attr)
  in
  let cf_attributes = sub.attributes sub x.cf_attributes in
  {cf_loc; cf_desc; cf_attributes}

let value_bindings sub (rec_flag, list) =
  (rec_flag, List.map (sub.value_binding sub) list)

let case
  : type k . mapper -> k case -> k case
  = fun sub {c_lhs; c_guard; c_rhs} ->
  {
    c_lhs = sub.pat sub c_lhs;
    c_guard = Option.map (sub.expr sub) c_guard;
    c_rhs = sub.expr sub c_rhs;
  }

let value_binding sub x =
  let vb_loc = sub.location sub x.vb_loc in
  let vb_pat = sub.pat sub x.vb_pat in
  let vb_expr = sub.expr sub x.vb_expr in
  let vb_attributes = sub.attributes sub x.vb_attributes in
  {vb_loc; vb_pat; vb_expr; vb_attributes}

let env _sub x = x

let default =
  {
    attribute;
    attributes;
    binding_op;
    case;
    class_declaration;
    class_description;
    class_expr;
    class_field;
    class_signature;
    class_structure;
    class_type;
    class_type_declaration;
    class_type_field;
    env;
    expr;
    extension_constructor;
    location;
    module_binding;
    module_coercion;
    module_declaration;
    module_substitution;
    module_expr;
    module_type;
    module_type_declaration;
    package_type;
    pat;
    row_field;
    object_field;
    open_declaration;
    open_description;
    signature;
    signature_item;
    structure;
    structure_item;
    typ;
    type_declaration;
    type_declarations;
    type_extension;
    type_exception;
    type_kind;
    value_binding;
    value_bindings;
    value_description;
    with_constraint;
  }
