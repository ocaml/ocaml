(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Nicolas Ojeda Bar, LexiFi                         *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* A generic Parsetree mapping class *)

(*
[@@@ocaml.warning "+9"]
  (* Ensure that record patterns don't miss any field. *)
*)


open Parsetree
open Location

type iterator = {
  attribute: iterator -> attribute -> unit;
  attributes: iterator -> attribute list -> unit;
  binding_op: iterator -> binding_op -> unit;
  case: iterator -> case -> unit;
  cases: iterator -> case list -> unit;
  class_declaration: iterator -> class_declaration -> unit;
  class_description: iterator -> class_description -> unit;
  class_expr: iterator -> class_expr -> unit;
  class_field: iterator -> class_field -> unit;
  class_signature: iterator -> class_signature -> unit;
  class_structure: iterator -> class_structure -> unit;
  class_type: iterator -> class_type -> unit;
  class_type_declaration: iterator -> class_type_declaration -> unit;
  class_type_field: iterator -> class_type_field -> unit;
  constructor_declaration: iterator -> constructor_declaration -> unit;
  expr: iterator -> expression -> unit;
  extension: iterator -> extension -> unit;
  extension_constructor: iterator -> extension_constructor -> unit;
  include_declaration: iterator -> include_declaration -> unit;
  include_description: iterator -> include_description -> unit;
  label_declaration: iterator -> label_declaration -> unit;
  location: iterator -> Location.t -> unit;
  module_binding: iterator -> module_binding -> unit;
  module_declaration: iterator -> module_declaration -> unit;
  module_substitution: iterator -> module_substitution -> unit;
  module_expr: iterator -> module_expr -> unit;
  module_type: iterator -> module_type -> unit;
  module_type_declaration: iterator -> module_type_declaration -> unit;
  open_declaration: iterator -> open_declaration -> unit;
  open_description: iterator -> open_description -> unit;
  pat: iterator -> pattern -> unit;
  payload: iterator -> payload -> unit;
  signature: iterator -> signature -> unit;
  signature_item: iterator -> signature_item -> unit;
  structure: iterator -> structure -> unit;
  structure_item: iterator -> structure_item -> unit;
  typ: iterator -> core_type -> unit;
  row_field: iterator -> row_field -> unit;
  object_field: iterator -> object_field -> unit;
  type_declaration: iterator -> type_declaration -> unit;
  type_extension: iterator -> type_extension -> unit;
  type_exception: iterator -> type_exception -> unit;
  type_kind: iterator -> type_kind -> unit;
  value_binding: iterator -> value_binding -> unit;
  value_description: iterator -> value_description -> unit;
  with_constraint: iterator -> with_constraint -> unit;
}
(** A [iterator] record implements one "method" per syntactic category,
    using an open recursion style: each method takes as its first
    argument the iterator to be applied to children in the syntax
    tree. *)

let iter_fst f (x, _) = f x
let iter_snd f (_, y) = f y
let iter_tuple f1 f2 (x, y) = f1 x; f2 y
let iter_tuple3 f1 f2 f3 (x, y, z) = f1 x; f2 y; f3 z
let iter_opt f = function None -> () | Some x -> f x

let iter_loc sub {loc; txt = _} = sub.location sub loc

module T = struct
  (* Type expressions for the core language *)

  let row_field sub {
      prf_desc;
      prf_loc;
      prf_attributes;
    } =
    sub.location sub prf_loc;
    sub.attributes sub prf_attributes;
    match prf_desc with
    | Rtag (_, _, tl) -> List.iter (sub.typ sub) tl
    | Rinherit t -> sub.typ sub t

  let object_field sub {
      pof_desc;
      pof_loc;
      pof_attributes;
    } =
    sub.location sub pof_loc;
    sub.attributes sub pof_attributes;
    match pof_desc with
    | Otag (_, t) -> sub.typ sub t
    | Oinherit t -> sub.typ sub t

  let iter sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Ptyp_any
    | Ptyp_var _ -> ()
    | Ptyp_arrow (_lab, t1, t2) ->
        sub.typ sub t1; sub.typ sub t2
    | Ptyp_tuple tyl -> List.iter (sub.typ sub) tyl
    | Ptyp_constr (lid, tl) ->
        iter_loc sub lid; List.iter (sub.typ sub) tl
    | Ptyp_object (ol, _o) ->
        List.iter (object_field sub) ol
    | Ptyp_class (lid, tl) ->
        iter_loc sub lid; List.iter (sub.typ sub) tl
    | Ptyp_alias (t, _) -> sub.typ sub t
    | Ptyp_variant (rl, _b, _ll) ->
        List.iter (row_field sub) rl
    | Ptyp_poly (_, t) -> sub.typ sub t
    | Ptyp_package (lid, l) ->
        iter_loc sub lid;
        List.iter (iter_tuple (iter_loc sub) (sub.typ sub)) l
    | Ptyp_extension x -> sub.extension sub x

  let iter_type_declaration sub
      {ptype_name; ptype_params; ptype_cstrs;
       ptype_kind;
       ptype_private = _;
       ptype_manifest;
       ptype_attributes;
       ptype_loc} =
    iter_loc sub ptype_name;
    List.iter (iter_fst (sub.typ sub)) ptype_params;
    List.iter
      (iter_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
      ptype_cstrs;
    sub.type_kind sub ptype_kind;
    iter_opt (sub.typ sub) ptype_manifest;
    sub.location sub ptype_loc;
    sub.attributes sub ptype_attributes

  let iter_type_kind sub = function
    | Ptype_abstract -> ()
    | Ptype_variant l ->
        List.iter (sub.constructor_declaration sub) l
    | Ptype_record l -> List.iter (sub.label_declaration sub) l
    | Ptype_open -> ()

  let iter_constructor_arguments sub = function
    | Pcstr_tuple l -> List.iter (sub.typ sub) l
    | Pcstr_record l ->
        List.iter (sub.label_declaration sub) l

  let iter_type_extension sub
      {ptyext_path; ptyext_params;
       ptyext_constructors;
       ptyext_private = _;
       ptyext_loc;
       ptyext_attributes} =
    iter_loc sub ptyext_path;
    List.iter (sub.extension_constructor sub) ptyext_constructors;
    List.iter (iter_fst (sub.typ sub)) ptyext_params;
    sub.location sub ptyext_loc;
    sub.attributes sub ptyext_attributes

  let iter_type_exception sub
      {ptyexn_constructor; ptyexn_loc; ptyexn_attributes} =
    sub.extension_constructor sub ptyexn_constructor;
    sub.location sub ptyexn_loc;
    sub.attributes sub ptyexn_attributes

  let iter_extension_constructor_kind sub = function
      Pext_decl(ctl, cto) ->
        iter_constructor_arguments sub ctl; iter_opt (sub.typ sub) cto
    | Pext_rebind li ->
        iter_loc sub li

  let iter_extension_constructor sub
      {pext_name;
       pext_kind;
       pext_loc;
       pext_attributes} =
    iter_loc sub pext_name;
    iter_extension_constructor_kind sub pext_kind;
    sub.location sub pext_loc;
    sub.attributes sub pext_attributes

end

module CT = struct
  (* Type expressions for the class language *)

  let iter sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pcty_constr (lid, tys) ->
        iter_loc sub lid; List.iter (sub.typ sub) tys
    | Pcty_signature x -> sub.class_signature sub x
    | Pcty_arrow (_lab, t, ct) ->
        sub.typ sub t; sub.class_type sub ct
    | Pcty_extension x -> sub.extension sub x
    | Pcty_open (o, e) ->
        sub.open_description sub o; sub.class_type sub e

  let iter_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
    =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pctf_inherit ct -> sub.class_type sub ct
    | Pctf_val (_s, _m, _v, t) -> sub.typ sub t
    | Pctf_method (_s, _p, _v, t) -> sub.typ sub t
    | Pctf_constraint (t1, t2) ->
        sub.typ sub t1; sub.typ sub t2
    | Pctf_attribute x -> sub.attribute sub x
    | Pctf_extension x -> sub.extension sub x

  let iter_signature sub {pcsig_self; pcsig_fields} =
    sub.typ sub pcsig_self;
    List.iter (sub.class_type_field sub) pcsig_fields
end

module MT = struct
  (* Type expressions for the module language *)

  let iter sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pmty_ident s -> iter_loc sub s
    | Pmty_alias s -> iter_loc sub s
    | Pmty_signature sg -> sub.signature sub sg
    | Pmty_functor (s, mt1, mt2) ->
        iter_loc sub s;
        iter_opt (sub.module_type sub) mt1;
        sub.module_type sub mt2
    | Pmty_with (mt, l) ->
        sub.module_type sub mt;
        List.iter (sub.with_constraint sub) l
    | Pmty_typeof me -> sub.module_expr sub me
    | Pmty_extension x -> sub.extension sub x

  let iter_with_constraint sub = function
    | Pwith_type (lid, d) ->
        iter_loc sub lid; sub.type_declaration sub d
    | Pwith_module (lid, lid2) ->
        iter_loc sub lid; iter_loc sub lid2
    | Pwith_typesubst (lid, d) ->
        iter_loc sub lid; sub.type_declaration sub d
    | Pwith_modsubst (s, lid) ->
        iter_loc sub s; iter_loc sub lid

  let iter_signature_item sub {psig_desc = desc; psig_loc = loc} =
    sub.location sub loc;
    match desc with
    | Psig_value vd -> sub.value_description sub vd
    | Psig_type (_, l)
    | Psig_typesubst l ->
      List.iter (sub.type_declaration sub) l
    | Psig_typext te -> sub.type_extension sub te
    | Psig_exception ed -> sub.type_exception sub ed
    | Psig_module x -> sub.module_declaration sub x
    | Psig_modsubst x -> sub.module_substitution sub x
    | Psig_recmodule l ->
        List.iter (sub.module_declaration sub) l
    | Psig_modtype x -> sub.module_type_declaration sub x
    | Psig_open x -> sub.open_description sub x
    | Psig_include x -> sub.include_description sub x
    | Psig_class l -> List.iter (sub.class_description sub) l
    | Psig_class_type l ->
        List.iter (sub.class_type_declaration sub) l
    | Psig_extension (x, attrs) ->
        sub.attributes sub attrs;
        sub.extension sub x
    | Psig_attribute x -> sub.attribute sub x
end


module M = struct
  (* Value expressions for the module language *)

  let iter sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pmod_ident x -> iter_loc sub x
    | Pmod_structure str -> sub.structure sub str
    | Pmod_functor (arg, arg_ty, body) ->
        iter_loc sub arg;
        iter_opt (sub.module_type sub) arg_ty;
        sub.module_expr sub body
    | Pmod_apply (m1, m2) ->
        sub.module_expr sub m1; sub.module_expr sub m2
    | Pmod_constraint (m, mty) ->
        sub.module_expr sub m; sub.module_type sub mty
    | Pmod_unpack e -> sub.expr sub e
    | Pmod_extension x -> sub.extension sub x

  let iter_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
    sub.location sub loc;
    match desc with
    | Pstr_eval (x, attrs) ->
        sub.attributes sub attrs; sub.expr sub x
    | Pstr_value (_r, vbs) -> List.iter (sub.value_binding sub) vbs
    | Pstr_primitive vd -> sub.value_description sub vd
    | Pstr_type (_rf, l) -> List.iter (sub.type_declaration sub) l
    | Pstr_typext te -> sub.type_extension sub te
    | Pstr_exception ed -> sub.type_exception sub ed
    | Pstr_module x -> sub.module_binding sub x
    | Pstr_recmodule l -> List.iter (sub.module_binding sub) l
    | Pstr_modtype x -> sub.module_type_declaration sub x
    | Pstr_open x -> sub.open_declaration sub x
    | Pstr_class l -> List.iter (sub.class_declaration sub) l
    | Pstr_class_type l ->
        List.iter (sub.class_type_declaration sub) l
    | Pstr_include x -> sub.include_declaration sub x
    | Pstr_extension (x, attrs) ->
        sub.attributes sub attrs; sub.extension sub x
    | Pstr_attribute x -> sub.attribute sub x
end

module E = struct
  (* Value expressions for the core language *)

  let iter sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pexp_ident x -> iter_loc sub x
    | Pexp_constant _ -> ()
    | Pexp_let (_r, vbs, e) ->
        List.iter (sub.value_binding sub) vbs;
        sub.expr sub e
    | Pexp_fun (_lab, def, p, e) ->
        iter_opt (sub.expr sub) def;
        sub.pat sub p;
        sub.expr sub e
    | Pexp_function pel -> sub.cases sub pel
    | Pexp_apply (e, l) ->
        sub.expr sub e; List.iter (iter_snd (sub.expr sub)) l
    | Pexp_match (e, pel) ->
        sub.expr sub e; sub.cases sub pel
    | Pexp_try (e, pel) -> sub.expr sub e; sub.cases sub pel
    | Pexp_tuple el -> List.iter (sub.expr sub) el
    | Pexp_construct (lid, arg) ->
        iter_loc sub lid; iter_opt (sub.expr sub) arg
    | Pexp_variant (_lab, eo) ->
        iter_opt (sub.expr sub) eo
    | Pexp_record (l, eo) ->
        List.iter (iter_tuple (iter_loc sub) (sub.expr sub)) l;
        iter_opt (sub.expr sub) eo
    | Pexp_field (e, lid) ->
        sub.expr sub e; iter_loc sub lid
    | Pexp_setfield (e1, lid, e2) ->
        sub.expr sub e1; iter_loc sub lid;
        sub.expr sub e2
    | Pexp_array el -> List.iter (sub.expr sub) el
    | Pexp_ifthenelse (e1, e2, e3) ->
        sub.expr sub e1; sub.expr sub e2;
        iter_opt (sub.expr sub) e3
    | Pexp_sequence (e1, e2) ->
        sub.expr sub e1; sub.expr sub e2
    | Pexp_while (e1, e2) ->
        sub.expr sub e1; sub.expr sub e2
    | Pexp_for (p, e1, e2, _d, e3) ->
        sub.pat sub p; sub.expr sub e1; sub.expr sub e2;
        sub.expr sub e3
    | Pexp_coerce (e, t1, t2) ->
        sub.expr sub e; iter_opt (sub.typ sub) t1;
        sub.typ sub t2
    | Pexp_constraint (e, t) ->
        sub.expr sub e; sub.typ sub t
    | Pexp_send (e, _s) -> sub.expr sub e
    | Pexp_new lid -> iter_loc sub lid
    | Pexp_setinstvar (s, e) ->
        iter_loc sub s; sub.expr sub e
    | Pexp_override sel ->
        List.iter (iter_tuple (iter_loc sub) (sub.expr sub)) sel
    | Pexp_letmodule (s, me, e) ->
        iter_loc sub s; sub.module_expr sub me;
        sub.expr sub e
    | Pexp_letexception (cd, e) ->
        sub.extension_constructor sub cd;
        sub.expr sub e
    | Pexp_assert e -> sub.expr sub e
    | Pexp_lazy e -> sub.expr sub e
    | Pexp_poly (e, t) ->
        sub.expr sub e; iter_opt (sub.typ sub) t
    | Pexp_object cls -> sub.class_structure sub cls
    | Pexp_newtype (_s, e) -> sub.expr sub e
    | Pexp_pack me -> sub.module_expr sub me
    | Pexp_open (o, e) ->
        sub.open_declaration sub o; sub.expr sub e
    | Pexp_letop {let_; ands; body} ->
        sub.binding_op sub let_;
        List.iter (sub.binding_op sub) ands;
        sub.expr sub body
    | Pexp_extension x -> sub.extension sub x
    | Pexp_unreachable -> ()

  let iter_binding_op sub {pbop_op; pbop_pat; pbop_exp; pbop_loc} =
    iter_loc sub pbop_op;
    sub.pat sub pbop_pat;
    sub.expr sub pbop_exp;
    sub.location sub pbop_loc

end

module P = struct
  (* Patterns *)

  let iter sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Ppat_any -> ()
    | Ppat_var s -> iter_loc sub s
    | Ppat_alias (p, s) -> sub.pat sub p; iter_loc sub s
    | Ppat_constant _ -> ()
    | Ppat_interval _ -> ()
    | Ppat_tuple pl -> List.iter (sub.pat sub) pl
    | Ppat_construct (l, p) ->
        iter_loc sub l; iter_opt (sub.pat sub) p
    | Ppat_variant (_l, p) -> iter_opt (sub.pat sub) p
    | Ppat_record (lpl, _cf) ->
        List.iter (iter_tuple (iter_loc sub) (sub.pat sub)) lpl
    | Ppat_array pl -> List.iter (sub.pat sub) pl
    | Ppat_or (p1, p2) -> sub.pat sub p1; sub.pat sub p2
    | Ppat_constraint (p, t) ->
        sub.pat sub p; sub.typ sub t
    | Ppat_type s -> iter_loc sub s
    | Ppat_lazy p -> sub.pat sub p
    | Ppat_unpack s -> iter_loc sub s
    | Ppat_exception p -> sub.pat sub p
    | Ppat_extension x -> sub.extension sub x
    | Ppat_open (lid, p) ->
        iter_loc sub lid; sub.pat sub p

end

module CE = struct
  (* Value expressions for the class language *)

  let iter sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pcl_constr (lid, tys) ->
        iter_loc sub lid; List.iter (sub.typ sub) tys
    | Pcl_structure s ->
        sub.class_structure sub s
    | Pcl_fun (_lab, e, p, ce) ->
        iter_opt (sub.expr sub) e;
        sub.pat sub p;
        sub.class_expr sub ce
    | Pcl_apply (ce, l) ->
        sub.class_expr sub ce;
        List.iter (iter_snd (sub.expr sub)) l
    | Pcl_let (_r, vbs, ce) ->
        List.iter (sub.value_binding sub) vbs;
        sub.class_expr sub ce
    | Pcl_constraint (ce, ct) ->
        sub.class_expr sub ce; sub.class_type sub ct
    | Pcl_extension x -> sub.extension sub x
    | Pcl_open (o, e) ->
        sub.open_description sub o; sub.class_expr sub e

  let iter_kind sub = function
    | Cfk_concrete (_o, e) -> sub.expr sub e
    | Cfk_virtual t -> sub.typ sub t

  let iter_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pcf_inherit (_o, ce, _s) -> sub.class_expr sub ce
    | Pcf_val (s, _m, k) -> iter_loc sub s; iter_kind sub k
    | Pcf_method (s, _p, k) ->
        iter_loc sub s; iter_kind sub k
    | Pcf_constraint (t1, t2) ->
        sub.typ sub t1; sub.typ sub t2
    | Pcf_initializer e -> sub.expr sub e
    | Pcf_attribute x -> sub.attribute sub x
    | Pcf_extension x -> sub.extension sub x

  let iter_structure sub {pcstr_self; pcstr_fields} =
    sub.pat sub pcstr_self;
    List.iter (sub.class_field sub) pcstr_fields

  let class_infos sub f {pci_virt = _; pci_params = pl; pci_name; pci_expr;
                         pci_loc; pci_attributes} =
    List.iter (iter_fst (sub.typ sub)) pl;
    iter_loc sub pci_name;
    f pci_expr;
    sub.location sub pci_loc;
    sub.attributes sub pci_attributes
end

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let default_iterator =
  {
    structure = (fun this l -> List.iter (this.structure_item this) l);
    structure_item = M.iter_structure_item;
    module_expr = M.iter;
    signature = (fun this l -> List.iter (this.signature_item this) l);
    signature_item = MT.iter_signature_item;
    module_type = MT.iter;
    with_constraint = MT.iter_with_constraint;
    class_declaration =
      (fun this -> CE.class_infos this (this.class_expr this));
    class_expr = CE.iter;
    class_field = CE.iter_field;
    class_structure = CE.iter_structure;
    class_type = CT.iter;
    class_type_field = CT.iter_field;
    class_signature = CT.iter_signature;
    class_type_declaration =
      (fun this -> CE.class_infos this (this.class_type this));
    class_description =
      (fun this -> CE.class_infos this (this.class_type this));
    type_declaration = T.iter_type_declaration;
    type_kind = T.iter_type_kind;
    typ = T.iter;
    row_field = T.row_field;
    object_field = T.object_field;
    type_extension = T.iter_type_extension;
    type_exception = T.iter_type_exception;
    extension_constructor = T.iter_extension_constructor;
    value_description =
      (fun this {pval_name; pval_type; pval_prim = _; pval_loc;
                 pval_attributes} ->
        iter_loc this pval_name;
        this.typ this pval_type;
        this.location this pval_loc;
        this.attributes this pval_attributes;
      );

    pat = P.iter;
    expr = E.iter;
    binding_op = E.iter_binding_op;

    module_declaration =
      (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
         iter_loc this pmd_name;
         this.module_type this pmd_type;
         this.location this pmd_loc;
         this.attributes this pmd_attributes;
      );

    module_substitution =
      (fun this {pms_name; pms_manifest; pms_attributes; pms_loc} ->
         iter_loc this pms_name;
         iter_loc this pms_manifest;
         this.location this pms_loc;
         this.attributes this pms_attributes;
      );

    module_type_declaration =
      (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
         iter_loc this pmtd_name;
         iter_opt (this.module_type this) pmtd_type;
         this.location this pmtd_loc;
         this.attributes this pmtd_attributes;
      );

    module_binding =
      (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
         iter_loc this pmb_name; this.module_expr this pmb_expr;
         this.location this pmb_loc;
         this.attributes this pmb_attributes;
      );

    open_declaration =
      (fun this {popen_expr; popen_override = _; popen_attributes; popen_loc} ->
         this.module_expr this popen_expr;
         this.location this popen_loc;
         this.attributes this popen_attributes
      );

    open_description =
      (fun this {popen_expr; popen_override = _; popen_attributes; popen_loc} ->
         iter_loc this popen_expr;
         this.location this popen_loc;
         this.attributes this popen_attributes
      );


    include_description =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         this.module_type this pincl_mod;
         this.location this pincl_loc;
         this.attributes this pincl_attributes
      );

    include_declaration =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         this.module_expr this pincl_mod;
         this.location this pincl_loc;
         this.attributes this pincl_attributes
      );


    value_binding =
      (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
         this.pat this pvb_pat;
         this.expr this pvb_expr;
         this.location this pvb_loc;
         this.attributes this pvb_attributes
      );


    constructor_declaration =
      (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
         iter_loc this pcd_name;
         T.iter_constructor_arguments this pcd_args;
         iter_opt (this.typ this) pcd_res;
         this.location this pcd_loc;
         this.attributes this pcd_attributes
      );

    label_declaration =
      (fun this {pld_name; pld_type; pld_loc; pld_mutable = _; pld_attributes}->
         iter_loc this pld_name;
         this.typ this pld_type;
         this.location this pld_loc;
         this.attributes this pld_attributes
      );

    cases = (fun this l -> List.iter (this.case this) l);
    case =
      (fun this {pc_lhs; pc_guard; pc_rhs} ->
         this.pat this pc_lhs;
         iter_opt (this.expr this) pc_guard;
         this.expr this pc_rhs
      );

    location = (fun _this _l -> ());

    extension = (fun this (s, e) -> iter_loc this s; this.payload this e);
    attribute = (fun this a ->
      iter_loc this a.attr_name;
      this.payload this a.attr_payload;
      this.location this a.attr_loc
    );
    attributes = (fun this l -> List.iter (this.attribute this) l);
    payload =
      (fun this -> function
         | PStr x -> this.structure this x
         | PSig x -> this.signature this x
         | PTyp x -> this.typ this x
         | PPat (x, g) -> this.pat this x; iter_opt (this.expr this) g
      );
  }
