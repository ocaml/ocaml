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

(** Helpers to produce Parsetree fragments *)

open Asttypes
open Parsetree

type lid = Longident.t loc
type str = string loc
type loc = Location.t
type attrs = attribute list

let default_loc = ref Location.none

let with_default_loc l f =
  let old = !default_loc in
  default_loc := l;
  try let r = f () in default_loc := old; r
  with exn -> default_loc := old; raise exn

module Typ = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d = {ptyp_desc = d; ptyp_loc = loc; ptyp_attributes = attrs}
  let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
  let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_arrow (a, b, c))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
  let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
  let object_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_object (a, b))
  let class_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_class (a, b))
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
  let variant ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_variant (a, b, c))
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
  let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Ptyp_extension a)

  let force_poly t =
    match t.ptyp_desc with
    | Ptyp_poly _ -> t
    | _ -> poly ~loc:t.ptyp_loc [] t (* -> ghost? *)
end

module Pat = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d = {ppat_desc = d; ppat_loc = loc; ppat_attributes = attrs}
  let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
  let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
  let interval ?loc ?attrs a b = mk ?loc ?attrs (Ppat_interval (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
  let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
  let or_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_or (a, b))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
  let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
  let unpack ?loc ?attrs a = mk ?loc ?attrs (Ppat_unpack a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Ppat_extension a)
end

module Exp = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d = {pexp_desc = d; pexp_loc = loc; pexp_attributes = attrs}
  let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
  let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
  let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_let (a, b, c))
  let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pexp_fun (a, b, c, d))
  let function_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_function a)
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
  let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
  let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Pexp_construct (a, b))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
  let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
  let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
  let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
  let ifthenelse ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_ifthenelse (a, b, c))
  let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
  let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
  let for_ ?loc ?attrs a b c d e = mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_constraint (a, b))
  let coerce ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_coerce (a, b, c))
  let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
  let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
  let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
  let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
  let letmodule ?loc ?attrs a b c= mk ?loc ?attrs (Pexp_letmodule (a, b, c))
  let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
  let object_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_object a)
  let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
  let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
  let open_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_open (a, b, c))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)

  let case lhs ?guard rhs =
    {
     pc_lhs = lhs;
     pc_guard = guard;
     pc_rhs = rhs;
    }
end

module Mty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d = {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
  let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
  let functor_ ?loc ?attrs a b c = mk ?loc ?attrs (Pmty_functor (a, b, c))
  let with_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_with (a, b))
  let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmty_extension a)
end

module Mod = struct
let mk ?(loc = !default_loc) ?(attrs = []) d = {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs}
  let attr d a = {d with pmod_attributes = d.pmod_attributes @ [a]}

  let ident ?loc ?attrs x = mk ?loc ?attrs (Pmod_ident x)
  let structure ?loc ?attrs x = mk ?loc ?attrs (Pmod_structure x)
  let functor_ ?loc ?attrs arg arg_ty body = mk ?loc ?attrs (Pmod_functor (arg, arg_ty, body))
  let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
  let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
  let unpack ?loc ?attrs e = mk ?loc ?attrs (Pmod_unpack e)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmod_extension a)
end

module Sig = struct
  let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

  let value ?loc a = mk ?loc (Psig_value a)
  let type_ ?loc a = mk ?loc (Psig_type a)
  let exception_ ?loc a = mk ?loc (Psig_exception a)
  let module_ ?loc a = mk ?loc (Psig_module a)
  let rec_module ?loc a = mk ?loc (Psig_recmodule a)
  let modtype ?loc a = mk ?loc (Psig_modtype a)
  let open_ ?loc ?(attrs = []) a b = mk ?loc (Psig_open (a, b, attrs))
  let include_ ?loc ?(attrs = []) a = mk ?loc (Psig_include (a, attrs))
  let class_ ?loc a = mk ?loc (Psig_class a)
  let class_type ?loc a = mk ?loc (Psig_class_type a)
  let extension ?loc ?(attrs = []) a = mk ?loc (Psig_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Psig_attribute a)
end

module Str = struct
  let mk ?(loc = !default_loc) d = {pstr_desc = d; pstr_loc = loc}

  let eval ?loc ?(attrs = []) a = mk ?loc (Pstr_eval (a, attrs))
  let value ?loc a b = mk ?loc (Pstr_value (a, b))
  let primitive ?loc a = mk ?loc (Pstr_primitive a)
  let type_ ?loc a = mk ?loc (Pstr_type a)
  let exception_ ?loc a = mk ?loc (Pstr_exception a)
  let exn_rebind ?loc ?(attrs = []) a b = mk ?loc (Pstr_exn_rebind (a, b, attrs))
  let module_ ?loc a = mk ?loc (Pstr_module a)
  let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
  let modtype ?loc a = mk ?loc (Pstr_modtype a)
  let open_ ?loc ?(attrs = []) a b = mk ?loc (Pstr_open (a, b, attrs))
  let class_ ?loc a = mk ?loc (Pstr_class a)
  let class_type ?loc a = mk ?loc (Pstr_class_type a)
  let include_ ?loc ?(attrs = []) a = mk ?loc (Pstr_include (a, attrs))
  let extension ?loc ?(attrs = []) a = mk ?loc (Pstr_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Pstr_attribute a)
end

module Cl = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcl_desc = d;
     pcl_loc = loc;
     pcl_attributes = attrs;
    }
  let attr d a = {d with pcl_attributes = d.pcl_attributes @ [a]}

  let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constr (a, b))
  let structure ?loc ?attrs a = mk ?loc ?attrs (Pcl_structure a)
  let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pcl_fun (a, b, c, d))
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pcl_apply (a, b))
  let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcl_let (a, b, c))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constraint (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcl_extension a)
end

module Cty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcty_desc = d;
     pcty_loc = loc;
     pcty_attributes = attrs;
    }
  let attr d a = {d with pcty_attributes = d.pcty_attributes @ [a]}

  let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcty_constr (a, b))
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pcty_signature a)
  let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Pcty_arrow (a, b, c))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcty_extension a)
end

module Ctf = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pctf_desc = d;
     pctf_loc = loc;
     pctf_attributes = attrs;
    }
  let attr d a = {d with pctf_attributes = d.pctf_attributes @ [a]}

  let inherit_ ?loc ?attrs a = mk ?loc ?attrs (Pctf_inherit a)
  let val_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_val (a, b, c, d))
  let method_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_method (a, b, c, d))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pctf_constraint (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pctf_extension a)
end

module Cf = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcf_desc = d;
     pcf_loc = loc;
     pcf_attributes = attrs;
    }
  let attr d a = {d with pcf_attributes = d.pcf_attributes @ [a]}

  let inherit_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_inherit (a, b, c))
  let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_val (a, b, c))
  let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_method (a, b, c))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcf_constraint (a, b))
  let initializer_ ?loc ?attrs a = mk ?loc ?attrs (Pcf_initializer a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcf_extension a)

  let virtual_ ct = Cfk_virtual ct
  let concrete o e = Cfk_concrete (o, e)
end

module Val = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(prim = []) name typ =
    {
     pval_name = name;
     pval_type = typ;
     pval_attributes = attrs;
     pval_loc = loc;
     pval_prim = prim;
    }
end

module Md = struct
  let mk ?(attrs = []) name typ =
    {
     pmd_name = name;
     pmd_type = typ;
     pmd_attributes = attrs;
    }
end

module Mtd = struct
  let mk ?(attrs = []) ?typ name =
    {
     pmtd_name = name;
     pmtd_type = typ;
     pmtd_attributes = attrs;
    }
end

module Mb = struct
  let mk ?(attrs = []) name expr =
    {
     pmb_name = name;
     pmb_expr = expr;
     pmb_attributes = attrs;
    }
end

module Vb = struct
  let mk ?(attrs = []) pat expr =
    {
     pvb_pat = pat;
     pvb_expr = expr;
     pvb_attributes = attrs;
    }
end

module Ci = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(virt = Concrete) ?(params = []) name expr =
    {
     pci_virt = virt;
     pci_params = params;
     pci_name = name;
     pci_expr = expr;
     pci_attributes = attrs;
     pci_loc = loc;
    }
end

module Type = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
      ?(params = [])
      ?(cstrs = [])
      ?(kind = Ptype_abstract)
      ?(priv = Public)
      ?manifest
      name =
    {
     ptype_name = name;
     ptype_params = params;
     ptype_cstrs = cstrs;
     ptype_kind = kind;
     ptype_private = priv;
     ptype_manifest = manifest;
     ptype_attributes = attrs;
     ptype_loc = loc;
    }

  let constructor ?(loc = !default_loc) ?(attrs = []) ?(args = []) ?res name =
    {
     pcd_name = name;
     pcd_args = args;
     pcd_res = res;
     pcd_loc = loc;
     pcd_attributes = attrs;
    }

  let field ?(loc = !default_loc) ?(attrs = []) ?(mut = Immutable) name typ =
    {
     pld_name = name;
     pld_mutable = mut;
     pld_type = typ;
     pld_loc = loc;
     pld_attributes = attrs;
    }
end

module Csig = struct
  let mk self fields =
    {
     pcsig_self = self;
     pcsig_fields = fields;
    }
end

module Cstr = struct
  let mk self fields =
    {
     pcstr_self = self;
     pcstr_fields = fields;
    }
end

module Convenience = struct
  open Location

  let may_tuple tup = function
    | [] -> None
    | [x] -> Some x
    | l -> Some (tup l)

  let lid s = mkloc (Longident.parse s) !default_loc
  let tuple l = Exp.tuple l
  let constr s args = Exp.construct (lid s) (may_tuple Exp.tuple args)
  let nil () = constr "[]" []
  let unit () = constr "()" []
  let cons hd tl = constr "::" [hd; tl]
  let list l = List.fold_right cons l (nil ())
  let str s = Exp.constant (Const_string (s, None))
  let int x = Exp.constant (Const_int x)
  let char x = Exp.constant (Const_char x)
  let float x = Exp.constant (Const_float (string_of_float x))
  let record ?over l =
    Exp.record (List.map (fun (s, e) -> (lid s, e)) l) over
  let func l = Exp.function_ (List.map (fun (p, e) -> Exp.case p e) l)
  let lam ?(label = "") ?default pat exp = Exp.fun_ label default pat exp
  let app f l = Exp.apply f (List.map (fun a -> "", a) l)
  let evar s = Exp.ident (lid s)
  let let_in ?(recursive = false) b body =
    Exp.let_ (if recursive then Recursive else Nonrecursive) b body

  let pvar s = Pat.var (mkloc s !default_loc)
  let pconstr s args = Pat.construct (lid s) (may_tuple Pat.tuple args)
  let punit () = pconstr "()" []


  let tconstr c l = Typ.constr (lid c) l

  let get_str = function
    | {pexp_desc=Pexp_constant (Const_string (s, _)); _} -> Some s
    | e -> None

  let get_lid = function
    | {pexp_desc=Pexp_ident{txt=id;_};_} ->
        Some (String.concat "." (Longident.flatten id))
    | _ -> None

  let find_attr s attrs =
    try Some (snd (List.find (fun (x, _) -> x.txt = s) attrs))
    with Not_found -> None

  let expr_of_payload = function
    | PStr [{pstr_desc=Pstr_eval(e, _)}] -> Some e
    | _ -> None

  let find_attr_expr s attrs =
    match find_attr s attrs with
    | Some e -> expr_of_payload e
    | None -> None

  let has_attr s attrs =
    find_attr s attrs <> None
end
