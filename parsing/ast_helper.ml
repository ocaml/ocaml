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

module Typ = struct
  let mk ?(attrs = []) ?(loc = Location.none) d = {ptyp_desc = d; ptyp_loc = loc; ptyp_attributes = attrs}
  let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
  let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_arrow (a, b, c))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
  let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
  let object_ ?loc ?attrs a = mk ?loc ?attrs (Ptyp_object a)
  let class_ ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_class (a, b, c))
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
  let variant ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_variant (a, b, c))
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
  let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Ptyp_extension a)
end

module Pat = struct
  let mk ?(attrs = []) ?(loc = Location.none) d = {ppat_desc = d; ppat_loc = loc; ppat_attributes = attrs}
  let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
  let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
  let construct ?loc ?attrs a b c = mk ?loc ?attrs (Ppat_construct (a, b, c))
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
  let mk ?(attrs = []) ?(loc = Location.none) d = {pexp_desc = d; pexp_loc = loc; pexp_attributes = attrs}
  let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
  let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
  let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_let (a, b, c))
  let function_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_function (a, b, c))
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
  let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
  let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
  let construct ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_construct (a, b, c))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
  let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
  let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
  let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
  let ifthenelse ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_ifthenelse (a, b, c))
  let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
  let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
  let for_ ?loc ?attrs a b c d e = mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
  let constraint_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_constraint (a, b, c))
  let when_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_when (a, b))
  let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
  let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
  let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
  let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
  let letmodule ?loc ?attrs a b c= mk ?loc ?attrs (Pexp_letmodule (a, b, c))
  let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
  let assertfalse ?loc ?attrs () = mk ?loc ?attrs Pexp_assertfalse
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
  let object_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_object a)
  let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
  let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
  let open_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_open (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)
end

module Mty = struct
  let mk ?(attrs = []) ?(loc = Location.none) d = {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
  let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
  let functor_ ?loc ?attrs a b c = mk ?loc ?attrs (Pmty_functor (a, b, c))
  let with_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_with (a, b))
  let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmty_extension a)
end

module Mod = struct
let mk ?(attrs = []) ?(loc = Location.none) d = {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs}
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
  let mk ?(loc = Location.none) d = {psig_desc = d; psig_loc = loc}

  let value ?loc a = mk ?loc (Psig_value a)
  let type_ ?loc a = mk ?loc (Psig_type a)
  let exception_ ?loc a = mk ?loc (Psig_exception a)
  let module_ ?loc a = mk ?loc (Psig_module a)
  let rec_module ?loc a = mk ?loc (Psig_recmodule a)
  let modtype ?loc a = mk ?loc (Psig_modtype a)
  let open_ ?loc ?(attrs = []) a = mk ?loc (Psig_open (a, attrs))
  let include_ ?loc ?(attrs = []) a = mk ?loc (Psig_include (a, attrs))
  let class_ ?loc a = mk ?loc (Psig_class a)
  let class_type ?loc a = mk ?loc (Psig_class_type a)
  let extension ?loc ?(attrs = []) a = mk ?loc (Psig_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Psig_attribute a)
end

module Str = struct
  let mk ?(loc = Location.none) d = {pstr_desc = d; pstr_loc = loc}

  let eval ?loc a = mk ?loc (Pstr_eval a)
  let value ?loc a b = mk ?loc (Pstr_value (a, b))
  let primitive ?loc a = mk ?loc (Pstr_primitive a)
  let type_ ?loc a = mk ?loc (Pstr_type a)
  let exception_ ?loc a = mk ?loc (Pstr_exception a)
  let exn_rebind ?loc ?(attrs = []) a b = mk ?loc (Pstr_exn_rebind (a, b, attrs))
  let module_ ?loc a = mk ?loc (Pstr_module a)
  let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
  let modtype ?loc a = mk ?loc (Pstr_modtype a)
  let open_ ?loc ?(attrs = []) a = mk ?loc (Pstr_open (a, attrs))
  let class_ ?loc a = mk ?loc (Pstr_class a)
  let class_type ?loc a = mk ?loc (Pstr_class_type a)
  let include_ ?loc ?(attrs = []) a = mk ?loc (Pstr_include (a, attrs))
  let extension ?loc ?(attrs = []) a = mk ?loc (Pstr_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Pstr_attribute a)
end

module Field = struct
  let mk ?(loc = Location.none) d = {pfield_desc = d; pfield_loc = loc}

  let field ?loc s t =
    let t =
      (* The type-checker expects the field to be a Ptyp_poly. Maybe
         it should wrap the type automatically... *)
      match t.ptyp_desc with
      | Ptyp_poly _ -> t
      | _ -> Typ.poly ?loc [] t
    in
    mk ?loc (Pfield (s, t))
  let var ?loc () = mk ?loc Pfield_var
end

module Cl = struct
  let mk ?(loc = Location.none) d = {pcl_desc = d; pcl_loc = loc}

  let constr ?loc a b = mk ?loc (Pcl_constr (a, b))
  let structure ?loc a = mk ?loc (Pcl_structure a)
  let fun_ ?loc a b c d = mk ?loc (Pcl_fun (a, b, c, d))
  let apply ?loc a b = mk ?loc (Pcl_apply (a, b))
  let let_ ?loc a b c = mk ?loc (Pcl_let (a, b, c))
  let constraint_ ?loc a b = mk ?loc (Pcl_constraint (a, b))
end

module Cty = struct
  let mk ?(loc = Location.none) d = {pcty_desc = d; pcty_loc = loc}

  let constr ?loc a b = mk ?loc (Pcty_constr (a, b))
  let signature ?loc a = mk ?loc (Pcty_signature a)
  let fun_ ?loc a b c = mk ?loc (Pcty_fun (a, b, c))
end

module Ctf = struct
  let mk ?(loc = Location.none) d = {pctf_desc = d; pctf_loc = loc}

  let inher ?loc a = mk ?loc (Pctf_inher a)
  let val_ ?loc a b c d = mk ?loc (Pctf_val (a, b, c, d))
  let virt ?loc a b c = mk ?loc (Pctf_virt (a, b, c))
  let meth ?loc a b c = mk ?loc (Pctf_meth (a, b, c))
  let cstr ?loc a b = mk ?loc (Pctf_cstr (a, b))
end

module Cf = struct
  let mk ?(loc = Location.none) d = {pcf_desc = d; pcf_loc = loc}

  let inher ?loc a b c = mk ?loc (Pcf_inher (a, b, c))
  let valvirt ?loc a b c = mk ?loc (Pcf_valvirt (a, b, c))
  let val_ ?loc a b c d = mk ?loc (Pcf_val (a, b, c, d))
  let virt ?loc a b c = mk ?loc (Pcf_virt (a, b, c))
  let meth ?loc a b c d = mk ?loc (Pcf_meth (a, b, c, d))
  let constr ?loc a b = mk ?loc (Pcf_constr (a, b))
  let init ?loc a = mk ?loc (Pcf_init a)
end

module Val = struct
  let mk ?(attrs = []) ?(loc = Location.none) ?(prim = []) name typ =
    {
     pval_name = name;
     pval_type = typ;
     pval_attributes = attrs;
     pval_loc = loc;
     pval_prim = prim;
    }
end

module Mtb = struct
  let mk ?(attrs = []) name typ =
    {
     pmtb_name = name;
     pmtb_type = typ;
     pmtb_attributes = attrs;
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

module Ci = struct
  let mk ?(attrs = []) ?(loc = Location.none) ?(virt = Concrete) ?(params = [], Location.none) name expr =
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
  let mk ?(attrs = []) ?(loc = Location.none)
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
end


module Cd = struct
  let mk ?(attrs = []) ?(loc = Location.none) ?(args = []) ?res name =
    {
     pcd_name = name;
     pcd_args = args;
     pcd_res = res;
     pcd_loc = loc;
     pcd_attributes = attrs;
    }
end


module Ld = struct
  let mk ?(attrs = []) ?(loc = Location.none) ?(mut = Immutable) name typ =
    {
     pld_name = name;
     pld_mutable = mut;
     pld_type = typ;
     pld_loc = loc;
     pld_attributes = attrs;
    }
end

module Csig = struct
  let mk ?(loc = Location.none) self fields =
    {
     pcsig_self = self;
     pcsig_fields = fields;
     pcsig_loc = loc;
    }
end
