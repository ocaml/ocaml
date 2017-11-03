

module type Extension = sig type 'a t end

module No_extension= struct
  type 'a t = 'a
end

type out_string =
  | Ostr_string
  | Ostr_bytes

type out_attribute =
  { oattr_name: string }

type out_rec_status =
  | Orec_not
  | Orec_first
  | Orec_next

type out_ext_status =
  | Oext_first
  | Oext_next
  | Oext_exception

module Make(Ext:Extension) = struct
  type 'a ext = 'a Ext.t
  type out_ident =
    | Oide_apply of out_ident ext * out_ident ext
    | Oide_dot of out_ident ext * string ext
    | Oide_ident of string ext

  type out_value =
    | Oval_array of out_value list
    | Oval_char of char
    | Oval_constr of out_ident * out_value list
    | Oval_ellipsis
    | Oval_float of float
    | Oval_int of int
    | Oval_int32 of int32
    | Oval_int64 of int64
    | Oval_nativeint of nativeint
    | Oval_list of out_value list
    | Oval_printer of (Format.formatter -> unit)
    | Oval_record of (out_ident * out_value) list
    | Oval_string of string * int * out_string (* string, size-to-print, kind *)
    | Oval_stuff of string
    | Oval_tuple of out_value list
    | Oval_variant of string * out_value option


  type out_type =
    | Otyp_abstract
    | Otyp_open
    | Otyp_alias of out_type ext * string ext
    | Otyp_arrow of out_labelled ext * out_type ext
    | Otyp_class of bool ext * out_ident ext * out_type ext list
    | Otyp_constr of out_ident ext * out_type ext list
    | Otyp_manifest of out_type ext * out_type ext
    | Otyp_object of out_labelled ext list * bool ext option ext
    | Otyp_record of out_field ext list
    | Otyp_stuff of string
    | Otyp_sum of out_constructor ext list
    | Otyp_tuple of out_type ext list
    | Otyp_var of bool ext * string ext
    | Otyp_variant of
        bool ext * out_variant ext * bool ext * string ext list option ext
    | Otyp_poly of string ext list * out_type ext
    | Otyp_module of string ext * string ext list * out_type ext list
    | Otyp_attribute of out_type ext * out_attribute ext

  and out_labelled = (string ext * out_type ext)

  and out_constructor =
    {cname:string ext; args:out_type ext list; ret:out_type ext option ext}

  and out_variant =
    | Ovar_fields of out_var_field ext list
    | Ovar_typ of out_type ext

  and out_var_field = {tag:string ext; ampersand:bool ext; conj: out_type ext list}

  and out_field = {label:string ext; mut: bool ext; typ:out_type ext}

  type type_constraint = {lhs:out_type ext ;rhs:out_type ext}

  type out_class_type =
    | Octy_constr of out_ident ext * out_type ext list
    | Octy_arrow of out_labelled ext * out_class_type ext
    | Octy_signature of out_type ext option ext * out_class_sig_item ext list
  and out_class_sig_item =
    | Ocsg_constraint of type_constraint
    | Ocsg_method of string ext * bool ext * bool ext * out_type ext
    | Ocsg_value of string ext * bool ext * bool ext * out_type ext

  type type_param = {covariant:bool ext;contravariant:bool ext;name:string ext}

  type out_module_type =
    | Omty_abstract
    | Omty_functor of functor_arg ext * out_module_type ext
    | Omty_ident of out_ident ext
    | Omty_signature of out_sig_item ext list
    | Omty_alias of out_ident ext
  and functor_arg = string ext * out_module_type ext option ext
  and out_sig_item =
    | Osig_class of
        bool ext * string ext * type_param ext list * out_class_type ext
        * out_rec_status ext
    | Osig_class_type of
        bool ext * string ext * type_param ext list * out_class_type ext
        * out_rec_status ext
    | Osig_typext of out_extension_constructor * out_ext_status ext
    | Osig_modtype of string ext * out_module_type ext
    | Osig_module of string ext * out_module_type ext * out_rec_status ext
    | Osig_type of out_type_decl * out_rec_status ext
    | Osig_value of out_val_decl
    | Osig_ellipsis
  and out_type_decl =
    { otype_name: string ext;
      otype_params: type_param ext list;
      otype_type: out_type ext;
      otype_private: Asttypes.private_flag ext;
      otype_immediate: bool ext;
      otype_unboxed: bool ext;
      otype_cstrs: type_constraint ext list }
  and out_extension_constructor =
    { oext_name: string ext;
      oext_type_name: string ext;
      oext_type_params: string ext list;
      oext_args: out_type ext list;
      oext_ret_type: out_type ext option ext;
      oext_private: Asttypes.private_flag ext }
  and out_type_extension =
    { otyext_name: string ext;
      otyext_params: string ext list;
      otyext_constructors: out_constructor ext list;
      otyext_private: Asttypes.private_flag ext }
  and out_val_decl =
    { oval_name: string ext;
      oval_type: out_type ext;
      oval_prims: string ext list;
      oval_attributes: out_attribute ext list }

  type out_phrase =
    | Ophr_eval of out_value * out_type
    | Ophr_signature of (out_sig_item * out_value option) list
    | Ophr_exception of (exn * out_value)

end

module type S = module type of Make(struct type 'a t end)


module Highlightable = struct
  type emphase = On | Off
  type 'a t =
    | Item of emphase * 'a
    | Ellipsis of int (** ellipsis length *)
end

include Make(No_extension)
module Decorated = Make(Highlightable)

module Decorate = struct
  module D = Decorated
  open Highlightable
  let fwd emphase x = Item(emphase,x)
  let fwds emphase x = List.map (fwd emphase) x
  let fmap emphase f x = fwd emphase (f emphase x)

  let rec out_ident h = function
    | Oide_apply(x,y) ->
        D.Oide_apply(ident h x, ident h y)
    | Oide_dot(x,y) -> D.Oide_dot(ident h x, fwd h y)
    | Oide_ident x -> D.Oide_ident(fwd h x)
  and ident h x = fmap h out_ident x

  let may f = function None -> None | Some x -> Some (f x)
  let mayf st f x = fwd st (may (f st) @@ x)

  let rec out_value h = function
    | Oval_array x -> D.Oval_array (List.map (out_value h) x)
    | Oval_char x -> D.Oval_char x
    | Oval_constr(x,y) ->
        D.Oval_constr( out_ident h x, List.map (out_value h) y)
    | Oval_ellipsis -> D.Oval_ellipsis
    | Oval_float f -> D.Oval_float f
    | Oval_int f -> D.Oval_int f
    | Oval_int32 n -> D.Oval_int32 n
    | Oval_int64 n -> D.Oval_int64 n
    | Oval_nativeint n -> D.Oval_nativeint n
    | Oval_list l -> D.Oval_list (List.map (out_value h) l)
    | Oval_printer p -> D.Oval_printer p
    | Oval_record l ->
        D.Oval_record
          (List.map (fun (x,y) -> out_ident h x, out_value h y) l)
    | Oval_string (s,n,k) -> D.Oval_string(s,n, k)
    | Oval_stuff s -> D.Oval_stuff s
    | Oval_tuple v -> D.Oval_tuple (List.map (out_value h) v)
    | Oval_variant (s,ov) -> D.Oval_variant (s, may (out_value h) ov)

  let value h = fmap h out_value

  let rec out_type h = function
    | Otyp_abstract -> D.Otyp_abstract
    | Otyp_open -> D.Otyp_open
    | Otyp_alias(x,y) -> D.Otyp_alias(typ h x,fwd h y)
    | Otyp_arrow(l,t) -> D.Otyp_arrow(label h l, typ h t)
    | Otyp_class(b,id,ts) ->
        D.Otyp_class(fwd h b, ident h id,types h ts)
    | Otyp_constr(id,ts) ->
        D.Otyp_constr(ident h id, types h ts)
    | Otyp_manifest(t,t') ->
        D.Otyp_manifest(typ h t, typ h t')
    | Otyp_object(lbls, o) ->
        D.Otyp_object(labels h lbls, mayf h fwd o)
    | Otyp_record r -> D.Otyp_record(fields h r)
    | Otyp_stuff s -> D.Otyp_stuff s
    | Otyp_sum cntrs -> D.Otyp_sum (List.map (constr h) cntrs)
    | Otyp_tuple ts -> D.Otyp_tuple(types h ts)
    | Otyp_var(b,s) -> D.Otyp_var(fwd h b, fwd h s)
    | Otyp_variant (b,vars,b',tags) ->
        D.Otyp_variant(fwd h b,variant h vars, fwd h b',
                       mayf h fwds tags )
    | Otyp_poly(us, t) -> D.Otyp_poly(fwds h us, typ h t)
    | Otyp_module (name,with',ts) ->
        D.Otyp_module(fwd h name, fwds h with', types h ts)
    | Otyp_attribute(t, attrs) ->
        D.Otyp_attribute(typ h t, fwd h attrs)

  and typ h x = fmap h out_type x
  and types h l = List.map (typ h) l
  and label h (lbl,t) = fwd h (fwd h lbl, typ h t)
  and labels h x = List.map (label h) x
  and constr h c =
    fwd h {D.cname = fwd h c.cname; args = types h c.args;
         ret = mayf h typ c.ret}

  and out_variant h = function
    | Ovar_fields fs -> D.Ovar_fields (List.map (var_field h) fs)
    | Ovar_typ t -> D.Ovar_typ (typ h t)

  and variant h x = fmap h out_variant x

  and var_field h f =
    fwd h {D.tag = fwd h f.tag; ampersand = fwd h f.ampersand;
         conj = types h f.conj }

  and field h f =
    {D.label= fwd h f.label; mut = fwd h f.mut; typ = typ h f.typ }
  and fields h x = List.map (fmap h field) x

  let type_constraint h c=
   {D.lhs= typ h c.lhs ;rhs = typ h c.rhs}

  let rec out_class_type h = function
    | Octy_constr(id,ts) -> D.Octy_constr(ident h id, types h ts)
    | Octy_arrow(lbl,t) -> D.Octy_arrow(label h lbl, class_type h t)
    | Octy_signature(self, items) ->
        D.Octy_signature(mayf h typ self,
                         List.map (fmap h out_class_sig_item) items)
  and out_class_sig_item h = function
    | Ocsg_constraint x -> D.Ocsg_constraint (type_constraint h x)
    | Ocsg_method(name,priv,virt,t) ->
        D.Ocsg_method(fwd h name,fwd h priv,fwd h virt, typ h t)
    | Ocsg_value(name,priv,virt,t) ->
        D.Ocsg_value(fwd h name,fwd h priv,fwd h virt, typ h t)
  and class_type h x = fmap h out_class_type x
  let type_param h tp =
    fwd h {D.covariant= fwd h tp.covariant;
         contravariant= fwd h tp.contravariant;
     name = fwd h tp.name}

  let type_params h = List.map (type_param h)

  let rec out_module_type h = function
    | Omty_abstract -> D.Omty_abstract
    | Omty_functor ( (name, arg), res) ->
        D.Omty_functor(fwd h (fwd h name, mayf h module_type arg),
                       module_type h res)
    | Omty_ident id -> D.Omty_ident (ident h id)
    | Omty_signature items -> D.Omty_signature (sigitems h items)
    | Omty_alias id -> D.Omty_alias(ident h id)
  and module_type h x = fmap h out_module_type x
  and out_sig_item h = function
    | Osig_class (virt, name, tps, ct, recs) ->
        D.Osig_class(fwd h virt,fwd h name, type_params h tps,
                     class_type h ct, fwd h recs)
    | Osig_class_type(virt, name, tps, ct, recs) ->
        D.Osig_class_type(fwd h virt,fwd h name, type_params h tps,
                          class_type h ct,fwd h recs)
    | Osig_typext(ext,est) ->
        D.Osig_typext(extension_constructor h ext, fwd h est)
    | Osig_modtype(name,mt) ->
        D.Osig_modtype(fwd h name,module_type h mt)
    | Osig_module(name,mt,recs) ->
        D.Osig_module(fwd h name,module_type h mt, fwd h recs)
    | Osig_type(decl, recs) ->
        D.Osig_type(type_decl h decl, fwd h recs)
    | Osig_value v -> D.Osig_value (val_decl h v)
    | Osig_ellipsis -> D.Osig_ellipsis
  and sig_item h x = fmap h out_sig_item x
  and sigitems h x = List.map (sig_item h) x
  and type_decl h d =
    { D.otype_name= fwd h d.otype_name;
      otype_params= type_params h d.otype_params;
      otype_type = typ h d.otype_type;
      otype_private= fwd h d.otype_private;
      otype_immediate= fwd h d.otype_immediate;
      otype_unboxed= fwd h d.otype_unboxed;
      otype_cstrs= List.map (fmap h type_constraint) d.otype_cstrs }
  and extension_constructor h e =
    { D.oext_name = fwd h e.oext_name;
      oext_type_name = fwd h e.oext_type_name;
      oext_type_params = fwds h e.oext_type_params;
      oext_args = types h e.oext_args;
      oext_ret_type = mayf h typ e.oext_ret_type;
      oext_private = fwd h e.oext_private }
  and type_extension h e =
    fwd h { D.otyext_name= fwd h e.otyext_name;
      otyext_params= fwds h e.otyext_params;
      otyext_constructors= List.map (constr h) e.otyext_constructors;
      otyext_private = fwd h e.otyext_private}
  and val_decl h v =
    { D.oval_name = fwd h v.oval_name;
      oval_type = typ h v.oval_type;
      oval_prims = fwds h v.oval_prims;
      oval_attributes = fwds h v.oval_attributes}

  let signature = sigitems
  let phrase h = function
    | Ophr_eval(v,t) ->
        D.Ophr_eval (out_value h v, out_type h t)
    | Ophr_signature l ->
        D.Ophr_signature(List.map (fun (x,y) ->
            out_sig_item h x, may (out_value h) y) l )
    | Ophr_exception (exn, v) ->
        D.Ophr_exception(exn, out_value h v)

  let export f ?highlight:(h=Off) = f h
  let value = export value
  let ident = export ident
  let typ = export typ
  let variant = export variant
  let type_extension = export type_extension
  let sig_item = export sig_item
  let signature = export signature
  let module_type = export module_type
  let class_type = export class_type
  let phrase = export phrase
end
