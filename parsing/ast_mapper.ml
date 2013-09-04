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

(* A generic Parsetree mapping class *)

open Location
open Config
open Parsetree
open Asttypes
open Ast_helper

let map_fst f (x, y) = (f x, y)
let map_snd f (x, y) = (x, f y)
let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
let map_opt f = function None -> None | Some x -> Some (f x)

let map_loc sub {loc; txt} = {loc = sub # location loc; txt}

module T = struct
  (* Type expressions for the core language *)

  let row_field sub = function
    | Rtag (l, b, tl) -> Rtag (l, b, List.map (sub # typ) tl)
    | Rinherit t -> Rinherit (sub # typ t)

  let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
    let open Typ in
    let loc = sub # location loc in
    let attrs = sub # attributes attrs in
    match desc with
    | Ptyp_any -> any ~loc ~attrs ()
    | Ptyp_var s -> var ~loc ~attrs s
    | Ptyp_arrow (lab, t1, t2) ->
        arrow ~loc ~attrs lab (sub # typ t1) (sub # typ t2)
    | Ptyp_tuple tyl -> tuple ~loc ~attrs (List.map (sub # typ) tyl)
    | Ptyp_constr (lid, tl) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub # typ) tl)
    | Ptyp_object (l, o) ->
        object_ ~loc ~attrs (List.map (map_snd (sub # typ)) l) o
    | Ptyp_class (lid, tl) ->
        class_ ~loc ~attrs (map_loc sub lid) (List.map (sub # typ) tl)
    | Ptyp_alias (t, s) -> alias ~loc ~attrs (sub # typ t) s
    | Ptyp_variant (rl, b, ll) ->
        variant ~loc ~attrs (List.map (row_field sub) rl) b ll
    | Ptyp_poly (sl, t) -> poly ~loc ~attrs sl (sub # typ t)
    | Ptyp_package (lid, l) ->
        package ~loc ~attrs (map_loc sub lid)
                (List.map (map_tuple (map_loc sub) (sub # typ)) l)
    | Ptyp_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_type_declaration sub
      {ptype_name; ptype_params; ptype_cstrs;
       ptype_kind;
       ptype_private;
       ptype_manifest;
       ptype_attributes;
       ptype_loc} =
    Type.mk (map_loc sub ptype_name)
      ~params:(List.map (map_fst (map_opt (map_loc sub))) ptype_params)
      ~priv:ptype_private
      ~cstrs:(List.map (map_tuple3 (sub # typ) (sub # typ) (sub # location))
                       ptype_cstrs)
      ~kind:(sub # type_kind ptype_kind)
      ?manifest:(map_opt (sub # typ) ptype_manifest)
      ~loc:(sub # location ptype_loc)
      ~attrs:(sub # attributes ptype_attributes)

  let map_type_kind sub = function
    | Ptype_abstract -> Ptype_abstract
    | Ptype_variant l ->
        Ptype_variant (List.map (sub # constructor_declaration) l)
    | Ptype_record l -> Ptype_record (List.map (sub # label_declaration) l)
end

module CT = struct
  (* Type expressions for the class language *)

  let map sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
    let open Cty in
    let loc = sub # location loc in
    match desc with
    | Pcty_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub # typ) tys)
    | Pcty_signature x -> signature ~loc ~attrs (sub # class_signature x)
    | Pcty_arrow (lab, t, ct) ->
        arrow ~loc ~attrs lab (sub # typ t) (sub # class_type ct)
    | Pcty_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
  =
    let open Ctf in
    let loc = sub # location loc in
    match desc with
    | Pctf_inherit ct -> inherit_ ~loc ~attrs (sub # class_type ct)
    | Pctf_val (s, m, v, t) -> val_ ~loc ~attrs s m v (sub # typ t)
    | Pctf_method (s, p, v, t) -> method_ ~loc ~attrs s p v (sub # typ t)
    | Pctf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub # typ t1) (sub # typ t2)
    | Pctf_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_signature sub {pcsig_self; pcsig_fields} =
    Csig.mk
      (sub # typ pcsig_self)
      (List.map (sub # class_type_field) pcsig_fields)
end

module MT = struct
  (* Type expressions for the module language *)

  let map sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
    let open Mty in
    let loc = sub # location loc in
    let attrs = sub # attributes attrs in
    match desc with
    | Pmty_ident s -> ident ~loc ~attrs (map_loc sub s)
    | Pmty_signature sg -> signature ~loc ~attrs (sub # signature sg)
    | Pmty_functor (s, mt1, mt2) ->
        functor_ ~loc ~attrs (map_loc sub s) (sub # module_type mt1)
                 (sub # module_type mt2)
    | Pmty_with (mt, l) ->
        with_ ~loc ~attrs (sub # module_type mt)
              (List.map (sub # with_constraint) l)
    | Pmty_typeof me -> typeof_ ~loc ~attrs (sub # module_expr me)
    | Pmty_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_with_constraint sub = function
    | Pwith_type (lid, d) ->
        Pwith_type (map_loc sub lid, sub # type_declaration d)
    | Pwith_module (lid, lid2) ->
        Pwith_module (map_loc sub lid, map_loc sub lid2)
    | Pwith_typesubst d -> Pwith_typesubst (sub # type_declaration d)
    | Pwith_modsubst (s, lid) ->
        Pwith_modsubst (map_loc sub s, map_loc sub lid)

  let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
    let open Sig in
    let loc = sub # location loc in
    match desc with
    | Psig_value vd -> value ~loc (sub # value_description vd)
    | Psig_type l -> type_ ~loc (List.map (sub # type_declaration) l)
    | Psig_exception ed -> exception_ ~loc (sub # constructor_declaration ed)
    | Psig_module x -> module_ ~loc (sub # module_declaration x)
    | Psig_recmodule l ->
        rec_module ~loc (List.map (sub # module_declaration) l)
    | Psig_modtype x -> modtype ~loc (sub # module_type_declaration x)
    | Psig_open (ovf, lid, attrs) ->
        open_ ~loc ~attrs:(sub # attributes attrs) ovf (map_loc sub lid)
    | Psig_include (mt, attrs) ->
        include_ ~loc (sub # module_type mt) ~attrs:(sub # attributes attrs)
    | Psig_class l -> class_ ~loc (List.map (sub # class_description) l)
    | Psig_class_type l ->
        class_type ~loc (List.map (sub # class_type_declaration) l)
    | Psig_extension (x, attrs) ->
        extension ~loc (sub # extension x) ~attrs:(sub # attributes attrs)
    | Psig_attribute x -> attribute ~loc (sub # attribute x)
end


module M = struct
  (* Value expressions for the module language *)

  let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    let open Mod in
    let loc = sub # location loc in
    let attrs = sub # attributes attrs in
    match desc with
    | Pmod_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pmod_structure str -> structure ~loc ~attrs (sub # structure str)
    | Pmod_functor (arg, arg_ty, body) ->
        functor_ ~loc ~attrs (map_loc sub arg) (sub # module_type arg_ty)
                 (sub # module_expr body)
    | Pmod_apply (m1, m2) ->
        apply ~loc ~attrs (sub # module_expr m1) (sub # module_expr m2)
    | Pmod_constraint (m, mty) ->
        constraint_ ~loc ~attrs (sub # module_expr m) (sub # module_type mty)
    | Pmod_unpack e -> unpack ~loc ~attrs (sub # expr e)
    | Pmod_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
    let open Str in
    let loc = sub # location loc in
    match desc with
    | Pstr_eval (x, attrs) ->
        eval ~loc ~attrs:(sub # attributes attrs) (sub # expr x)
    | Pstr_value (r, vbs) -> value ~loc r (List.map (sub # value_binding) vbs)
    | Pstr_primitive vd -> primitive ~loc (sub # value_description vd)
    | Pstr_type l -> type_ ~loc (List.map (sub # type_declaration) l)
    | Pstr_exception ed -> exception_ ~loc (sub # constructor_declaration ed)
    | Pstr_exn_rebind (s, lid, attrs) ->
        exn_rebind ~loc (map_loc sub s) (map_loc sub lid)
                   ~attrs:(sub # attributes attrs)
    | Pstr_module x -> module_ ~loc (sub # module_binding x)
    | Pstr_recmodule l -> rec_module ~loc (List.map (sub # module_binding) l)
    | Pstr_modtype x -> modtype ~loc (sub # module_type_declaration x)
    | Pstr_open (ovf, lid, attrs) ->
        open_ ~loc ~attrs:(sub # attributes attrs) ovf (map_loc sub lid)
    | Pstr_class l -> class_ ~loc (List.map (sub # class_declaration) l)
    | Pstr_class_type l ->
        class_type ~loc (List.map (sub # class_type_declaration) l)
    | Pstr_include (e, attrs) ->
        include_ ~loc (sub # module_expr e) ~attrs:(sub # attributes attrs)
    | Pstr_extension (x, attrs) ->
        extension ~loc (sub # extension x) ~attrs:(sub # attributes attrs)
    | Pstr_attribute x -> attribute ~loc (sub # attribute x)
end

module E = struct
  (* Value expressions for the core language *)

  let lid ?(loc = Location.none) ?attrs lid =
    Exp.ident ~loc ?attrs (mkloc (Longident.parse lid) loc)
  let apply_nolabs ?loc ?attrs f el =
    Exp.apply ?loc ?attrs f (List.map (fun e -> ("", e)) el)
  let strconst ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_string (x, None))

  let map sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
    let open Exp in
    let loc = sub # location loc in
    let attrs = sub # attributes attrs in
    match desc with
    | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pexp_constant x -> constant ~loc ~attrs x
    | Pexp_let (r, vbs, e) ->
        let_ ~loc ~attrs r (List.map (sub # value_binding) vbs) (sub # expr e)
    | Pexp_fun (lab, def, p, e) ->
        fun_ ~loc ~attrs lab (map_opt (sub # expr) def) (sub # pat p)
             (sub # expr e)
    | Pexp_function pel -> function_ ~loc ~attrs (sub # cases pel)
    | Pexp_apply (e, l) ->
        apply ~loc ~attrs (sub # expr e) (List.map (map_snd (sub # expr)) l)
    | Pexp_match (e, pel) -> match_ ~loc ~attrs (sub # expr e) (sub # cases pel)
    | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub # expr e) (sub # cases pel)
    | Pexp_tuple el -> tuple ~loc ~attrs (List.map (sub # expr) el)
    | Pexp_construct (lid, arg) ->
        construct ~loc ~attrs (map_loc sub lid) (map_opt (sub # expr) arg)
    | Pexp_variant (lab, eo) ->
        variant ~loc ~attrs lab (map_opt (sub # expr) eo)
    | Pexp_record (l, eo) ->
        record ~loc ~attrs (List.map (map_tuple (map_loc sub) (sub # expr)) l)
               (map_opt (sub # expr) eo)
    | Pexp_field (e, lid) -> field ~loc ~attrs (sub # expr e) (map_loc sub lid)
    | Pexp_setfield (e1, lid, e2) ->
        setfield ~loc ~attrs (sub # expr e1) (map_loc sub lid) (sub # expr e2)
    | Pexp_array el -> array ~loc ~attrs (List.map (sub # expr) el)
    | Pexp_ifthenelse (e1, e2, e3) ->
        ifthenelse ~loc ~attrs (sub # expr e1) (sub # expr e2)
                   (map_opt (sub # expr) e3)
    | Pexp_sequence (e1, e2) ->
        sequence ~loc ~attrs (sub # expr e1) (sub # expr e2)
    | Pexp_while (e1, e2) -> while_ ~loc ~attrs (sub # expr e1) (sub # expr e2)
    | Pexp_for (id, e1, e2, d, e3) ->
        for_ ~loc ~attrs (map_loc sub id) (sub # expr e1) (sub # expr e2) d
             (sub # expr e3)
    | Pexp_coerce (e, t1, t2) ->
        coerce ~loc ~attrs (sub # expr e) (map_opt (sub # typ) t1)
               (sub # typ t2)
    | Pexp_constraint (e, t) ->
        constraint_ ~loc ~attrs (sub # expr e) (sub # typ t)
    | Pexp_send (e, s) -> send ~loc ~attrs (sub # expr e) s
    | Pexp_new lid -> new_ ~loc ~attrs (map_loc sub lid)
    | Pexp_setinstvar (s, e) ->
        setinstvar ~loc ~attrs (map_loc sub s) (sub # expr e)
    | Pexp_override sel ->
        override ~loc ~attrs
                 (List.map (map_tuple (map_loc sub) (sub # expr)) sel)
    | Pexp_letmodule (s, me, e) ->
        letmodule ~loc ~attrs (map_loc sub s) (sub # module_expr me)
                  (sub # expr e)
    | Pexp_assert e -> assert_ ~loc ~attrs (sub # expr e)
    | Pexp_lazy e -> lazy_ ~loc ~attrs (sub # expr e)
    | Pexp_poly (e, t) ->
        poly ~loc ~attrs (sub # expr e) (map_opt (sub # typ) t)
    | Pexp_object cls -> object_ ~loc ~attrs (sub # class_structure cls)
    | Pexp_newtype (s, e) -> newtype ~loc ~attrs s (sub # expr e)
    | Pexp_pack me -> pack ~loc ~attrs (sub # module_expr me)
    | Pexp_open (ovf, lid, e) ->
        open_ ~loc ~attrs ovf (map_loc sub lid) (sub # expr e)
    | Pexp_extension x -> extension ~loc ~attrs (sub # extension x)
end

module P = struct
  (* Patterns *)

  let map sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
    let open Pat in
    let loc = sub # location loc in
    let attrs = sub # attributes attrs in
    match desc with
    | Ppat_any -> any ~loc ~attrs ()
    | Ppat_var s -> var ~loc ~attrs (map_loc sub s)
    | Ppat_alias (p, s) -> alias ~loc ~attrs (sub # pat p) (map_loc sub s)
    | Ppat_constant c -> constant ~loc ~attrs c
    | Ppat_interval (c1, c2) -> interval ~loc ~attrs c1 c2
    | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub # pat) pl)
    | Ppat_construct (l, p) ->
        construct ~loc ~attrs (map_loc sub l) (map_opt (sub # pat) p)
    | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub # pat) p)
    | Ppat_record (lpl, cf) ->
        record ~loc ~attrs (List.map (map_tuple (map_loc sub) (sub # pat)) lpl)
               cf
    | Ppat_array pl -> array ~loc ~attrs (List.map (sub # pat) pl)
    | Ppat_or (p1, p2) -> or_ ~loc ~attrs (sub # pat p1) (sub # pat p2)
    | Ppat_constraint (p, t) ->
        constraint_ ~loc ~attrs (sub # pat p) (sub # typ t)
    | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
    | Ppat_lazy p -> lazy_ ~loc ~attrs (sub # pat p)
    | Ppat_unpack s -> unpack ~loc ~attrs (map_loc sub s)
    | Ppat_extension x -> extension ~loc ~attrs (sub # extension x)
end

module CE = struct
  (* Value expressions for the class language *)

  let map sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
    let open Cl in
    let loc = sub # location loc in
    match desc with
    | Pcl_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub # typ) tys)
    | Pcl_structure s ->
        structure ~loc ~attrs (sub # class_structure s)
    | Pcl_fun (lab, e, p, ce) ->
        fun_ ~loc ~attrs lab
          (map_opt (sub # expr) e)
          (sub # pat p)
          (sub # class_expr ce)
    | Pcl_apply (ce, l) ->
        apply ~loc ~attrs (sub # class_expr ce)
              (List.map (map_snd (sub # expr)) l)
    | Pcl_let (r, vbs, ce) ->
        let_ ~loc ~attrs r (List.map (sub # value_binding) vbs)
             (sub # class_expr ce)
    | Pcl_constraint (ce, ct) ->
        constraint_ ~loc ~attrs (sub # class_expr ce) (sub # class_type ct)
    | Pcl_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_kind sub = function
    | Cfk_concrete (o, e) -> Cfk_concrete (o, sub # expr e)
    | Cfk_virtual t -> Cfk_virtual (sub # typ t)

  let map_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
    let open Cf in
    let loc = sub # location loc in
    match desc with
    | Pcf_inherit (o, ce, s) -> inherit_ ~loc ~attrs o (sub # class_expr ce) s
    | Pcf_val (s, m, k) -> val_ ~loc ~attrs (map_loc sub s) m (map_kind sub k)
    | Pcf_method (s, p, k) ->
        method_ ~loc ~attrs (map_loc sub s) p (map_kind sub k)
    | Pcf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub # typ t1) (sub # typ t2)
    | Pcf_initializer e -> initializer_ ~loc ~attrs (sub # expr e)
    | Pcf_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_structure sub {pcstr_self; pcstr_fields} =
    {
     pcstr_self = sub # pat pcstr_self;
     pcstr_fields = List.map (sub # class_field) pcstr_fields;
    }

  let class_infos sub f {pci_virt; pci_params = pl; pci_name; pci_expr;
                         pci_loc; pci_attributes} =
    Ci.mk
     ~virt:pci_virt
     ~params:(List.map (map_fst (map_loc sub)) pl)
      (map_loc sub pci_name)
      (f pci_expr)
      ~loc:(sub # location pci_loc)
      ~attrs:(sub # attributes pci_attributes)
end

(* Now, a generic AST mapper class, to be extended to cover all kinds
   and cases of the OCaml grammar.  The default behavior of the mapper
   is the identity. *)

class mapper =
  object(this)
    method implementation (input_name : string) ast =
      (input_name, this # structure ast)
    method interface (input_name: string) ast =
      (input_name, this # signature ast)
    method structure l = List.map (this # structure_item) l
    method structure_item si = M.map_structure_item this si
    method module_expr = M.map this

    method signature l = List.map (this # signature_item) l
    method signature_item si = MT.map_signature_item this si
    method module_type = MT.map this
    method with_constraint c = MT.map_with_constraint this c

    method class_declaration = CE.class_infos this (this # class_expr)
    method class_expr = CE.map this
    method class_field = CE.map_field this
    method class_structure = CE.map_structure this

    method class_type = CT.map this
    method class_type_field = CT.map_field this
    method class_signature = CT.map_signature this

    method class_type_declaration = CE.class_infos this (this # class_type)
    method class_description = CE.class_infos this (this # class_type)

    method type_declaration = T.map_type_declaration this
    method type_kind = T.map_type_kind this
    method typ = T.map this

    method value_description {pval_name; pval_type; pval_prim; pval_loc;
                              pval_attributes} =
      Val.mk
        (map_loc this pval_name)
        (this # typ pval_type)
        ~attrs:(this # attributes pval_attributes)
        ~loc:(this # location pval_loc)
        ~prim:pval_prim

    method pat = P.map this
    method expr = E.map this

    method module_declaration {pmd_name; pmd_type; pmd_attributes} =
      Md.mk
        (map_loc this pmd_name)
        (this # module_type pmd_type)
        ~attrs:(this # attributes pmd_attributes)

    method module_type_declaration {pmtd_name; pmtd_type; pmtd_attributes} =
      {
       pmtd_name = map_loc this pmtd_name;
       pmtd_type = map_opt (this # module_type) pmtd_type;
       pmtd_attributes = this # attributes pmtd_attributes;
      }

    method module_binding {pmb_name; pmb_expr; pmb_attributes} =
      Mb.mk (map_loc this pmb_name) (this # module_expr pmb_expr)
        ~attrs:(this # attributes pmb_attributes)

    method value_binding {pvb_pat; pvb_expr; pvb_attributes} =
      Vb.mk
        (this # pat pvb_pat)
        (this # expr pvb_expr)
        ~attrs:(this # attributes pvb_attributes)


    method constructor_declaration {pcd_name; pcd_args; pcd_res; pcd_loc;
                                    pcd_attributes} =
      Type.constructor
        (map_loc this pcd_name)
        ~args:(List.map (this # typ) pcd_args)
        ?res:(map_opt (this # typ) pcd_res)
        ~loc:(this # location pcd_loc)
        ~attrs:(this # attributes pcd_attributes)

    method label_declaration {pld_name; pld_type; pld_loc; pld_mutable;
                              pld_attributes} =
      Type.field
        (map_loc this pld_name)
        (this # typ pld_type)
        ~mut:pld_mutable
        ~loc:(this # location pld_loc)
        ~attrs:(this # attributes pld_attributes)

    method cases l = List.map (this # case) l
    method case {pc_lhs; pc_guard; pc_rhs} =
      {
       pc_lhs = this # pat pc_lhs;
       pc_guard = map_opt (this # expr) pc_guard;
       pc_rhs = this # expr pc_rhs;
    }



    method location l = l

    method extension (s, e) = (map_loc this s, this # payload e)
    method attribute (s, e) = (map_loc this s, this # payload e)
    method attributes l = List.map (this # attribute) l
    method payload = function
      | PStr x -> PStr (this # structure x)
      | PTyp x -> PTyp (this # typ x)
      | PPat (x, g) -> PPat (this # pat x, map_opt (this # expr) g)
  end

class type main_entry_points =
  object
    method implementation: string -> structure -> string * structure
    method interface: string -> signature -> string * signature
  end

let apply ~source ~target mapper =
  let ic = open_in_bin source in
  let magic = String.create (String.length ast_impl_magic_number) in
  really_input ic magic 0 (String.length magic);
  if magic <> ast_impl_magic_number && magic <> ast_intf_magic_number then
    failwith "Bad magic";
  let input_name = input_value ic in
  let ast = input_value ic in
  close_in ic;

  let (input_name, ast) =
    if magic = ast_impl_magic_number
    then Obj.magic (mapper # implementation input_name (Obj.magic ast))
    else Obj.magic (mapper # interface input_name (Obj.magic ast))
  in
  let oc = open_out_bin target in
  output_string oc magic;
  output_value oc input_name;
  output_value oc ast;
  close_out oc

let run_main mapper =
  try
    let a = Sys.argv in
    let n = Array.length a in
    if n > 2 then
      apply ~source:a.(n - 2) ~target:a.(n - 1)
            (mapper (Array.to_list (Array.sub a 1 (n - 3))))
    else begin
      Printf.eprintf "Usage: %s [extra_args] <infile> <outfile>\n%!"
                     Sys.executable_name;
      exit 1
    end
  with exn ->
    prerr_endline (Printexc.to_string exn);
    exit 2

let main mapper = run_main (fun _ -> mapper)

let register_function = ref (fun _name f -> run_main f)
let register name f = !register_function name (f :> string list -> mapper)
