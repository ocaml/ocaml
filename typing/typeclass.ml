(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Parsetree
open Asttypes
open Path
open Types
open Typecore
open Typetexp
open Format

type 'a class_info = {
  cls_id : Ident.t;
  cls_id_loc : string loc;
  cls_decl : class_declaration;
  cls_ty_id : Ident.t;
  cls_ty_decl : class_type_declaration;
  cls_obj_id : Ident.t;
  cls_obj_abbr : type_declaration;
  cls_abbr : type_declaration;
  cls_arity : int;
  cls_pub_methods : string list;
  cls_info : 'a;
}

type class_type_info = {
  clsty_ty_id : Ident.t;
  clsty_id_loc : string loc;
  clsty_ty_decl : class_type_declaration;
  clsty_obj_id : Ident.t;
  clsty_obj_abbr : type_declaration;
  clsty_abbr : type_declaration;
  clsty_info : Typedtree.class_type_declaration;
}

type 'a full_class = {
  id : Ident.t;
  id_loc : tag loc;
  clty: class_declaration;
  ty_id: Ident.t;
  cltydef: class_type_declaration;
  obj_id: Ident.t;
  obj_abbr: type_declaration;
  arity: int;
  pub_meths: string list;
  coe: Warnings.loc list;
  req: 'a Typedtree.class_infos;
}

type kind =
  | Object
  | Class
  | Class_type

type final =
  | Final
  | Not_final

let kind_of_final = function
  | Final -> Object
  | Not_final -> Class

type error =
  | Unconsistent_constraint of Errortrace.unification_error
  | Field_type_mismatch of string * string * Errortrace.unification_error
  | Unexpected_field of type_expr * string
  | Structure_expected of class_type
  | Cannot_apply of class_type
  | Apply_wrong_label of arg_label
  | Pattern_type_clash of type_expr
  | Repeated_parameter
  | Unbound_class_2 of Longident.t
  | Unbound_class_type_2 of Longident.t
  | Abbrev_type_clash of type_expr * type_expr * type_expr
  | Constructor_type_mismatch of string * Errortrace.unification_error
  | Virtual_class of kind * string list * string list
  | Undeclared_methods of kind * string list
  | Parameter_arity_mismatch of Longident.t * int * int
  | Parameter_mismatch of Errortrace.unification_error
  | Bad_parameters of Ident.t * type_expr * type_expr
  | Class_match_failure of Ctype.class_match_failure list
  | Unbound_val of string
  | Unbound_type_var of
      (formatter -> unit) * (type_expr * bool * string * type_expr)
  | Non_generalizable_class of Ident.t * Types.class_declaration
  | Cannot_coerce_self of type_expr
  | Non_collapsable_conjunction of
      Ident.t * Types.class_declaration * Errortrace.unification_error
  | Self_clash of Errortrace.unification_error
  | Mutability_mismatch of string * mutable_flag
  | No_overriding of string * string
  | Duplicate of string * string
  | Closing_self_type of class_signature

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

open Typedtree

let type_open_descr :
  (?used_slot:bool ref -> Env.t -> Parsetree.open_description
   -> open_description * Env.t) ref =
  ref (fun ?used_slot:_ _ -> assert false)

let ctyp desc typ env loc =
  { ctyp_desc = desc; ctyp_type = typ; ctyp_loc = loc; ctyp_env = env;
    ctyp_attributes = [] }

(*
   Path associated to the temporary class type of a class being typed
   (its constructor is not available).
*)
let unbound_class =
  Path.Pident (Ident.create_local "*undef*")


                (************************************)
                (*  Some operations on class types  *)
                (************************************)

let extract_constraints cty =
  let sign = Btype.signature_of_class_type cty in
  (Btype.instance_vars sign,
   Btype.methods sign,
   Btype.concrete_methods sign)

(* Record a class type *)
let rc node =
  Cmt_format.add_saved_type (Cmt_format.Partial_class_expr node);
  node

let update_class_signature loc env ~warn_implicit_public virt kind sign =
  let implicit_public, implicit_declared =
    Ctype.update_class_signature env sign
  in
  if implicit_declared <> [] then begin
    match virt with
    | Virtual -> () (* Should perhaps emit warning 17 here *)
    | Concrete ->
        raise (Error(loc, env, Undeclared_methods(kind, implicit_declared)))
  end;
  if warn_implicit_public && implicit_public <> [] then begin
    Location.prerr_warning
      loc (Warnings.Implicit_public_methods implicit_public)
  end

let complete_class_signature loc env virt kind sign =
  update_class_signature loc env ~warn_implicit_public:false virt kind sign;
  Ctype.hide_private_methods env sign

let complete_class_type loc env virt kind typ =
  let sign = Btype.signature_of_class_type typ in
  complete_class_signature loc env virt kind sign

let check_virtual loc env virt kind sign =
  match virt with
  | Virtual -> ()
  | Concrete ->
      match Btype.virtual_methods sign, Btype.virtual_instance_vars sign with
      | [], [] -> ()
      | meths, vars ->
          raise(Error(loc, env, Virtual_class(kind, meths, vars)))

let rec check_virtual_clty loc env virt kind clty =
  match clty with
  | Cty_constr(_, _, clty) | Cty_arrow(_, _, clty) ->
      check_virtual_clty loc env virt kind clty
  | Cty_signature sign ->
      check_virtual loc env virt kind sign

(* Return the constructor type associated to a class type *)
let rec constructor_type constr cty =
  match cty with
    Cty_constr (_, _, cty) ->
      constructor_type constr cty
  | Cty_signature _ ->
      constr
  | Cty_arrow (l, ty, cty) ->
      Ctype.newty (Tarrow (l, ty, constructor_type constr cty, commu_ok))

                (***********************************)
                (*  Primitives for typing classes  *)
                (***********************************)

let raise_add_method_failure loc env label sign failure =
  match (failure : Ctype.add_method_failure) with
  | Ctype.Unexpected_method ->
      raise(Error(loc, env, Unexpected_field (sign.Types.csig_self, label)))
  | Ctype.Type_mismatch trace ->
      raise(Error(loc, env, Field_type_mismatch ("method", label, trace)))

let raise_add_instance_variable_failure loc env label failure =
  match (failure : Ctype.add_instance_variable_failure) with
  | Ctype.Mutability_mismatch mut ->
      raise (Error(loc, env, Mutability_mismatch(label, mut)))
  | Ctype.Type_mismatch trace ->
      raise (Error(loc, env,
        Field_type_mismatch("instance variable", label, trace)))

let raise_inherit_class_signature_failure loc env sign = function
  | Ctype.Self_type_mismatch trace ->
      raise(Error(loc, env, Self_clash trace))
  | Ctype.Method(label, failure) ->
      raise_add_method_failure loc env label sign failure
  | Ctype.Instance_variable(label, failure) ->
      raise_add_instance_variable_failure loc env label failure

let add_method loc env label priv virt ty sign =
  match Ctype.add_method env label priv virt ty sign with
  | () -> ()
  | exception Ctype.Add_method_failed failure ->
      raise_add_method_failure loc env label sign failure

let add_instance_variable ~strict loc env label mut virt ty sign =
  match Ctype.add_instance_variable ~strict env label mut virt ty sign with
  | () -> ()
  | exception Ctype.Add_instance_variable_failed failure ->
      raise_add_instance_variable_failure loc env label failure

let inherit_class_signature ~strict loc env sign1 sign2 =
  match Ctype.inherit_class_signature ~strict env sign1 sign2 with
  | () -> ()
  | exception Ctype.Inherit_class_signature_failed failure ->
      raise_inherit_class_signature_failure loc env sign1 failure

let inherit_class_type ~strict loc env sign1 cty2 =
  let sign2 =
    match Btype.scrape_class_type cty2 with
    | Cty_signature sign2 -> sign2
    | _ ->
      raise(Error(loc, env, Structure_expected cty2))
  in
  inherit_class_signature ~strict loc env sign1 sign2

let unify_delayed_method_type loc env label ty expected_ty=
  match Ctype.unify env ty expected_ty with
  | () -> ()
  | exception Ctype.Unify trace ->
      raise(Error(loc, env, Field_type_mismatch ("method", label, trace)))

let type_constraint val_env sty sty' loc =
  let cty  = transl_simple_type val_env false sty in
  let ty = cty.ctyp_type in
  let cty' = transl_simple_type val_env false sty' in
  let ty' = cty'.ctyp_type in
  begin
    try Ctype.unify val_env ty ty' with Ctype.Unify err ->
        raise(Error(loc, val_env, Unconsistent_constraint err));
  end;
  (cty, cty')

let make_method loc cl_num expr =
  let open Ast_helper in
  let mkid s = mkloc s loc in
  Exp.fun_ ~loc:expr.pexp_loc Nolabel None
    (Pat.alias ~loc (Pat.var ~loc (mkid "self-*")) (mkid ("self-" ^ cl_num)))
    expr

(*******************************)

let delayed_meth_specs = ref []

let rec class_type_field env sign self_scope ctf =
  let loc = ctf.pctf_loc in
  let mkctf desc =
    { ctf_desc = desc; ctf_loc = loc; ctf_attributes = ctf.pctf_attributes }
  in
  let mkctf_with_attrs f =
    Builtin_attributes.warning_scope ctf.pctf_attributes
      (fun () -> mkctf (f ()))
  in
  match ctf.pctf_desc with
  | Pctf_inherit sparent ->
      mkctf_with_attrs
        (fun () ->
          let parent = class_type env Virtual self_scope sparent in
          complete_class_type parent.cltyp_loc
            env Virtual Class_type parent.cltyp_type;
          inherit_class_type ~strict:false loc env sign parent.cltyp_type;
          Tctf_inherit parent)
  | Pctf_val ({txt=lab}, mut, virt, sty) ->
      mkctf_with_attrs
        (fun () ->
          let cty = transl_simple_type env false sty in
          let ty = cty.ctyp_type in
          add_instance_variable ~strict:false loc env lab mut virt ty sign;
          Tctf_val (lab, mut, virt, cty))

  | Pctf_method ({txt=lab}, priv, virt, sty)  ->
      mkctf_with_attrs
        (fun () ->
           let sty = Ast_helper.Typ.force_poly sty in
           match sty.ptyp_desc, priv with
           | Ptyp_poly ([],sty'), Public ->
               let expected_ty = Ctype.newvar () in
               add_method loc env lab priv virt expected_ty sign;
               let returned_cty = ctyp Ttyp_any (Ctype.newty Tnil) env loc in
               delayed_meth_specs :=
                 Warnings.mk_lazy (fun () ->
                   let cty = transl_simple_type_univars env sty' in
                   let ty = cty.ctyp_type in
                   unify_delayed_method_type loc env lab ty expected_ty;
                   returned_cty.ctyp_desc <- Ttyp_poly ([], cty);
                   returned_cty.ctyp_type <- ty;
                 ) :: !delayed_meth_specs;
               Tctf_method (lab, priv, virt, returned_cty)
           | _ ->
               let cty = transl_simple_type env false sty in
               let ty = cty.ctyp_type in
               add_method loc env lab priv virt ty sign;
               Tctf_method (lab, priv, virt, cty))

  | Pctf_constraint (sty, sty') ->
      mkctf_with_attrs
        (fun () ->
           let (cty, cty') = type_constraint env sty sty' ctf.pctf_loc in
           Tctf_constraint (cty, cty'))

  | Pctf_attribute x ->
      Builtin_attributes.warning_attribute x;
      mkctf (Tctf_attribute x)

  | Pctf_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))

and class_signature virt env pcsig self_scope loc =
  let {pcsig_self=sty; pcsig_fields=psign} = pcsig in
  let sign = Ctype.new_class_signature () in
  (* Introduce a dummy method preventing self type from being closed. *)
  Ctype.add_dummy_method env ~scope:self_scope sign;

  let self_cty = transl_simple_type env false sty in
  let self_type = self_cty.ctyp_type in
  begin try
    Ctype.unify env self_type sign.csig_self
  with Ctype.Unify _ ->
    raise(Error(sty.ptyp_loc, env, Pattern_type_clash self_type))
  end;

  (* Class type fields *)
  let fields =
    Builtin_attributes.warning_scope []
      (fun () -> List.map (class_type_field env sign self_scope) psign)
  in
  check_virtual loc env virt Class_type sign;
  { csig_self = self_cty;
    csig_fields = fields;
    csig_type = sign; }

and class_type env virt self_scope scty =
  Builtin_attributes.warning_scope scty.pcty_attributes
    (fun () -> class_type_aux env virt self_scope scty)

and class_type_aux env virt self_scope scty =
  let cltyp desc typ =
    {
     cltyp_desc = desc;
     cltyp_type = typ;
     cltyp_loc = scty.pcty_loc;
     cltyp_env = env;
     cltyp_attributes = scty.pcty_attributes;
    }
  in
  match scty.pcty_desc with
  | Pcty_constr (lid, styl) ->
      let (path, decl) = Env.lookup_cltype ~loc:scty.pcty_loc lid.txt env in
      if Path.same decl.clty_path unbound_class then
        raise(Error(scty.pcty_loc, env, Unbound_class_type_2 lid.txt));
      let (params, clty) =
        Ctype.instance_class decl.clty_params decl.clty_type
      in
      (* Adding a dummy method to the self type prevents it from being closed /
         escaping. *)
      Ctype.add_dummy_method env ~scope:self_scope
        (Btype.signature_of_class_type clty);
      if List.length params <> List.length styl then
        raise(Error(scty.pcty_loc, env,
                    Parameter_arity_mismatch (lid.txt, List.length params,
                                                   List.length styl)));
      let ctys = List.map2
        (fun sty ty ->
          let cty' = transl_simple_type env false sty in
          let ty' = cty'.ctyp_type in
          begin
           try Ctype.unify env ty' ty with Ctype.Unify err ->
                  raise(Error(sty.ptyp_loc, env, Parameter_mismatch err))
            end;
            cty'
        )       styl params
      in
      let typ = Cty_constr (path, params, clty) in
      (* Check for unexpected virtual methods *)
      check_virtual_clty scty.pcty_loc env virt Class_type typ;
      cltyp (Tcty_constr ( path, lid , ctys)) typ

  | Pcty_signature pcsig ->
      let clsig = class_signature virt env pcsig self_scope scty.pcty_loc in
      let typ = Cty_signature clsig.csig_type in
      cltyp (Tcty_signature clsig) typ

  | Pcty_arrow (l, sty, scty) ->
      let cty = transl_simple_type env false sty in
      let ty = cty.ctyp_type in
      let ty =
        if Btype.is_optional l
        then Ctype.newty (Tconstr(Predef.path_option,[ty], ref Mnil))
        else ty in
      let clty = class_type env virt self_scope scty in
      let typ = Cty_arrow (l, ty, clty.cltyp_type) in
      cltyp (Tcty_arrow (l, cty, clty)) typ

  | Pcty_open (od, e) ->
      let (od, newenv) = !type_open_descr env od in
      let clty = class_type newenv virt self_scope e in
      cltyp (Tcty_open (od, clty)) clty.cltyp_type

  | Pcty_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))

let class_type env virt self_scope scty =
  delayed_meth_specs := [];
  let cty = class_type env virt self_scope scty in
  List.iter Lazy.force (List.rev !delayed_meth_specs);
  delayed_meth_specs := [];
  cty

(*******************************)

let enter_ancestor_val name val_env =
  Env.enter_unbound_value name Val_unbound_ancestor val_env

let enter_self_val name val_env =
  Env.enter_unbound_value name Val_unbound_self val_env

let enter_instance_var_val name val_env =
  Env.enter_unbound_value name Val_unbound_instance_variable val_env

let enter_ancestor_met ~loc name ~sign ~meths ~cl_num ~ty ~attrs met_env =
  let check s = Warnings.Unused_ancestor s in
  let kind = Val_anc (sign, meths, cl_num) in
  let desc =
    { val_type = ty; val_kind = kind;
      val_attributes = attrs;
      Types.val_loc = loc;
      val_uid = Uid.mk ~current_unit:(Env.get_unit_name ()) }
  in
  Env.enter_value ~check name desc met_env

let add_self_met loc id sign self_var_kind vars cl_num
      as_var ty attrs met_env =
  let check =
    if as_var then (fun s -> Warnings.Unused_var s)
    else (fun s -> Warnings.Unused_var_strict s)
  in
  let kind = Val_self (sign, self_var_kind, vars, cl_num) in
  let desc =
    { val_type = ty; val_kind = kind;
      val_attributes = attrs;
      Types.val_loc = loc;
      val_uid = Uid.mk ~current_unit:(Env.get_unit_name ()) }
  in
  Env.add_value ~check id desc met_env

let add_instance_var_met loc label id sign cl_num attrs met_env =
  let mut, ty =
    match Vars.find label sign.csig_vars with
    | (mut, _, ty) -> mut, ty
    | exception Not_found -> assert false
  in
  let kind = Val_ivar (mut, cl_num) in
  let desc =
    { val_type = ty; val_kind = kind;
      val_attributes = attrs;
      Types.val_loc = loc;
      val_uid = Uid.mk ~current_unit:(Env.get_unit_name ()) }
  in
  Env.add_value id desc met_env

let add_instance_vars_met loc vars sign cl_num met_env =
  List.fold_left
    (fun met_env (label, id) ->
       add_instance_var_met loc label id sign cl_num [] met_env)
    met_env vars

type intermediate_class_field =
  | Inherit of
      { override : override_flag;
        parent : class_expr;
        super : string option;
        inherited_vars : (string * Ident.t) list;
        super_meths : (string * Ident.t) list;
        loc : Location.t;
        attributes : attribute list; }
  | Virtual_val of
      { label : string loc;
        mut : mutable_flag;
        id : Ident.t;
        cty : core_type;
        already_declared : bool;
        loc : Location.t;
        attributes : attribute list; }
  | Concrete_val of
      { label : string loc;
        mut : mutable_flag;
        id : Ident.t;
        override : override_flag;
        definition : expression;
        already_declared : bool;
        loc : Location.t;
        attributes : attribute list; }
  | Virtual_method of
      { label : string loc;
        priv : private_flag;
        cty : core_type;
        loc : Location.t;
        attributes : attribute list; }
  | Concrete_method of
      { label : string loc;
        priv : private_flag;
        override : override_flag;
        sdefinition : Parsetree.expression;
        warning_state : Warnings.state;
        loc : Location.t;
        attributes : attribute list; }
  | Constraint of
      { cty1 : core_type;
        cty2 : core_type;
        loc : Location.t;
        attributes : attribute list; }
  | Initializer of
      { sexpr : Parsetree.expression;
        warning_state : Warnings.state;
        loc : Location.t;
        attributes : attribute list; }
  | Attribute of
      { attribute : attribute;
        loc : Location.t;
        attributes : attribute list; }

type first_pass_accummulater =
  { rev_fields : intermediate_class_field list;
    val_env : Env.t;
    par_env : Env.t;
    concrete_meths : MethSet.t;
    concrete_vals : VarSet.t;
    local_meths : MethSet.t;
    local_vals : VarSet.t;
    vars : Ident.t Vars.t; }

let rec class_field_first_pass self_loc cl_num sign self_scope acc cf =
  let { rev_fields; val_env; par_env; concrete_meths; concrete_vals;
        local_meths; local_vals; vars } = acc
  in
  let loc = cf.pcf_loc in
  let attributes = cf.pcf_attributes in
  let with_attrs f = Builtin_attributes.warning_scope attributes f in
  match cf.pcf_desc with
  | Pcf_inherit (override, sparent, super) ->
      with_attrs
        (fun () ->
           let parent =
             class_expr cl_num val_env par_env
               Virtual self_scope sparent
           in
           complete_class_type parent.cl_loc
             par_env Virtual Class parent.cl_type;
           inherit_class_type ~strict:true loc val_env sign parent.cl_type;
           let parent_sign = Btype.signature_of_class_type parent.cl_type in
           let new_concrete_meths = Btype.concrete_methods parent_sign in
           let new_concrete_vals = Btype.concrete_instance_vars parent_sign in
           let over_meths = MethSet.inter new_concrete_meths concrete_meths in
           let over_vals = VarSet.inter new_concrete_vals concrete_vals in
           begin match override with
           | Fresh ->
               let cname =
                 match parent.cl_type with
                 | Cty_constr (p, _, _) -> Path.name p
                 | _ -> "inherited"
               in
               if not (MethSet.is_empty over_meths) then
                 Location.prerr_warning loc
                   (Warnings.Method_override
                      (cname :: MethSet.elements over_meths));
               if not (VarSet.is_empty over_vals) then
                 Location.prerr_warning loc
                   (Warnings.Instance_variable_override
                      (cname :: VarSet.elements over_vals));
           | Override ->
               if MethSet.is_empty over_meths && VarSet.is_empty over_vals then
                 raise (Error(loc, val_env, No_overriding ("","")))
           end;
           let concrete_vals = VarSet.union new_concrete_vals concrete_vals in
           let concrete_meths =
             MethSet.union new_concrete_meths concrete_meths
           in
           let val_env, par_env, inherited_vars, vars =
             Vars.fold
               (fun label _ (val_env, par_env, inherited_vars, vars) ->
                  let val_env = enter_instance_var_val label val_env in
                  let par_env = enter_instance_var_val label par_env in
                  let id = Ident.create_local label in
                  let inherited_vars = (label, id) :: inherited_vars in
                  let vars = Vars.add label id vars in
                  (val_env, par_env, inherited_vars, vars))
               parent_sign.csig_vars (val_env, par_env, [], vars)
           in
           (* Methods available through super *)
           let super_meths =
             MethSet.fold
               (fun label acc -> (label, Ident.create_local label) :: acc)
               new_concrete_meths []
           in
           (* Super *)
           let (val_env, par_env, super) =
             match super with
             | None -> (val_env, par_env, None)
             | Some {txt=name} ->
                 let val_env = enter_ancestor_val name val_env in
                 let par_env = enter_ancestor_val name par_env in
                 (val_env, par_env, Some name)
           in
           let field =
             Inherit
               { override; parent; super; inherited_vars;
                 super_meths; loc; attributes }
           in
           let rev_fields = field :: rev_fields in
           { acc with rev_fields; val_env; par_env;
                      concrete_meths; concrete_vals; vars })
  | Pcf_val (label, mut, Cfk_virtual styp) ->
      with_attrs
        (fun () ->
           if !Clflags.principal then Ctype.begin_def ();
           let cty = Typetexp.transl_simple_type val_env false styp in
           let ty = cty.ctyp_type in
           if !Clflags.principal then begin
             Ctype.end_def ();
             Ctype.generalize_structure ty
           end;
           add_instance_variable ~strict:true loc val_env
             label.txt mut Virtual ty sign;
           let already_declared, val_env, par_env, id, vars =
             match Vars.find label.txt vars with
             | id -> true, val_env, par_env, id, vars
             | exception Not_found ->
                 let name = label.txt in
                 let val_env = enter_instance_var_val name val_env in
                 let par_env = enter_instance_var_val name par_env in
                 let id = Ident.create_local name in
                 let vars = Vars.add label.txt id vars in
                 false, val_env, par_env, id, vars
           in
           let field =
             Virtual_val
               { label; mut; id; cty; already_declared; loc; attributes }
           in
           let rev_fields = field :: rev_fields in
           { acc with rev_fields; val_env; par_env; vars })
  | Pcf_val (label, mut, Cfk_concrete (override, sdefinition)) ->
      with_attrs
        (fun () ->
           if VarSet.mem label.txt local_vals then
             raise(Error(loc, val_env,
                         Duplicate ("instance variable", label.txt)));
           if VarSet.mem label.txt concrete_vals then begin
             if override = Fresh then
               Location.prerr_warning label.loc
                 (Warnings.Instance_variable_override[label.txt])
           end else begin
             if override = Override then
               raise(Error(loc, val_env,
                           No_overriding ("instance variable", label.txt)))
           end;
           if !Clflags.principal then Ctype.begin_def ();
           let definition = type_exp val_env sdefinition in
           if !Clflags.principal then begin
             Ctype.end_def ();
             Ctype.generalize_structure definition.exp_type
           end;
           add_instance_variable ~strict:true loc val_env
             label.txt mut Concrete definition.exp_type sign;
           let already_declared, val_env, par_env, id, vars =
             match Vars.find label.txt vars with
             | id -> true, val_env, par_env, id, vars
             | exception Not_found ->
                 let name = label.txt in
                 let val_env = enter_instance_var_val name val_env in
                 let par_env = enter_instance_var_val name par_env in
                 let id = Ident.create_local name in
                 let vars = Vars.add label.txt id vars in
                 false, val_env, par_env, id, vars
           in
           let field =
             Concrete_val
               { label; mut; id; override; definition;
                 already_declared; loc; attributes }
           in
           let rev_fields = field :: rev_fields in
           let concrete_vals = VarSet.add label.txt concrete_vals in
           let local_vals = VarSet.add label.txt local_vals in
           { acc with rev_fields; val_env; par_env;
                      concrete_vals; local_vals; vars })

  | Pcf_method (label, priv, Cfk_virtual sty) ->
      with_attrs
        (fun () ->
           let sty = Ast_helper.Typ.force_poly sty in
           let cty = transl_simple_type val_env false sty in
           let ty = cty.ctyp_type in
           add_method loc val_env label.txt priv Virtual ty sign;
           let field =
             Virtual_method { label; priv; cty; loc; attributes }
           in
           let rev_fields = field :: rev_fields in
           { acc with rev_fields })

  | Pcf_method (label, priv, Cfk_concrete (override, expr)) ->
      with_attrs
        (fun () ->
           if MethSet.mem label.txt local_meths then
             raise(Error(loc, val_env, Duplicate ("method", label.txt)));
           if MethSet.mem label.txt concrete_meths then begin
             if override = Fresh then begin
                 Location.prerr_warning loc
                   (Warnings.Method_override [label.txt])
             end
           end else begin
             if override = Override then begin
               raise(Error(loc, val_env, No_overriding("method", label.txt)))
             end
           end;
           let expr =
             match expr.pexp_desc with
             | Pexp_poly _ -> expr
             | _ -> Ast_helper.Exp.poly ~loc:expr.pexp_loc expr None
           in
           let sbody, sty =
             match expr.pexp_desc with
             | Pexp_poly (sbody, sty) -> sbody, sty
             | _ -> assert false
           in
           let ty =
             match sty with
             | None -> Ctype.newvar ()
             | Some sty ->
                 let sty = Ast_helper.Typ.force_poly sty in
                 let cty' =
                   Typetexp.transl_simple_type val_env false sty
                 in
                 cty'.ctyp_type
           in
           add_method loc val_env label.txt priv Concrete ty sign;
           begin
             try
               match get_desc ty with
               | Tvar _ ->
                   let ty' = Ctype.newvar () in
                   Ctype.unify val_env (Ctype.newty (Tpoly (ty', []))) ty;
                   Ctype.unify val_env (type_approx val_env sbody) ty'
               | Tpoly (ty1, tl) ->
                   let _, ty1' = Ctype.instance_poly false tl ty1 in
                   let ty2 = type_approx val_env sbody in
                   Ctype.unify val_env ty2 ty1'
               | _ -> assert false
             with Ctype.Unify err ->
               raise(Error(loc, val_env,
                           Field_type_mismatch ("method", label.txt, err)))
           end;
           let sdefinition = make_method self_loc cl_num expr in
           let warning_state = Warnings.backup () in
           let field =
             Concrete_method
               { label; priv; override; sdefinition;
                 warning_state; loc; attributes }
           in
           let rev_fields = field :: rev_fields in
           let concrete_meths = MethSet.add label.txt concrete_meths in
           let local_meths = MethSet.add label.txt local_meths in
           { acc with rev_fields; concrete_meths; local_meths })

  | Pcf_constraint (sty1, sty2) ->
      with_attrs
        (fun () ->
           let (cty1, cty2) = type_constraint val_env sty1 sty2 loc in
           let field =
             Constraint { cty1; cty2; loc; attributes }
           in
           let rev_fields = field :: rev_fields in
           { acc with rev_fields })

  | Pcf_initializer sexpr ->
      with_attrs
        (fun () ->
           let sexpr = make_method self_loc cl_num sexpr in
           let warning_state = Warnings.backup () in
           let field =
             Initializer { sexpr; warning_state; loc; attributes }
           in
           let rev_fields = field :: rev_fields in
           { acc with rev_fields })
  | Pcf_attribute attribute ->
      Builtin_attributes.warning_attribute attribute;
      let field = Attribute { attribute; loc; attributes } in
      let rev_fields = field :: rev_fields in
      { acc with rev_fields }
  | Pcf_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))

and class_fields_first_pass self_loc cl_num sign self_scope
      val_env par_env cfs =
  let rev_fields = [] in
  let concrete_meths = MethSet.empty in
  let concrete_vals = VarSet.empty in
  let local_meths = MethSet.empty in
  let local_vals = VarSet.empty in
  let vars = Vars.empty in
  let init_acc =
    { rev_fields; val_env; par_env;
      concrete_meths; concrete_vals;
      local_meths; local_vals; vars }
  in
  let acc =
    Builtin_attributes.warning_scope []
      (fun () ->
        List.fold_left
          (class_field_first_pass self_loc cl_num sign self_scope)
          init_acc cfs)
  in
  List.rev acc.rev_fields, acc.vars

and class_field_second_pass cl_num sign met_env field =
  let mkcf desc loc attrs =
    { cf_desc = desc; cf_loc = loc; cf_attributes = attrs }
  in
  match field with
  | Inherit { override; parent; super;
              inherited_vars; super_meths; loc; attributes } ->
      let met_env =
        add_instance_vars_met loc inherited_vars sign cl_num met_env
      in
      let met_env =
        match super with
        | None -> met_env
        | Some name ->
            let meths =
              List.fold_left
                (fun acc (label, id) -> Meths.add label id acc)
                Meths.empty super_meths
            in
            let ty = Btype.self_type parent.cl_type in
            let attrs = [] in
            let _id, met_env =
              enter_ancestor_met ~loc name ~sign ~meths
                ~cl_num ~ty ~attrs met_env
            in
            met_env
      in
      let desc =
        Tcf_inherit(override, parent, super, inherited_vars, super_meths)
      in
      met_env, mkcf desc loc attributes
  | Virtual_val { label; mut; id; cty; already_declared; loc; attributes } ->
      let met_env =
        if already_declared then met_env
        else begin
          add_instance_var_met loc label.txt id sign cl_num attributes met_env
        end
      in
      let kind = Tcfk_virtual cty in
      let desc = Tcf_val(label, mut, id, kind, already_declared) in
      met_env, mkcf desc loc attributes
  | Concrete_val { label; mut; id; override;
                   definition; already_declared; loc; attributes } ->
      let met_env =
        if already_declared then met_env
        else begin
          add_instance_var_met loc label.txt id sign cl_num attributes met_env
        end
      in
      let kind = Tcfk_concrete(override, definition) in
      let desc = Tcf_val(label, mut, id, kind, already_declared) in
      met_env, mkcf desc loc attributes
  | Virtual_method { label; priv; cty; loc; attributes } ->
      let kind = Tcfk_virtual cty in
      let desc = Tcf_method(label, priv, kind) in
      met_env, mkcf desc loc attributes
  | Concrete_method { label; priv; override;
                      sdefinition; warning_state; loc; attributes } ->
      Warnings.with_state warning_state
        (fun () ->
           let ty = Btype.method_type label.txt sign in
           let self_type = sign.Types.csig_self in
           let meth_type =
             mk_expected
               (Btype.newgenty (Tarrow(Nolabel, self_type, ty, commu_ok)))
           in
           Ctype.raise_nongen_level ();
           let texp = type_expect met_env sdefinition meth_type in
           Ctype.end_def ();
           let kind = Tcfk_concrete (override, texp) in
           let desc = Tcf_method(label, priv, kind) in
           met_env, mkcf desc loc attributes)
  | Constraint { cty1; cty2; loc; attributes } ->
      let desc = Tcf_constraint(cty1, cty2) in
      met_env, mkcf desc loc attributes
  | Initializer { sexpr; warning_state; loc; attributes } ->
      Warnings.with_state warning_state
        (fun () ->
           Ctype.raise_nongen_level ();
           let unit_type = Ctype.instance Predef.type_unit in
           let self_type = sign.Types.csig_self in
           let meth_type =
             mk_expected
               (Ctype.newty (Tarrow (Nolabel, self_type, unit_type, commu_ok)))
           in
           let texp = type_expect met_env sexpr meth_type in
           Ctype.end_def ();
           let desc = Tcf_initializer texp in
           met_env, mkcf desc loc attributes)
  | Attribute { attribute; loc; attributes; } ->
      let desc = Tcf_attribute attribute in
      met_env, mkcf desc loc attributes

and class_fields_second_pass cl_num sign met_env fields =
  let _, rev_cfs =
    List.fold_left
      (fun (met_env, cfs) field ->
         let met_env, cf =
           class_field_second_pass cl_num sign met_env field
         in
         met_env, cf :: cfs)
      (met_env, []) fields
  in
  List.rev rev_cfs

(* N.B. the self type of a final object type doesn't contain a dummy method in
   the beginning.
   We only explicitly add a dummy method to class definitions (and class (type)
   declarations)), which are later removed (made absent) by [final_decl].

   If we ever find a dummy method in a final object self type, it means that
   somehow we've unified the self type of the object with the self type of a not
   yet finished class.
   When this happens, we cannot close the object type and must error. *)
and class_structure cl_num virt self_scope final val_env met_env loc
  { pcstr_self = spat; pcstr_fields = str } =
  (* Environment for substructures *)
  let par_env = met_env in

  (* Location of self. Used for locations of self arguments *)
  let self_loc = {spat.ppat_loc with Location.loc_ghost = true} in

  let sign = Ctype.new_class_signature () in

  (* Adding a dummy method to the signature prevents it from being closed /
     escaping. That isn't needed for objects though. *)
  begin match final with
  | Not_final -> Ctype.add_dummy_method val_env ~scope:self_scope sign;
  | Final -> ()
  end;

  (* Self binder *)
  let (self_pat, self_pat_vars) = type_self_pattern val_env spat in
  let val_env, par_env =
    List.fold_right
      (fun {pv_id; _} (val_env, par_env) ->
         let name = Ident.name pv_id in
         let val_env = enter_self_val name val_env in
         let par_env = enter_self_val name par_env in
         val_env, par_env)
      self_pat_vars (val_env, par_env)
  in

  (* Check that the binder has a correct type *)
  begin try Ctype.unify val_env self_pat.pat_type sign.csig_self with
    Ctype.Unify _ ->
      raise(Error(spat.ppat_loc, val_env,
        Pattern_type_clash self_pat.pat_type))
  end;

  (* Typing of class fields *)
  let (fields, vars) =
    class_fields_first_pass self_loc cl_num sign self_scope
           val_env par_env str
  in
  let kind = kind_of_final final in

  (* Check for unexpected virtual methods *)
  check_virtual loc val_env virt kind sign;

  (* Update the class signature *)
  update_class_signature loc val_env
    ~warn_implicit_public:false virt kind sign;

  let meths =
    Meths.fold
      (fun label _ meths ->
         Meths.add label (Ident.create_local label) meths)
      sign.csig_meths Meths.empty
  in

  (* Close the signature if it is final *)
  begin match final with
  | Not_final -> ()
  | Final ->
      if not (Ctype.close_class_signature val_env sign) then
        raise(Error(loc, val_env, Closing_self_type sign));
  end;
  (* Typing of method bodies *)
  Ctype.generalize_class_signature_spine val_env sign;
  let self_var_kind =
    match virt with
    | Virtual -> Self_virtual(ref meths)
    | Concrete -> Self_concrete meths
  in
  let met_env =
    List.fold_right
      (fun {pv_id; pv_type; pv_loc; pv_as_var; pv_attributes} met_env ->
         add_self_met pv_loc pv_id sign self_var_kind vars
           cl_num pv_as_var pv_type pv_attributes met_env)
      self_pat_vars met_env
  in
  let fields =
    class_fields_second_pass cl_num sign met_env fields
  in

  (* Update the class signature and warn about public methods made private *)
  update_class_signature loc val_env
    ~warn_implicit_public:true virt kind sign;

  let meths =
    match self_var_kind with
    | Self_virtual meths_ref -> !meths_ref
    | Self_concrete meths -> meths
  in
  { cstr_self = self_pat;
    cstr_fields = fields;
    cstr_type = sign;
    cstr_meths = meths; }

and class_expr cl_num val_env met_env virt self_scope scl =
  Builtin_attributes.warning_scope scl.pcl_attributes
    (fun () -> class_expr_aux cl_num val_env met_env virt self_scope scl)

and class_expr_aux cl_num val_env met_env virt self_scope scl =
  match scl.pcl_desc with
  | Pcl_constr (lid, styl) ->
      let (path, decl) = Env.lookup_class ~loc:scl.pcl_loc lid.txt val_env in
      if Path.same decl.cty_path unbound_class then
        raise(Error(scl.pcl_loc, val_env, Unbound_class_2 lid.txt));
      let tyl = List.map
          (fun sty -> transl_simple_type val_env false sty)
          styl
      in
      let (params, clty) =
        Ctype.instance_class decl.cty_params decl.cty_type
      in
      let clty' = Btype.abbreviate_class_type path params clty in
      (* Adding a dummy method to the self type prevents it from being closed /
         escaping. *)
      Ctype.add_dummy_method val_env ~scope:self_scope
        (Btype.signature_of_class_type clty');
      if List.length params <> List.length tyl then
        raise(Error(scl.pcl_loc, val_env,
                    Parameter_arity_mismatch (lid.txt, List.length params,
                                                   List.length tyl)));
      List.iter2
        (fun cty' ty ->
          let ty' = cty'.ctyp_type in
           try Ctype.unify val_env ty' ty with Ctype.Unify err ->
             raise(Error(cty'.ctyp_loc, val_env, Parameter_mismatch err)))
        tyl params;
      (* Check for unexpected virtual methods *)
      check_virtual_clty scl.pcl_loc val_env virt Class clty';
      let cl =
        rc {cl_desc = Tcl_ident (path, lid, tyl);
            cl_loc = scl.pcl_loc;
            cl_type = clty';
            cl_env = val_env;
            cl_attributes = scl.pcl_attributes;
           }
      in
      let (vals, meths, concrs) = extract_constraints clty in
      rc {cl_desc = Tcl_constraint (cl, None, vals, meths, concrs);
          cl_loc = scl.pcl_loc;
          cl_type = clty';
          cl_env = val_env;
          cl_attributes = []; (* attributes are kept on the inner cl node *)
         }
  | Pcl_structure cl_str ->
      let desc =
        class_structure cl_num virt self_scope Not_final
          val_env met_env scl.pcl_loc cl_str
      in
      rc {cl_desc = Tcl_structure desc;
          cl_loc = scl.pcl_loc;
          cl_type = Cty_signature desc.cstr_type;
          cl_env = val_env;
          cl_attributes = scl.pcl_attributes;
         }
  | Pcl_fun (l, Some default, spat, sbody) ->
      let loc = default.pexp_loc in
      let open Ast_helper in
      let scases = [
        Exp.case
          (Pat.construct ~loc
             (mknoloc (Longident.(Ldot (Lident "*predef*", "Some"))))
             (Some ([], Pat.var ~loc (mknoloc "*sth*"))))
          (Exp.ident ~loc (mknoloc (Longident.Lident "*sth*")));

        Exp.case
          (Pat.construct ~loc
             (mknoloc (Longident.(Ldot (Lident "*predef*", "None"))))
             None)
          default;
       ]
      in
      let smatch =
        Exp.match_ ~loc (Exp.ident ~loc (mknoloc (Longident.Lident "*opt*")))
          scases
      in
      let sfun =
        Cl.fun_ ~loc:scl.pcl_loc
          l None
          (Pat.var ~loc (mknoloc "*opt*"))
          (Cl.let_ ~loc:scl.pcl_loc Nonrecursive [Vb.mk spat smatch] sbody)
          (* Note: we don't put the '#default' attribute, as it
             is not detected for class-level let bindings.  See #5975.*)
      in
      class_expr cl_num val_env met_env virt self_scope sfun
  | Pcl_fun (l, None, spat, scl') ->
      if !Clflags.principal then Ctype.begin_def ();
      let (pat, pv, val_env', met_env) =
        Typecore.type_class_arg_pattern cl_num val_env met_env l spat
      in
      if !Clflags.principal then begin
        Ctype.end_def ();
        let gen {pat_type = ty} = Ctype.generalize_structure ty in
        iter_pattern gen pat
      end;
      let pv =
        List.map
          begin fun (id, id', _ty) ->
            let path = Pident id' in
            (* do not mark the value as being used *)
            let vd = Env.find_value path val_env' in
            (id,
             {exp_desc =
              Texp_ident(path, mknoloc (Longident.Lident (Ident.name id)), vd);
              exp_loc = Location.none; exp_extra = [];
              exp_type = Ctype.instance vd.val_type;
              exp_attributes = []; (* check *)
              exp_env = val_env'})
          end
          pv
      in
      let rec not_nolabel_function = function
        | Cty_arrow(Nolabel, _, _) -> false
        | Cty_arrow(_, _, cty) -> not_nolabel_function cty
        | _ -> true
      in
      let partial =
        let dummy = type_exp val_env (Ast_helper.Exp.unreachable ()) in
        Typecore.check_partial val_env pat.pat_type pat.pat_loc
          [{c_lhs = pat; c_guard = None; c_rhs = dummy}]
      in
      Ctype.raise_nongen_level ();
      let cl = class_expr cl_num val_env' met_env virt self_scope scl' in
      Ctype.end_def ();
      if Btype.is_optional l && not_nolabel_function cl.cl_type then
        Location.prerr_warning pat.pat_loc
          Warnings.Unerasable_optional_argument;
      rc {cl_desc = Tcl_fun (l, pat, pv, cl, partial);
          cl_loc = scl.pcl_loc;
          cl_type = Cty_arrow
            (l, Ctype.instance pat.pat_type, cl.cl_type);
          cl_env = val_env;
          cl_attributes = scl.pcl_attributes;
         }
  | Pcl_apply (scl', sargs) ->
      assert (sargs <> []);
      if !Clflags.principal then Ctype.begin_def ();
      let cl = class_expr cl_num val_env met_env virt self_scope scl' in
      if !Clflags.principal then begin
        Ctype.end_def ();
        Ctype.generalize_class_type_structure cl.cl_type;
      end;
      let rec nonopt_labels ls ty_fun =
        match ty_fun with
        | Cty_arrow (l, _, ty_res) ->
            if Btype.is_optional l then nonopt_labels ls ty_res
            else nonopt_labels (l::ls) ty_res
        | _    -> ls
      in
      let ignore_labels =
        !Clflags.classic ||
        let labels = nonopt_labels [] cl.cl_type in
        List.length labels = List.length sargs &&
        List.for_all (fun (l,_) -> l = Nolabel) sargs &&
        List.exists (fun l -> l <> Nolabel) labels &&
        begin
          Location.prerr_warning
            cl.cl_loc
            (Warnings.Labels_omitted
               (List.map Printtyp.string_of_label
                         (List.filter ((<>) Nolabel) labels)));
          true
        end
      in
      let rec type_args args omitted ty_fun ty_fun0 sargs =
        match ty_fun, ty_fun0 with
        | Cty_arrow (l, ty, ty_fun), Cty_arrow (_, ty0, ty_fun0)
          when sargs <> [] ->
            let name = Btype.label_name l
            and optional = Btype.is_optional l in
            let use_arg sarg l' =
              Some (
                if not optional || Btype.is_optional l' then
                  type_argument val_env sarg ty ty0
                else
                  let ty' = extract_option_type val_env ty
                  and ty0' = extract_option_type val_env ty0 in
                  let arg = type_argument val_env sarg ty' ty0' in
                  option_some val_env arg
              )
            in
            let eliminate_optional_arg () =
              Some (option_none val_env ty0 Location.none)
            in
            let remaining_sargs, arg =
              if ignore_labels then begin
                match sargs with
                | [] -> assert false
                | (l', sarg) :: remaining_sargs ->
                    if name = Btype.label_name l' ||
                       (not optional && l' = Nolabel)
                    then
                      (remaining_sargs, use_arg sarg l')
                    else if
                      optional &&
                      not (List.exists (fun (l, _) -> name = Btype.label_name l)
                             remaining_sargs)
                    then
                      (sargs, eliminate_optional_arg ())
                    else
                      raise(Error(sarg.pexp_loc, val_env, Apply_wrong_label l'))
              end else
                match Btype.extract_label name sargs with
                | Some (l', sarg, _, remaining_sargs) ->
                    if not optional && Btype.is_optional l' then
                      Location.prerr_warning sarg.pexp_loc
                        (Warnings.Nonoptional_label
                           (Printtyp.string_of_label l));
                    remaining_sargs, use_arg sarg l'
                | None ->
                    sargs,
                    if Btype.is_optional l && List.mem_assoc Nolabel sargs then
                      eliminate_optional_arg ()
                    else
                      None
            in
            let omitted = if arg = None then (l,ty0) :: omitted else omitted in
            type_args ((l,arg)::args) omitted ty_fun ty_fun0 remaining_sargs
        | _ ->
            match sargs with
              (l, sarg0)::_ ->
                if omitted <> [] then
                  raise(Error(sarg0.pexp_loc, val_env, Apply_wrong_label l))
                else
                  raise(Error(cl.cl_loc, val_env, Cannot_apply cl.cl_type))
            | [] ->
                (List.rev args,
                 List.fold_left
                   (fun ty_fun (l,ty) -> Cty_arrow(l,ty,ty_fun))
                   ty_fun0 omitted)
      in
      let (args, cty) =
        let (_, ty_fun0) = Ctype.instance_class [] cl.cl_type in
        type_args [] [] cl.cl_type ty_fun0 sargs
      in
      rc {cl_desc = Tcl_apply (cl, args);
          cl_loc = scl.pcl_loc;
          cl_type = cty;
          cl_env = val_env;
          cl_attributes = scl.pcl_attributes;
         }
  | Pcl_let (rec_flag, sdefs, scl') ->
      let (defs, val_env) =
        Typecore.type_let In_class_def val_env rec_flag sdefs in
      let (vals, met_env) =
        List.fold_right
          (fun (id, _id_loc, _typ) (vals, met_env) ->
             let path = Pident id in
             (* do not mark the value as used *)
             let vd = Env.find_value path val_env in
             Ctype.begin_def ();
             let expr =
               {exp_desc =
                Texp_ident(path, mknoloc(Longident.Lident (Ident.name id)),vd);
                exp_loc = Location.none; exp_extra = [];
                exp_type = Ctype.instance vd.val_type;
                exp_attributes = [];
                exp_env = val_env;
               }
             in
             Ctype.end_def ();
             Ctype.generalize expr.exp_type;
             let desc =
               {val_type = expr.exp_type; val_kind = Val_ivar (Immutable,
                                                               cl_num);
                val_attributes = [];
                Types.val_loc = vd.Types.val_loc;
                val_uid = vd.val_uid;
               }
             in
             let id' = Ident.create_local (Ident.name id) in
             ((id', expr)
              :: vals,
              Env.add_value id' desc met_env))
          (let_bound_idents_full defs)
          ([], met_env)
      in
      let cl = class_expr cl_num val_env met_env virt self_scope scl' in
      let () = if rec_flag = Recursive then
        check_recursive_bindings val_env defs
      in
      rc {cl_desc = Tcl_let (rec_flag, defs, vals, cl);
          cl_loc = scl.pcl_loc;
          cl_type = cl.cl_type;
          cl_env = val_env;
          cl_attributes = scl.pcl_attributes;
         }
  | Pcl_constraint (scl', scty) ->
      Ctype.begin_class_def ();
      let context = Typetexp.narrow () in
      let cl = class_expr cl_num val_env met_env virt self_scope scl' in
      complete_class_type cl.cl_loc val_env virt Class_type cl.cl_type;
      Typetexp.widen context;
      let context = Typetexp.narrow () in
      let clty = class_type val_env virt self_scope scty in
      complete_class_type clty.cltyp_loc val_env virt Class clty.cltyp_type;
      Typetexp.widen context;
      Ctype.end_def ();

      Ctype.limited_generalize_class_type
        (Btype.self_type_row cl.cl_type) cl.cl_type;
      Ctype.limited_generalize_class_type
        (Btype.self_type_row clty.cltyp_type) clty.cltyp_type;

      begin match
        Includeclass.class_types val_env cl.cl_type clty.cltyp_type
      with
        []    -> ()
      | error -> raise(Error(cl.cl_loc, val_env, Class_match_failure error))
      end;
      let (vals, meths, concrs) = extract_constraints clty.cltyp_type in
      let ty = snd (Ctype.instance_class [] clty.cltyp_type) in
      (* Adding a dummy method to the self type prevents it from being closed /
         escaping. *)
      Ctype.add_dummy_method val_env ~scope:self_scope
        (Btype.signature_of_class_type ty);
      rc {cl_desc = Tcl_constraint (cl, Some clty, vals, meths, concrs);
          cl_loc = scl.pcl_loc;
          cl_type = ty;
          cl_env = val_env;
          cl_attributes = scl.pcl_attributes;
         }
  | Pcl_open (pod, e) ->
      let used_slot = ref false in
      let (od, new_val_env) = !type_open_descr ~used_slot val_env pod in
      let ( _, new_met_env) = !type_open_descr ~used_slot met_env pod in
      let cl = class_expr cl_num new_val_env new_met_env virt self_scope e in
      rc {cl_desc = Tcl_open (od, cl);
          cl_loc = scl.pcl_loc;
          cl_type = cl.cl_type;
          cl_env = val_env;
          cl_attributes = scl.pcl_attributes;
         }
  | Pcl_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))

(*******************************)

(* Approximate the type of the constructor to allow recursive use *)
(* of optional parameters                                         *)

let var_option = Predef.type_option (Btype.newgenvar ())

let rec approx_declaration cl =
  match cl.pcl_desc with
    Pcl_fun (l, _, _, cl) ->
      let arg =
        if Btype.is_optional l then Ctype.instance var_option
        else Ctype.newvar () in
      Ctype.newty (Tarrow (l, arg, approx_declaration cl, commu_ok))
  | Pcl_let (_, _, cl) ->
      approx_declaration cl
  | Pcl_constraint (cl, _) ->
      approx_declaration cl
  | _ -> Ctype.newvar ()

let rec approx_description ct =
  match ct.pcty_desc with
    Pcty_arrow (l, _, ct) ->
      let arg =
        if Btype.is_optional l then Ctype.instance var_option
        else Ctype.newvar () in
      Ctype.newty (Tarrow (l, arg, approx_description ct, commu_ok))
  | _ -> Ctype.newvar ()

(*******************************)

let temp_abbrev loc arity uid =
  let params = ref [] in
  for _i = 1 to arity do
    params := Ctype.newvar () :: !params
  done;
  let ty = Ctype.newobj (Ctype.newvar ()) in
  let ty_td =
      {type_params = !params;
       type_arity = arity;
       type_kind = Type_abstract;
       type_private = Public;
       type_manifest = Some ty;
       type_variance = Variance.unknown_signature ~injective:false ~arity;
       type_separability = Types.Separability.default_signature ~arity;
       type_is_newtype = false;
       type_expansion_scope = Btype.lowest_level;
       type_loc = loc;
       type_attributes = []; (* or keep attrs from the class decl? *)
       type_immediate = Unknown;
       type_unboxed_default = false;
       type_uid = uid;
      }
  in
  (!params, ty, ty_td)

let initial_env define_class approx
    (res, env) (cl, id, ty_id, obj_id, uid) =
  (* Temporary abbreviations *)
  let arity = List.length cl.pci_params in
  let (obj_params, obj_ty, obj_td) = temp_abbrev cl.pci_loc arity uid in
  let env = Env.add_type ~check:true obj_id obj_td env in
  let (cl_params, cl_ty, cl_td) = temp_abbrev cl.pci_loc arity uid in

  (* Temporary type for the class constructor *)
  if !Clflags.principal then Ctype.begin_def ();
  let constr_type = approx cl.pci_expr in
  if !Clflags.principal then begin
    Ctype.end_def ();
    Ctype.generalize_structure constr_type;
  end;
  let dummy_cty = Cty_signature (Ctype.new_class_signature ()) in
  let dummy_class =
    {Types.cty_params = [];             (* Dummy value *)
     cty_variance = [];
     cty_type = dummy_cty;        (* Dummy value *)
     cty_path = unbound_class;
     cty_new =
       begin match cl.pci_virt with
       | Virtual  -> None
       | Concrete -> Some constr_type
       end;
     cty_loc = Location.none;
     cty_attributes = [];
     cty_uid = uid;
    }
  in
  let env =
    Env.add_cltype ty_id
      {clty_params = [];            (* Dummy value *)
       clty_variance = [];
       clty_type = dummy_cty;       (* Dummy value *)
       clty_path = unbound_class;
       clty_hash_type = cl_td;      (* Dummy value *)
       clty_loc = Location.none;
       clty_attributes = [];
       clty_uid = uid;
      }
      (
        if define_class then
          Env.add_class id dummy_class env
        else
          env
      )
  in
  ((cl, id, ty_id,
    obj_id, obj_params, obj_ty,
    cl_params, cl_ty, cl_td,
    constr_type,
    dummy_class)::res,
   env)

let class_infos define_class kind
    (cl, id, ty_id,
     obj_id, obj_params, obj_ty,
     cl_params, cl_ty, cl_td,
     constr_type,
     dummy_class)
    (res, env) =

  reset_type_variables ();
  Ctype.begin_class_def ();

  (* Introduce class parameters *)
  let ci_params =
    let make_param (sty, v) =
      try
          (transl_type_param env sty, v)
      with Already_bound ->
        raise(Error(sty.ptyp_loc, env, Repeated_parameter))
    in
      List.map make_param cl.pci_params
  in
  let params = List.map (fun (cty, _) -> cty.ctyp_type) ci_params in

  (* Allow self coercions (only for class declarations) *)
  let coercion_locs = ref [] in

  (* Type the class expression *)
  let (expr, typ) =
    try
      Typecore.self_coercion :=
        (Path.Pident obj_id, coercion_locs) :: !Typecore.self_coercion;
      let res = kind env cl.pci_virt cl.pci_expr in
      Typecore.self_coercion := List.tl !Typecore.self_coercion;
      res
    with exn ->
      Typecore.self_coercion := []; raise exn
  in
  let sign = Btype.signature_of_class_type typ in

  Ctype.end_def ();

  (* Generalize the row variable *)
  List.iter (Ctype.limited_generalize sign.csig_self_row) params;
  Ctype.limited_generalize_class_type sign.csig_self_row typ;

  (* Check the abbreviation for the object type *)
  let (obj_params', obj_type) = Ctype.instance_class params typ in
  let constr = Ctype.newconstr (Path.Pident obj_id) obj_params in
  begin
    let row = Btype.self_type_row obj_type in
    Ctype.unify env row (Ctype.newty Tnil);
    begin try
      List.iter2 (Ctype.unify env) obj_params obj_params'
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc, env,
            Bad_parameters (obj_id, constr,
                            Ctype.newconstr (Path.Pident obj_id)
                                            obj_params')))
    end;
    let ty = Btype.self_type obj_type in
    begin try
      Ctype.unify env ty constr
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc, env,
        Abbrev_type_clash (constr, ty, Ctype.expand_head env constr)))
    end
  end;

  Ctype.set_object_name obj_id params (Btype.self_type typ);

  (* Check the other temporary abbreviation (#-type) *)
  begin
    let (cl_params', cl_type) = Ctype.instance_class params typ in
    let ty = Btype.self_type cl_type in
    begin try
      List.iter2 (Ctype.unify env) cl_params cl_params'
    with Ctype.Unify _ ->
      let dummy_class_type params =
        let tv = Ctype.newvar () in
        Ctype.newty(Tobject (tv, ref (Some (Pident ty_id, tv :: params)))) in
      let ty1 = dummy_class_type cl_params
      and ty2 = dummy_class_type cl_params' in
      raise(Error(cl.pci_loc, env, Bad_parameters (ty_id, ty1, ty2)))
    end;
    begin try
      Ctype.unify env ty cl_ty
    with Ctype.Unify _ ->
      let tv = Ctype.newvar () in
      let constr =
        Ctype.newty(Tobject (tv, ref (Some (Pident ty_id, tv :: params)))) in
      raise(Error(cl.pci_loc, env, Abbrev_type_clash (constr, ty, cl_ty)))
    end
  end;

  (* Type of the class constructor *)
  begin try
    Ctype.unify env
      (constructor_type constr obj_type)
      (Ctype.instance constr_type)
  with Ctype.Unify err ->
    raise(Error(cl.pci_loc, env,
                Constructor_type_mismatch (cl.pci_name.txt, err)))
  end;

  (* Class and class type temporary definitions *)
  let cty_variance =
    Variance.unknown_signature ~injective:false ~arity:(List.length params) in
  let cltydef =
    {clty_params = params; clty_type = Btype.class_body typ;
     clty_variance = cty_variance;
     clty_path = Path.Pident obj_id;
     clty_hash_type = cl_td;
     clty_loc = cl.pci_loc;
     clty_attributes = cl.pci_attributes;
     clty_uid = dummy_class.cty_uid;
    }
  and clty =
    {cty_params = params; cty_type = typ;
     cty_variance = cty_variance;
     cty_path = Path.Pident obj_id;
     cty_new =
       begin match cl.pci_virt with
       | Virtual  -> None
       | Concrete -> Some constr_type
       end;
     cty_loc = cl.pci_loc;
     cty_attributes = cl.pci_attributes;
     cty_uid = dummy_class.cty_uid;
    }
  in
  dummy_class.cty_type <- typ;
  let env =
    Env.add_cltype ty_id cltydef (
    if define_class then Env.add_class id clty env else env)
  in

  (* Misc. *)
  let arity = Btype.class_type_arity typ in
  let pub_meths = Btype.public_methods sign in

  (* Final definitions *)
  let (params', typ') = Ctype.instance_class params typ in
  let clty =
    {cty_params = params'; cty_type = typ';
     cty_variance = cty_variance;
     cty_path = Path.Pident obj_id;
     cty_new =
       begin match cl.pci_virt with
       | Virtual  -> None
       | Concrete -> Some (Ctype.instance constr_type)
       end;
     cty_loc = cl.pci_loc;
     cty_attributes = cl.pci_attributes;
     cty_uid = dummy_class.cty_uid;
    }
  in
  let obj_abbr =
    let arity = List.length obj_params in
    {
     type_params = obj_params;
     type_arity = arity;
     type_kind = Type_abstract;
     type_private = Public;
     type_manifest = Some obj_ty;
     type_variance = Variance.unknown_signature ~injective:false ~arity;
     type_separability = Types.Separability.default_signature ~arity;
     type_is_newtype = false;
     type_expansion_scope = Btype.lowest_level;
     type_loc = cl.pci_loc;
     type_attributes = []; (* or keep attrs from cl? *)
     type_immediate = Unknown;
     type_unboxed_default = false;
     type_uid = dummy_class.cty_uid;
    }
  in
  let (cl_params, cl_ty) =
    Ctype.instance_parameterized_type params (Btype.self_type typ)
  in
  Ctype.set_object_name obj_id cl_params cl_ty;
  let cl_abbr =
    { cl_td with
     type_params = cl_params;
     type_manifest = Some cl_ty
    }
  in
  let cltydef =
    {clty_params = params'; clty_type = Btype.class_body typ';
     clty_variance = cty_variance;
     clty_path = Path.Pident obj_id;
     clty_hash_type = cl_abbr;
     clty_loc = cl.pci_loc;
     clty_attributes = cl.pci_attributes;
     clty_uid = dummy_class.cty_uid;
    }
  in
  ((cl, id, clty, ty_id, cltydef, obj_id, obj_abbr, ci_params,
    arity, pub_meths, List.rev !coercion_locs, expr) :: res,
   env)

let final_decl env define_class
    (cl, id, clty, ty_id, cltydef, obj_id, obj_abbr, ci_params,
     arity, pub_meths, coe, expr) =
  let cl_abbr = cltydef.clty_hash_type in

  begin try Ctype.collapse_conj_params env clty.cty_params
  with Ctype.Unify err ->
    raise(Error(cl.pci_loc, env, Non_collapsable_conjunction (id, clty, err)))
  end;

  List.iter Ctype.generalize clty.cty_params;
  Ctype.generalize_class_type clty.cty_type;
  Option.iter  Ctype.generalize clty.cty_new;
  List.iter Ctype.generalize obj_abbr.type_params;
  Option.iter  Ctype.generalize obj_abbr.type_manifest;
  List.iter Ctype.generalize cl_abbr.type_params;
  Option.iter  Ctype.generalize cl_abbr.type_manifest;

  if Ctype.nongen_class_declaration clty then
    raise(Error(cl.pci_loc, env, Non_generalizable_class (id, clty)));

  begin match
    Ctype.closed_class clty.cty_params
      (Btype.signature_of_class_type clty.cty_type)
  with
    None        -> ()
  | Some reason ->
      let printer =
        if define_class
        then function ppf -> Printtyp.class_declaration id ppf clty
        else function ppf -> Printtyp.cltype_declaration id ppf cltydef
      in
      raise(Error(cl.pci_loc, env, Unbound_type_var(printer, reason)))
  end;
  { id; clty; ty_id; cltydef; obj_id; obj_abbr; arity;
    pub_meths; coe;
    id_loc = cl.pci_name;
    req = { ci_loc = cl.pci_loc;
            ci_virt = cl.pci_virt;
            ci_params = ci_params;
        (* TODO : check that we have the correct use of identifiers *)
            ci_id_name = cl.pci_name;
            ci_id_class = id;
            ci_id_class_type = ty_id;
            ci_id_object = obj_id;
            ci_expr = expr;
            ci_decl = clty;
            ci_type_decl = cltydef;
            ci_attributes = cl.pci_attributes;
        }
  }
(*   (cl.pci_variance, cl.pci_loc)) *)

let class_infos define_class kind
    (cl, id, ty_id,
     obj_id, obj_params, obj_ty,
     cl_params, cl_ty, cl_td,
     constr_type,
     dummy_class)
    (res, env) =
  Builtin_attributes.warning_scope cl.pci_attributes
    (fun () ->
       class_infos define_class kind
         (cl, id, ty_id,
          obj_id, obj_params, obj_ty,
          cl_params, cl_ty, cl_td,
          constr_type,
          dummy_class)
         (res, env)
    )

let extract_type_decls { clty; cltydef; obj_id; obj_abbr; req} decls =
  (obj_id, obj_abbr, clty, cltydef, req) :: decls

let merge_type_decls decl (obj_abbr, clty, cltydef) =
  {decl with obj_abbr; clty; cltydef}

let final_env define_class env { id; clty; ty_id; cltydef; obj_id; obj_abbr; } =
  (* Add definitions after cleaning them *)
  Env.add_type ~check:true obj_id
    (Subst.type_declaration Subst.identity obj_abbr) (
  Env.add_cltype ty_id (Subst.cltype_declaration Subst.identity cltydef) (
  if define_class then
    Env.add_class id (Subst.class_declaration Subst.identity clty) env
  else env))

(* Check that #c is coercible to c if there is a self-coercion *)
let check_coercions env { id; id_loc; clty; ty_id; cltydef; obj_id; obj_abbr;
    arity; pub_meths; coe; req } =
  let cl_abbr = cltydef.clty_hash_type in
  begin match coe with [] -> ()
  | loc :: _ ->
      let cl_ty, obj_ty =
        match cl_abbr.type_manifest, obj_abbr.type_manifest with
          Some cl_ab, Some obj_ab ->
            let cl_params, cl_ty =
              Ctype.instance_parameterized_type cl_abbr.type_params cl_ab
            and obj_params, obj_ty =
              Ctype.instance_parameterized_type obj_abbr.type_params obj_ab
            in
            List.iter2 (Ctype.unify env) cl_params obj_params;
            cl_ty, obj_ty
        | _ -> assert false
      in
      begin try Ctype.subtype env cl_ty obj_ty ()
      with Ctype.Subtype err ->
        raise(Typecore.Error(loc, env, Typecore.Not_subtype err))
      end;
      if not (Ctype.opened_object cl_ty) then
        raise(Error(loc, env, Cannot_coerce_self obj_ty))
  end;
  {cls_id = id;
   cls_id_loc = id_loc;
   cls_decl = clty;
   cls_ty_id = ty_id;
   cls_ty_decl = cltydef;
   cls_obj_id = obj_id;
   cls_obj_abbr = obj_abbr;
   cls_abbr = cl_abbr;
   cls_arity = arity;
   cls_pub_methods = pub_meths;
   cls_info=req}

(*******************************)

let type_classes define_class approx kind env cls =
  let scope = Ctype.create_scope () in
  let cls =
    List.map
      (function cl ->
         (cl,
          Ident.create_scoped ~scope cl.pci_name.txt,
          Ident.create_scoped ~scope cl.pci_name.txt,
          Ident.create_scoped ~scope cl.pci_name.txt,
          Uid.mk ~current_unit:(Env.get_unit_name ())
         ))
      cls
  in
  Ctype.begin_class_def ();
  let (res, env) =
    List.fold_left (initial_env define_class approx) ([], env) cls
  in
  let (res, env) =
    List.fold_right (class_infos define_class kind) res ([], env)
  in
  Ctype.end_def ();
  let res = List.rev_map (final_decl env define_class) res in
  let decls = List.fold_right extract_type_decls res [] in
  let decls =
    try Typedecl_variance.update_class_decls env decls
    with Typedecl_variance.Error(loc, err) ->
      raise (Typedecl.Error(loc, Typedecl.Variance err))
  in
  let res = List.map2 merge_type_decls res decls in
  let env = List.fold_left (final_env define_class) env res in
  let res = List.map (check_coercions env) res in
  (res, env)

let class_num = ref 0
let class_declaration env virt sexpr =
  incr class_num;
  let self_scope = Ctype.get_current_level () in
  let expr =
    class_expr (Int.to_string !class_num) env env virt self_scope sexpr
  in
  complete_class_type expr.cl_loc env virt Class expr.cl_type;
  (expr, expr.cl_type)

let class_description env virt sexpr =
  let self_scope = Ctype.get_current_level () in
  let expr = class_type env virt self_scope sexpr in
  complete_class_type expr.cltyp_loc env virt Class_type expr.cltyp_type;
  (expr, expr.cltyp_type)

let class_declarations env cls =
  let info, env =
    type_classes true approx_declaration class_declaration env cls
  in
  let ids, exprs =
    List.split
      (List.map
         (fun ci -> ci.cls_id, ci.cls_info.ci_expr)
         info)
  in
  check_recursive_class_bindings env ids exprs;
  info, env

let class_descriptions env cls =
  type_classes true approx_description class_description env cls

let class_type_declarations env cls =
  let (decls, env) =
    type_classes false approx_description class_description env cls
  in
  (List.map
     (fun decl ->
        {clsty_ty_id = decl.cls_ty_id;
         clsty_id_loc = decl.cls_id_loc;
         clsty_ty_decl = decl.cls_ty_decl;
         clsty_obj_id = decl.cls_obj_id;
         clsty_obj_abbr = decl.cls_obj_abbr;
         clsty_abbr = decl.cls_abbr;
         clsty_info = decl.cls_info})
     decls,
   env)

let type_object env loc s =
  incr class_num;
  let desc =
    class_structure (Int.to_string !class_num)
      Concrete Btype.lowest_level Final env env loc s
  in
  complete_class_signature loc env Concrete Object desc.cstr_type;
  let meths = Btype.public_methods desc.cstr_type in
  (desc, meths)

let () =
  Typecore.type_object := type_object

(*******************************)

(* Approximate the class declaration as class ['params] id = object end *)
let approx_class sdecl =
  let open Ast_helper in
  let self' = Typ.any () in
  let clty' = Cty.signature ~loc:sdecl.pci_expr.pcty_loc (Csig.mk self' []) in
  { sdecl with pci_expr = clty' }

let approx_class_declarations env sdecls =
  fst (class_type_declarations env (List.map approx_class sdecls))

(*******************************)

(* Error report *)

open Format

let non_virtual_string_of_kind = function
  | Object -> "object"
  | Class -> "non-virtual class"
  | Class_type -> "non-virtual class type"

let report_error env ppf = function
  | Repeated_parameter ->
      fprintf ppf "A type parameter occurs several times"
  | Unconsistent_constraint err ->
      fprintf ppf "@[<v>The class constraints are not consistent.@ ";
      Printtyp.report_unification_error ppf env err
        (fun ppf -> fprintf ppf "Type")
        (fun ppf -> fprintf ppf "is not compatible with type");
      fprintf ppf "@]"
  | Field_type_mismatch (k, m, err) ->
      Printtyp.report_unification_error ppf env err
        (function ppf ->
           fprintf ppf "The %s %s@ has type" k m)
        (function ppf ->
           fprintf ppf "but is expected to have type")
  | Unexpected_field (ty, lab) ->
      fprintf ppf
        "@[@[<2>This object is expected to have type :@ %a@]\
         @ This type does not have a method %s."
        Printtyp.type_expr ty lab
  | Structure_expected clty ->
      fprintf ppf
        "@[This class expression is not a class structure; it has type@ %a@]"
        Printtyp.class_type clty
  | Cannot_apply _ ->
      fprintf ppf
        "This class expression is not a class function, it cannot be applied"
  | Apply_wrong_label l ->
      let mark_label = function
        | Nolabel -> "out label"
        |  l -> sprintf " label %s" (Btype.prefixed_label_name l) in
      fprintf ppf "This argument cannot be applied with%s" (mark_label l)
  | Pattern_type_clash ty ->
      (* XXX Trace *)
      (* XXX Revoir message d'erreur | Improve error message *)
      fprintf ppf "@[%s@ %a@]"
        "This pattern cannot match self: it only matches values of type"
        Printtyp.type_expr ty
  | Unbound_class_2 cl ->
      fprintf ppf "@[The class@ %a@ is not yet completely defined@]"
      Printtyp.longident cl
  | Unbound_class_type_2 cl ->
      fprintf ppf "@[The class type@ %a@ is not yet completely defined@]"
      Printtyp.longident cl
  | Abbrev_type_clash (abbrev, actual, expected) ->
      (* XXX Afficher une trace ? | Print a trace? *)
      Printtyp.prepare_for_printing [abbrev; actual; expected];
      fprintf ppf "@[The abbreviation@ %a@ expands to type@ %a@ \
       but is used with type@ %a@]"
        !Oprint.out_type (Printtyp.tree_of_typexp Type abbrev)
        !Oprint.out_type (Printtyp.tree_of_typexp Type actual)
        !Oprint.out_type (Printtyp.tree_of_typexp Type expected)
  | Constructor_type_mismatch (c, err) ->
      Printtyp.report_unification_error ppf env err
        (function ppf ->
           fprintf ppf "The expression \"new %s\" has type" c)
        (function ppf ->
           fprintf ppf "but is used with type")
  | Virtual_class (kind, mets, vals) ->
      let kind = non_virtual_string_of_kind kind in
      let missings =
        match mets, vals with
          [], _ -> "variables"
        | _, [] -> "methods"
        | _ -> "methods and variables"
      in
      fprintf ppf
        "@[This %s has virtual %s.@ \
         @[<2>The following %s are virtual : %a@]@]"
        kind missings missings
        (pp_print_list ~pp_sep:pp_print_space pp_print_string) (mets @ vals)
  | Undeclared_methods(kind, mets) ->
      let kind = non_virtual_string_of_kind kind in
      fprintf ppf
        "@[This %s has undeclared virtual methods.@ \
         @[<2>The following methods were not declared : %a@]@]"
        kind (pp_print_list ~pp_sep:pp_print_space pp_print_string) mets
  | Parameter_arity_mismatch(lid, expected, provided) ->
      fprintf ppf
        "@[The class constructor %a@ expects %i type argument(s),@ \
           but is here applied to %i type argument(s)@]"
        Printtyp.longident lid expected provided
  | Parameter_mismatch err ->
      Printtyp.report_unification_error ppf env err
        (function ppf ->
           fprintf ppf "The type parameter")
        (function ppf ->
           fprintf ppf "does not meet its constraint: it should be")
  | Bad_parameters (id, params, cstrs) ->
      Printtyp.prepare_for_printing [params; cstrs];
      fprintf ppf
        "@[The abbreviation %a@ is used with parameters@ %a@ \
           which are incompatible with constraints@ %a@]"
        Printtyp.ident id
        !Oprint.out_type (Printtyp.tree_of_typexp Type params)
        !Oprint.out_type (Printtyp.tree_of_typexp Type cstrs)
  | Class_match_failure error ->
      Includeclass.report_error Type ppf error
  | Unbound_val lab ->
      fprintf ppf "Unbound instance variable %s" lab
  | Unbound_type_var (printer, reason) ->
      let print_reason ppf (ty0, real, lab, ty) =
        let ty1 =
          if real then ty0 else Btype.newgenty(Tobject(ty0, ref None)) in
        Printtyp.add_type_to_preparation ty;
        Printtyp.add_type_to_preparation ty1;
        fprintf ppf
          "The method %s@ has type@;<1 2>%a@ where@ %a@ is unbound"
          lab
          !Oprint.out_type (Printtyp.tree_of_typexp Type ty)
          !Oprint.out_type (Printtyp.tree_of_typexp Type ty0)
      in
      fprintf ppf
        "@[<v>@[Some type variables are unbound in this type:@;<1 2>%t@]@ \
              @[%a@]@]"
       printer print_reason reason
  | Non_generalizable_class (id, clty) ->
      fprintf ppf
        "@[The type of this class,@ %a,@ \
           contains type variables that cannot be generalized@]"
        (Printtyp.class_declaration id) clty
  | Cannot_coerce_self ty ->
      fprintf ppf
        "@[The type of self cannot be coerced to@ \
           the type of the current class:@ %a.@.\
           Some occurrences are contravariant@]"
        Printtyp.type_scheme ty
  | Non_collapsable_conjunction (id, clty, err) ->
      fprintf ppf
        "@[The type of this class,@ %a,@ \
           contains non-collapsible conjunctive types in constraints.@ %t@]"
        (Printtyp.class_declaration id) clty
        (fun ppf -> Printtyp.report_unification_error ppf env err
            (fun ppf -> fprintf ppf "Type")
            (fun ppf -> fprintf ppf "is not compatible with type")
        )
  | Self_clash err ->
      Printtyp.report_unification_error ppf env err
        (function ppf ->
           fprintf ppf "This object is expected to have type")
        (function ppf ->
           fprintf ppf "but actually has type")
  | Mutability_mismatch (_lab, mut) ->
      let mut1, mut2 =
        if mut = Immutable then "mutable", "immutable"
        else "immutable", "mutable" in
      fprintf ppf
        "@[The instance variable is %s;@ it cannot be redefined as %s@]"
        mut1 mut2
  | No_overriding (_, "") ->
      fprintf ppf "@[This inheritance does not override any method@ %s@]"
        "instance variable"
  | No_overriding (kind, name) ->
      fprintf ppf "@[The %s `%s'@ has no previous definition@]" kind name
  | Duplicate (kind, name) ->
      fprintf ppf "@[The %s `%s'@ has multiple definitions in this object@]"
                    kind name
  | Closing_self_type sign ->
    fprintf ppf
      "@[Cannot close type of object literal:@ %a@,\
       it has been unified with the self type of a class that is not yet@ \
       completely defined.@]"
      Printtyp.type_scheme sign.csig_self

let report_error env ppf err =
  Printtyp.wrap_printing_env ~error:true
    env (fun () -> report_error env ppf err)

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, env, err) ->
        Some (Location.error_of_printer ~loc (report_error env) err)
      | Error_forward err ->
        Some err
      | _ ->
        None
    )
