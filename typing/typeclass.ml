(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Misc
open Parsetree
open Asttypes
open Types
open Typedtree
open Typecore
open Typetexp

type error =
    Unconsistent_constraint of (type_expr * type_expr) list
  | Method_type_mismatch of string * (type_expr * type_expr) list
  | Structure_expected of class_type
  | Cannot_apply of class_type
  | Pattern_type_clash of type_expr
  | Repeated_parameter
  | Unbound_class of Longident.t
  | Unbound_class_2 of Longident.t
  | Unbound_class_type of Longident.t
  | Unbound_class_type_2 of Longident.t
  | Abbrev_type_clash of type_expr * type_expr * type_expr
  | Constructor_type_mismatch of string * (type_expr * type_expr) list
  | Virtual_class of bool * string list
  | Parameter_arity_mismatch of Longident.t * int * int
  | Parameter_mismatch of (type_expr * type_expr) list
  | Bad_parameters of Ident.t * type_expr * type_expr
  | Class_match_failure of Ctype.class_match_failure list
  | Unbound_val of string
  | Unbound_type_var of (unit -> unit) * Ctype.closed_class_failure
  | Make_nongen_seltype of type_expr
  | Non_generalizable_class of Ident.t * Types.class_declaration

exception Error of Location.t * error


                       (**********************)
                       (*  Useful constants  *)
                       (**********************)
                                   

(*
   Self type have a dummy private method, thus preventing it to become
   closed.
*)
let dummy_method = "*dummy method*"

(*
   Path associated to the temporary class type of a class being typed
   (its constructor is not available).
*)
let unbound_class = Path.Pident (Ident.create "")


                (************************************)
                (*  Some operations on class types  *)
                (************************************)
                                   

(* Fully expand the head of a class type *)
let rec scrape_class_type =
  function
    Tcty_constr (_, _, cty) -> scrape_class_type cty
  | cty                     -> cty

(* Generalize a class type *)
let rec generalize_class_type =
  function
    Tcty_constr (_, params, cty) ->
      List.iter Ctype.generalize params;
      generalize_class_type cty
  | Tcty_signature {cty_self = sty; cty_vars = vars } ->
      Ctype.generalize sty;
      Vars.iter (fun _ (_, ty) -> Ctype.generalize ty) vars
  | Tcty_fun (ty, cty) ->
      Ctype.generalize ty;
      generalize_class_type cty

(* Return the virtual methods of a class type *)
let virtual_methods cty =
  let sign = Ctype.signature_of_class_type cty in
  let (fields, _) = Ctype.flatten_fields (Ctype.object_fields sign.cty_self) in
  List.fold_left
    (fun virt (lab, _, _) ->
       if lab = dummy_method then virt else
       if Concr.mem lab sign.cty_concr then virt else
       lab::virt)
    [] fields

(* Return the constructor type associated to a class type *)
let rec constructor_type constr cty =
  match cty with
    Tcty_constr (_, _, cty) ->
      constructor_type constr cty
  | Tcty_signature sign ->
      constr
  | Tcty_fun (ty, cty) ->
      Ctype.newty (Tarrow (ty, constructor_type constr cty))

let rec class_body cty =
  match cty with
    Tcty_constr (_, _, cty') ->
      cty (* Only class bodies can be abbreviated *)
  | Tcty_signature sign ->
      cty
  | Tcty_fun (ty, cty) ->
      class_body cty

let rec extract_constraints cty =
  let sign = Ctype.signature_of_class_type cty in
  (Vars.fold (fun lab _ vars -> lab :: vars) sign.cty_vars [],
   begin let (fields, _) =
     Ctype.flatten_fields (Ctype.object_fields sign.cty_self)
   in
   List.fold_left
     (fun meths (lab, _, _) ->
        if lab = dummy_method then meths else lab::meths)
     [] fields
   end,
   sign.cty_concr)

let rec abbreviate_class_type path params cty =
  match cty with
    Tcty_constr (_, _, _) | Tcty_signature _ ->
      Tcty_constr (path, params, cty)
  | Tcty_fun (ty, cty) ->
      Tcty_fun (ty, abbreviate_class_type path params cty)

let rec closed_class_type =
  function
    Tcty_constr (_, params, _) ->
      List.for_all Ctype.closed_schema params
  | Tcty_signature sign ->
      Ctype.closed_schema sign.cty_self
        &&
      Vars.fold (fun _ (_, ty) cc -> Ctype.closed_schema ty && cc)
        sign.cty_vars
        true
  | Tcty_fun (ty, cty) ->
      Ctype.closed_schema ty
        &&
      closed_class_type cty

let closed_class cty =
  List.for_all Ctype.closed_schema cty.cty_params
    &&
  closed_class_type cty.cty_type

                (***********************************)
                (*  Primitives for typing classes  *)
                (***********************************)
                                   

(* Enter a value in the method environment only *)
let enter_met_env lab kind ty val_env met_env par_env =
  let (id, val_env) =
    Env.enter_value lab {val_type = ty; val_kind = Val_unbound} val_env
  in
  (id, val_env,
   Env.add_value id {val_type = ty; val_kind = kind} met_env,
   Env.add_value id {val_type = ty; val_kind = Val_unbound} par_env)

(* Enter an instance variable in the environment *)
let enter_val vars lab mut ty val_env met_env par_env =
  let (id, val_env, met_env, par_env) as result =
    enter_met_env lab (Val_ivar mut) ty val_env met_env par_env
  in
  vars := Vars.add lab (id, mut, ty) !vars;
  result

let rec make_nongen_class_type env =
  function
    Tcty_constr (_, _, cty) ->
      make_nongen_class_type env cty
  | Tcty_signature sign ->
      Vars.iter (fun _ (_, ty) -> Ctype.make_nongen ty) sign.cty_vars;
      Concr.iter
        (function nm ->
           Ctype.make_nongen
            (Ctype.filter_method env nm Private sign.cty_self))
        sign.cty_concr
  | Tcty_fun (ty, cty) ->
      Ctype.make_nongen ty;
      make_nongen_class_type env cty

let inheritance impl self_type env concr_meths loc parent =
  match scrape_class_type parent with
    Tcty_signature cl_sig ->

      (* Methods *)
      begin try
        Ctype.unify env self_type cl_sig.cty_self
      with Ctype.Unify trace ->
        match trace with
          _::_::_::({desc = Tfield(n, _, _, _)}, _)::rem ->
            raise(Error(loc, Method_type_mismatch (n, rem)))
        | _ ->
            assert false
      end;

      if impl then begin
        let overridings = Concr.inter cl_sig.cty_concr concr_meths in
        if not (Concr.is_empty overridings) then begin
          Location.print_warning loc
            (Warnings.Method_override (Concr.elements overridings))
        end
      end;
      let concr_meths = Concr.union cl_sig.cty_concr concr_meths in

      (cl_sig, concr_meths)

  | _ ->
      raise(Error(loc, Structure_expected parent))

let virtual_method val_env meths self_type lab priv sty loc =
  let (_, ty') =
     Ctype.filter_self_method val_env lab priv meths self_type
  in
  let ty = transl_simple_type val_env false sty in
  try Ctype.unify val_env ty ty' with Ctype.Unify trace ->
    raise(Error(loc, Method_type_mismatch (lab, trace)))

let type_constraint val_env sty sty' loc =
  let ty  = transl_simple_type val_env false sty in
  let ty' = transl_simple_type val_env false sty' in
  try Ctype.unify val_env ty ty' with Ctype.Unify trace ->
    raise(Error(loc, Unconsistent_constraint trace))

(*******************************)

let rec class_type_field env self_type meths (val_sig, concr_meths) =
  function
    Pctf_inher sparent ->
      let parent = class_type env sparent in
      let (cl_sig, concr_meths) =
        inheritance false self_type env concr_meths sparent.pcty_loc parent
      in
      let val_sig =
        Vars.fold
          (fun lab (mut, ty) val_sig -> Vars.add lab (mut, ty) val_sig)
          cl_sig.cty_vars val_sig
      in
      (val_sig, concr_meths)

  | Pctf_val (lab, mut, sty_opt, loc) ->
      let (mut, ty) =
        match sty_opt with
          None     ->
            let (mut', ty) =
              try Vars.find lab val_sig with Not_found ->
                raise(Error(loc, Unbound_val lab))
            in
            (if mut = Mutable then mut' else Immutable), ty
        | Some sty ->
            mut, transl_simple_type env false sty
      in
      (Vars.add lab (mut, ty) val_sig, concr_meths)

  | Pctf_virt (lab, priv, sty, loc) ->
      virtual_method env meths self_type lab priv sty loc;
      (val_sig, concr_meths)

  | Pctf_meth (lab, priv, sty, loc)  ->
      virtual_method env meths self_type lab priv sty loc;
      (val_sig, Concr.add lab concr_meths)

  | Pctf_cstr (sty, sty', loc) ->
      type_constraint env sty sty' loc;
      (val_sig, concr_meths)

and class_signature env sty sign =
  let meths = ref Meths.empty in
  let self_type = transl_simple_type env false sty in
  
  (* Check that the binder is a correct type, and introduce a dummy
     method preventing self type from being closed. *)
  begin try
    Ctype.filter_method env dummy_method Private self_type
  with Ctype.Unify _ ->
    raise(Error(sty.ptyp_loc, Pattern_type_clash self_type))
  end;
  
  (* Class type fields *)
  let (val_sig, concr_meths) =
    List.fold_left (class_type_field env self_type meths)
      (Vars.empty, Concr.empty)
      sign
  in
  
  {cty_self = self_type;
   cty_vars = val_sig;
   cty_concr = concr_meths }

and class_type env scty =
  match scty.pcty_desc with
    Pcty_constr (lid, styl) ->
      let (path, decl) =
        try Env.lookup_cltype lid env with Not_found ->
          raise(Error(scty.pcty_loc, Unbound_class_type lid))
      in
      if Path.same decl.clty_path unbound_class then
        raise(Error(scty.pcty_loc, Unbound_class_type_2 lid));
      let (params, clty) =
        Ctype.instance_class decl.clty_params decl.clty_type
      in
      let sty = Ctype.self_type clty in
      if List.length params <> List.length styl then
        raise(Error(scty.pcty_loc,
                    Parameter_arity_mismatch (lid, List.length params,
                                                   List.length styl)));
      List.iter2
        (fun sty ty ->
           let ty' = transl_simple_type env false sty in
           try Ctype.unify env ty' ty with Ctype.Unify trace ->
             raise(Error(sty.ptyp_loc, Parameter_mismatch trace)))
        styl params;
      Tcty_constr (path, params, clty)

  | Pcty_signature (sty, sign) ->
      Tcty_signature (class_signature env sty sign)
      
  | Pcty_fun (sty, scty) ->
      let ty = transl_simple_type env false sty in
      let cty = class_type env scty in
      Tcty_fun (ty, cty)

(*******************************)

module StringSet = Set.Make(struct type t = string let compare = compare end)

let rec class_field self_type meths vars
    (val_env, met_env, par_env, fields, concr_meths, inh_vals) =
  function
    Pcf_inher (sparent, super) ->
      let parent = class_expr val_env par_env sparent in
      let (cl_sig, concr_meths) =
        inheritance true self_type val_env concr_meths sparent.pcl_loc
          parent.cl_type
      in
      (* Variables *)
      let (val_env, met_env, par_env, inh_vars, inh_vals) =
        Vars.fold
          (fun lab (mut, ty) (val_env, met_env, par_env, inh_vars, inh_vals) ->
             let (id, val_env, met_env, par_env) =
               enter_val vars lab mut ty val_env met_env par_env
             in
             if StringSet.mem lab inh_vals then
               Location.print_warning sparent.pcl_loc
                 (Warnings.Hide_instance_variable lab);
             (val_env, met_env, par_env, (lab, id) :: inh_vars,
              StringSet.add lab inh_vals))
          cl_sig.cty_vars (val_env, met_env, par_env, [], inh_vals)
      in
      (* Inherited concrete methods *)
      let inh_meths = 
        Concr.fold (fun lab rem -> (lab, Ident.create lab)::rem)
          cl_sig.cty_concr []
      in
      (* Super *)      
      let (val_env, met_env, par_env) =
        match super with
          None ->
            (val_env, met_env, par_env)
        | Some name ->
            let (id, val_env, met_env, par_env) =
              enter_met_env name (Val_anc inh_meths) self_type
                val_env met_env par_env
            in
            (val_env, met_env, par_env)
      in
      (val_env, met_env, par_env,
       Cf_inher (parent, inh_vars, inh_meths)::fields,
       concr_meths, inh_vals)

  | Pcf_val (lab, mut, sexp, loc) ->
      if StringSet.mem lab inh_vals then
        Location.print_warning loc (Warnings.Hide_instance_variable lab);
      let exp = type_exp val_env sexp in
      if not (Typecore.is_nonexpansive exp) then
        begin try
          Ctype.make_nongen exp.exp_type
        with Ctype.Unify [(ty, _)] ->
          raise(Error(loc, Make_nongen_seltype ty))
        end;
      let (id, val_env, met_env, par_env) =
        enter_val vars lab mut exp.exp_type val_env met_env par_env
      in
      (val_env, met_env, par_env, Cf_val (lab, id, exp) :: fields,
       concr_meths, inh_vals)

  | Pcf_virt (lab, priv, sty, loc) ->
      virtual_method val_env meths self_type lab priv sty loc;
      (val_env, met_env, par_env, fields, concr_meths, inh_vals)

  | Pcf_meth (lab, priv, expr, loc)  ->
      Ctype.raise_nongen_level ();
      let (_, ty) =
        Ctype.filter_self_method val_env lab priv meths self_type
      in
      let meth_type = Ctype.newvar () in
      let (obj_ty, res_ty) = Ctype.filter_arrow val_env meth_type in
      Ctype.unify val_env obj_ty self_type;
      Ctype.unify val_env res_ty ty;
      let texp = type_expect met_env expr meth_type in
      Ctype.end_def ();
      (val_env, met_env, par_env, Cf_meth (lab, texp)::fields,
       Concr.add lab concr_meths, inh_vals)

  | Pcf_cstr (sty, sty', loc) ->
      type_constraint val_env sty sty' loc;
      (val_env, met_env, par_env, fields, concr_meths, inh_vals)

  | Pcf_let (rec_flag, sdefs, loc) ->
      let (defs, val_env) =
        try
          Typecore.type_let val_env rec_flag sdefs
        with Ctype.Unify [(ty, _)] ->
          raise(Error(loc, Make_nongen_seltype ty))
      in
      let (vals, met_env, par_env) =
        List.fold_right
          (fun id (vals, met_env, par_env) ->
             let expr =
               Typecore.type_exp val_env
                 {pexp_desc = Pexp_ident (Longident.Lident (Ident.name id));
                  pexp_loc = Location.none}
             in
             let desc =
               {val_type = expr.exp_type; val_kind = Val_ivar Immutable}
             in
             let id' = Ident.create (Ident.name id) in
             ((id', expr)
              :: vals,
              Env.add_value id' desc met_env,
              Env.add_value id' desc par_env))
          (let_bound_idents defs)
          ([], met_env, par_env)
      in
      (val_env, met_env, par_env, Cf_let (rec_flag, defs, vals)::fields,
       concr_meths, inh_vals)

  | Pcf_init expr ->
      Ctype.raise_nongen_level ();
      let meth_type = Ctype.newvar () in
      let (obj_ty, res_ty) = Ctype.filter_arrow val_env meth_type in
      Ctype.unify val_env obj_ty self_type;
      Ctype.unify val_env res_ty (Ctype.instance Predef.type_unit);
      let texp = type_expect met_env expr meth_type in
      Ctype.end_def ();
      (val_env, met_env, par_env, Cf_init texp::fields, concr_meths, inh_vals)

and class_structure val_env met_env (spat, str) =
  (* Environment for substructures *)
  let par_env = met_env in

  (* Self binder *)
  let (pat, meths, vars, val_env, meth_env, par_env) =
    type_self_pattern val_env met_env par_env spat
  in
  let self_type = pat.pat_type in

  (* Check that the binder has a correct type, and introduce a dummy
     method preventing self type from being closed. *)
  let ty = Ctype.newvar () in
  Ctype.filter_method val_env dummy_method Private ty;
  begin try Ctype.unify val_env self_type ty with
    Ctype.Unify _ ->
      raise(Error(pat.pat_loc, Pattern_type_clash self_type))
  end;

  (* Class fields *)
  let (_, _, _, fields, concr_meths, _) =
    List.fold_left (class_field self_type meths vars)
      (val_env, meth_env, par_env, [], Concr.empty, StringSet.empty)
      str
  in

  {cl_field = List.rev fields;
   cl_meths = Meths.map (function (id, ty) -> id) !meths},

  {cty_self = self_type;
   cty_vars = Vars.map (function (id, mut, ty) -> (mut, ty)) !vars;
   cty_concr = concr_meths }

and class_expr val_env met_env scl =
  match scl.pcl_desc with
    Pcl_constr (lid, styl) ->
      let (path, decl) =
        try Env.lookup_class lid val_env with Not_found ->
          raise(Error(scl.pcl_loc, Unbound_class lid))
      in
      if Path.same decl.cty_path unbound_class then
        raise(Error(scl.pcl_loc, Unbound_class_2 lid));
      let tyl = List.map (transl_simple_type val_env false) styl in
      let (params, clty) =
        Ctype.instance_class decl.cty_params decl.cty_type
      in
      let clty' = abbreviate_class_type path params clty in
      if List.length params <> List.length styl then
        raise(Error(scl.pcl_loc,
                    Parameter_arity_mismatch (lid, List.length params,
                                                   List.length styl)));
      List.iter2
        (fun sty ty ->
           let ty' = transl_simple_type val_env false sty in
           try Ctype.unify val_env ty' ty with Ctype.Unify trace ->
             raise(Error(sty.ptyp_loc, Parameter_mismatch trace)))
        styl params;
      let cl =        
        {cl_desc = Tclass_ident path;
         cl_loc = scl.pcl_loc;
         cl_type = clty'}
      in
      let (vals, meths, concrs) = extract_constraints clty in
      {cl_desc = Tclass_constraint (cl, vals, meths, concrs);
       cl_loc = scl.pcl_loc;
       cl_type = clty'}
  | Pcl_structure cl_str ->
      let (desc, ty) = class_structure val_env met_env cl_str in
      {cl_desc = Tclass_structure desc;
       cl_loc = scl.pcl_loc;
       cl_type = Tcty_signature ty}
  | Pcl_fun (spat, scl') ->
      let (pat, pv, val_env, met_env) =
        Typecore.type_class_arg_pattern val_env met_env spat
      in
      let pv =
        List.map
          (function (id, id', ty) ->
            (id,
             Typecore.type_exp val_env
               {pexp_desc = Pexp_ident (Longident.Lident (Ident.name id));
                pexp_loc = Location.none}))
          pv
      in
      Parmatch.check_partial pat.pat_loc
        [pat, (* Dummy expression *)
              {exp_desc = Texp_constant (Asttypes.Const_int 1);
               exp_loc = Location.none;
               exp_type = Ctype.none;
               exp_env = Env.empty }];
      Ctype.raise_nongen_level ();
      let cl = class_expr val_env met_env scl' in
      Ctype.end_def ();
      {cl_desc = Tclass_fun (pat, pv, cl);
       cl_loc = scl.pcl_loc;
       cl_type = Tcty_fun (pat.pat_type, cl.cl_type)}
  | Pcl_apply (scl', sargs) ->
      let cl = class_expr val_env met_env scl' in
      let rec type_args ty_fun =
        function
          [] ->
            ([], ty_fun)
        | sarg1 :: sargl ->
            begin match ty_fun with
              Tcty_fun (ty, cty) ->
                let arg1 = type_expect val_env sarg1 ty in
                let (argl, ty_res) = type_args cty sargl in
                (arg1 :: argl, ty_res)
            | _ ->
                raise(Error(cl.cl_loc, Cannot_apply cl.cl_type))
            end
      in
      let (args, cty) = type_args cl.cl_type sargs in
      begin try
        make_nongen_class_type val_env cty
      with Ctype.Unify [(ty, _)] ->
        raise(Error(scl.pcl_loc, Make_nongen_seltype ty))
      end;
      {cl_desc = Tclass_apply (cl, args);
       cl_loc = scl.pcl_loc;
       cl_type = cty}
  | Pcl_let (rec_flag, sdefs, scl') ->
      let (defs, val_env) =
        try
          Typecore.type_let val_env rec_flag sdefs
        with Ctype.Unify [(ty, _)] ->
          raise(Error(scl.pcl_loc, Make_nongen_seltype ty))
      in
      let (vals, met_env) =
        List.fold_right
          (fun id (vals, met_env) ->
             Ctype.begin_def ();
             let expr =
               Typecore.type_exp val_env
                 {pexp_desc = Pexp_ident (Longident.Lident (Ident.name id));
                  pexp_loc = Location.none}
             in
             Ctype.end_def ();
             Ctype.generalize expr.exp_type;
             let desc =
               {val_type = expr.exp_type; val_kind = Val_ivar Immutable}
             in
             let id' = Ident.create (Ident.name id) in
             ((id', expr)
              :: vals,
              Env.add_value id' desc met_env))
          (let_bound_idents defs)
          ([], met_env)
      in
      let cl = class_expr val_env met_env scl' in
      {cl_desc = Tclass_let (rec_flag, defs, vals, cl);
       cl_loc = scl.pcl_loc;
       cl_type = cl.cl_type}
  | Pcl_constraint (scl', scty) ->
      Ctype.begin_class_def ();
      Typetexp.narrow ();
      let cl = class_expr val_env met_env scl' in
      Typetexp.widen ();
      Typetexp.narrow ();
      let clty = class_type val_env scty in
      Typetexp.widen ();
      Ctype.end_def ();
      generalize_class_type cl.cl_type;
      generalize_class_type clty;
      begin match Includeclass.class_types val_env cl.cl_type clty with
        []    -> ()
      | error -> raise(Error(cl.cl_loc, Class_match_failure error))
      end;
      let (vals, meths, concrs) = extract_constraints clty in
      {cl_desc = Tclass_constraint (cl, vals, meths, concrs);
       cl_loc = scl.pcl_loc;
       cl_type = snd (Ctype.instance_class [] clty)}

(*******************************)

let temp_abbrev env id arity =
  let params = ref [] in
  for i = 1 to arity do
    params := Ctype.newvar () :: !params
  done;
  let ty = Ctype.newobj (Ctype.newvar ()) in
  let env =
    Env.add_type id
      {type_params = !params;
       type_arity = arity;
       type_kind = Type_abstract;
       type_manifest = Some ty }
      env
  in
  (!params, ty, env)

let rec initial_env define_class (res, env) (cl, id, ty_id, obj_id, cl_id) =
  (* Temporary abbreviations *)
  let arity = List.length (fst cl.pci_params) in
  let (obj_params, obj_ty, env) = temp_abbrev env obj_id arity in
  let (cl_params, cl_ty, env) = temp_abbrev env cl_id arity in
  
  (* Temporary type for the class constructor *)
  let constr_type = Ctype.newvar () in
  let dummy_cty =
    Tcty_signature
      { cty_self = Ctype.newvar ();
        cty_vars = Vars.empty;
        cty_concr = Concr.empty }
  in
  let dummy_class =
    {cty_params = [];             (* Dummy value *)
     cty_type = dummy_cty;        (* Dummy value *)
     cty_path = unbound_class;
     cty_new =
       match cl.pci_virt with
         Virtual  -> None
       | Concrete -> Some constr_type}
  in
  let env =
    Env.add_cltype ty_id
      {clty_params = [];            (* Dummy value *)
       clty_type = dummy_cty;       (* Dummy value *)
       clty_path = unbound_class} (
    if define_class then
      Env.add_class id dummy_class env
    else
      env)
  in
  ((cl, id, ty_id,
    obj_id, obj_params, obj_ty,
    cl_id, cl_params, cl_ty,
    constr_type, dummy_class)::res,
   env)

let class_infos define_class kind
    (cl, id, ty_id,
     obj_id, obj_params, obj_ty,
     cl_id, cl_params, cl_ty,
     constr_type, dummy_class)
    (res, env) =

  reset_type_variables ();
  Ctype.begin_class_def ();
  
  (* Introduce class parameters *)
  let params =
    try
      List.map (enter_type_variable true) (fst cl.pci_params)
    with Already_bound ->
      raise(Error(snd cl.pci_params, Repeated_parameter))
  in
  
  (* Type the class expression *)
  let (expr, typ) = kind env cl.pci_expr in
  
  Ctype.end_def ();
  
  let sty = Ctype.self_type typ in

  (* Generalize the row variable *)
  let rv = Ctype.row_variable sty in
  let rec limited_generalize rv =
    function
      Tcty_constr (path, params, cty) ->
        List.iter (Ctype.limited_generalize rv) params;
        limited_generalize rv cty
    | Tcty_signature sign ->
        Ctype.limited_generalize rv sign.cty_self;
        Vars.iter (fun _ (_, ty) -> Ctype.limited_generalize rv ty)
          sign.cty_vars
    | Tcty_fun (ty, cty) ->
        Ctype.limited_generalize rv ty;
        limited_generalize rv cty
  in
  List.iter (Ctype.limited_generalize rv) params;
  limited_generalize rv typ;

  (* Check the abbreviation for the object type *)
  let (obj_params', obj_type) = Ctype.instance_class params typ in
  let constr = Ctype.newconstr (Path.Pident obj_id) obj_params in
  begin
    let ty = Ctype.self_type obj_type in
    Ctype.hide_private_methods ty;
    Ctype.close_object ty;
    begin try
      List.iter2 (Ctype.unify env) obj_params obj_params'
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc,
            Bad_parameters (obj_id, constr,
                            Ctype.newconstr (Path.Pident obj_id)
                                            obj_params')))
    end;
    begin try
      Ctype.unify env ty constr
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc,
        Abbrev_type_clash (constr, ty, Ctype.expand_head env constr)))
    end
  end;
  
  (* Check the other temporary abbreviation (#-type) *)
  begin
    let (cl_params', cl_type) = Ctype.instance_class params typ in
    let ty = Ctype.self_type cl_type in
    Ctype.hide_private_methods ty;
    Ctype.set_object_name obj_id (Ctype.row_variable ty) cl_params ty;
    begin try
      List.iter2 (Ctype.unify env) cl_params cl_params'
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc,
            Bad_parameters (cl_id,
                            Ctype.newconstr (Path.Pident cl_id)
                                            cl_params,
                            Ctype.newconstr (Path.Pident cl_id)
                                            cl_params')))
    end;
    begin try
      Ctype.unify env ty cl_ty
    with Ctype.Unify _ ->
      let constr = Ctype.newconstr (Path.Pident cl_id) params in
      raise(Error(cl.pci_loc, Abbrev_type_clash (constr, ty, cl_ty)))
    end
  end;
  
  (* Type of the class constructor *)
  begin try
    Ctype.unify env (constructor_type constr obj_type) constr_type
  with Ctype.Unify trace ->
    raise(Error(cl.pci_loc,
                Constructor_type_mismatch (cl.pci_name, trace)))
  end;

  (* Class and class type temporary definitions *)
  let cltydef =
    {clty_params = params; clty_type = class_body typ;
     clty_path = Path.Pident obj_id}
  and clty =
    {cty_params = params; cty_type = typ;
     cty_path = Path.Pident obj_id;
     cty_new =
       match cl.pci_virt with
         Virtual  -> None
       | Concrete -> Some constr_type}
  in
  dummy_class.cty_type <- typ;
  let env =
    Env.add_cltype ty_id cltydef (
    if define_class then Env.add_class id clty env else env)
  in

  if cl.pci_virt = Concrete then begin
    match virtual_methods typ with
      []   -> ()
    | mets -> raise(Error(cl.pci_loc, Virtual_class(define_class, mets)))
  end;

  (* Misc. *)
  let arity = Ctype.class_type_arity typ in
  let pub_meths =
    let (fields, _) =
      Ctype.flatten_fields (Ctype.object_fields (Ctype.expand_head env obj_ty))
    in
    List.map (function (lab, _, _) -> lab) fields
  in
  
  (* Final definitions *)
  let (params', typ') = Ctype.instance_class params typ in
  let cltydef =
    {clty_params = params'; clty_type = class_body typ';
     clty_path = Path.Pident obj_id}
  and clty =
    {cty_params = params'; cty_type = typ';
     cty_path = Path.Pident obj_id;
     cty_new =
       match cl.pci_virt with
         Virtual  -> None
       | Concrete -> Some constr_type}
  in
  let obj_abbr =
    {type_params = obj_params;
     type_arity = List.length obj_params;
     type_kind = Type_abstract;
     type_manifest = Some obj_ty }
  in
  let (cl_params, cl_ty) =
    Ctype.instance_parameterized_type params (Ctype.self_type typ)
  in
  Ctype.hide_private_methods cl_ty;
  Ctype.set_object_name obj_id (Ctype.row_variable cl_ty) cl_params cl_ty;
  let cl_abbr =
    {type_params = cl_params;
     type_arity = List.length cl_params;
     type_kind = Type_abstract;
     type_manifest = Some cl_ty }
  in
  ((cl, id, clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr,
    arity, pub_meths, expr) :: res,
   env)

let final_env define_class
    (cl, id, clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr,
     arity, pub_meths, expr)
    (res, env) =

  List.iter Ctype.generalize clty.cty_params;
  generalize_class_type clty.cty_type;
  begin match clty.cty_new with
    None -> ()
  | Some ty -> Ctype.generalize ty
  end;
  List.iter Ctype.generalize obj_abbr.type_params;
  begin match obj_abbr.type_manifest with
    None    -> ()
  | Some ty -> Ctype.generalize ty
  end;
  List.iter Ctype.generalize cl_abbr.type_params;
  begin match cl_abbr.type_manifest with
    None    -> ()
  | Some ty -> Ctype.generalize ty
  end;

  if not (closed_class clty) then
    raise(Error(cl.pci_loc, Non_generalizable_class (id, clty)));

  begin match
    Ctype.closed_class clty.cty_params
      (Ctype.signature_of_class_type clty.cty_type)
  with
    None        -> ()
  | Some reason ->
      let printer =
        if define_class then fun () -> Printtyp.class_declaration id clty
        else fun () -> Printtyp.cltype_declaration id cltydef
      in
      raise(Error(cl.pci_loc, Unbound_type_var(printer, reason)))
  end;

  let env =
    Env.add_type obj_id obj_abbr (
    Env.add_type cl_id cl_abbr (
    Env.add_cltype ty_id cltydef (
    if define_class then Env.add_class id clty env else env)))
  in
  ((id, clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr,
    arity, pub_meths, expr)::res,
   env)

(*******************************)

let type_classes define_class kind env cls =
  let cls =
    List.map
      (function cl ->
         (cl,
          Ident.create cl.pci_name, Ident.create cl.pci_name,
          Ident.create cl.pci_name, Ident.create ("#" ^ cl.pci_name)))
      cls
  in
  Ctype.init_def (Ident.current_time ());
  Ctype.begin_class_def ();
  let (res, env) =
    List.fold_left (initial_env define_class) ([], env) cls
  in
  let (res, env) =
    List.fold_right (class_infos define_class kind) res ([], env)
  in
  Ctype.end_def ();
  let (res, env) =
    List.fold_right (final_env define_class) res ([], env)
  in
  (List.rev res, env)

let class_declaration env sexpr =
  let expr = class_expr env env sexpr in
  (expr, expr.cl_type)

let class_description env sexpr =
  let expr = class_type env sexpr in
  (expr, expr)

let class_declarations env cls =
  type_classes true class_declaration env cls

let class_descriptions env cls =
  type_classes true class_description env cls

let class_type_declarations env cls =
  let (decl, env) =
    type_classes false class_description env cls
  in
  (List.map
     (function
          (_, _, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr, _, _, _) ->
        (ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr))
     decl,
   env)

(*******************************)

(* Error report *)

open Format

let report_error = function
  | Repeated_parameter ->
      print_string "A type parameter occurs several times"
  | Unconsistent_constraint trace ->
      Printtyp.unification_error true trace
        (function () ->
           print_string "The class constraints are not consistent : type")
        (function () ->
           print_string "is not compatible with type")
  | Method_type_mismatch (m, trace) ->
      Printtyp.unification_error true trace
        (function () ->
           print_string "The method ";
           print_string m; print_space ();
           print_string "has type")
        (function () ->
           print_string "but is expected to have type")
  | Structure_expected clty ->
      open_box 0;
      print_string
        "This class expression is not a class structure; it has type";
      print_space();
      Printtyp.class_type clty;
      close_box()
  | Cannot_apply clty ->
      print_string
        "This class expression is not a class function, it cannot be applied"
  | Pattern_type_clash ty ->
      (* XXX Trace *)
      (* XXX Revoir message d'erreur *)
      open_box 0;
      print_string "This pattern cannot match self: \
                    it only matches values of type";
      print_space ();
      Printtyp.type_expr ty;
      close_box ()
  | Unbound_class cl ->
      print_string "Unbound class"; print_space ();
      Printtyp.longident cl
  | Unbound_class_2 cl ->
      print_string "The class"; print_space ();
      Printtyp.longident cl; print_space ();
      print_string "is not yet completely defined"
  | Unbound_class_type cl ->
      print_string "Unbound class type"; print_space ();
      Printtyp.longident cl
  | Unbound_class_type_2 cl ->
      print_string "The class type"; print_space ();
      Printtyp.longident cl; print_space ();
      print_string "is not yet completely defined"
  | Abbrev_type_clash (abbrev, actual, expected) ->
      (* XXX Afficher une trace ? *)
      open_box 0;
      Printtyp.reset ();
      Printtyp.mark_loops abbrev; Printtyp.mark_loops actual;
      Printtyp.mark_loops expected;
      print_string "The abbreviation"; print_space ();
      Printtyp.type_expr abbrev; print_space ();
      print_string "expands to type"; print_space ();
      Printtyp.type_expr actual; print_space ();
      print_string "but is used with type"; print_space ();
      Printtyp.type_expr expected;
      close_box ()
  | Constructor_type_mismatch (c, trace) ->
      Printtyp.unification_error true trace
        (function () ->
           print_string "The expression \"new ";
           print_string c;
           print_string "\" has type")
        (function () ->
           print_string "but is used with type")
  | Virtual_class (cl, mets) ->
      open_vbox 0;
      if cl then
        print_string "This class should be virtual"
      else
        print_string "This class type should be virtual";
      print_space ();
      open_box 2;
      print_string "The following methods are undefined :";
      List.iter
        (function met ->
          print_space (); print_string met)
        mets;
      close_box (); close_box()
  | Parameter_arity_mismatch(lid, expected, provided) ->
      open_box 0;
      print_string "The class constructor "; Printtyp.longident lid;
      print_space(); print_string "expects "; print_int expected;
      print_string " type argument(s),"; print_space();
      print_string "but is here applied to "; print_int provided;
      print_string " type argument(s)";
      close_box()
  | Parameter_mismatch trace ->
      Printtyp.unification_error true trace
        (function () ->
           print_string "The type parameter")
        (function () ->
           print_string "does not meet its constraint: it should be")
  | Bad_parameters (id, params, cstrs) ->
      open_box 0;
      Printtyp.reset ();
      Printtyp.mark_loops params; Printtyp.mark_loops cstrs;
      print_string "The abbreviation"; print_space ();
      Printtyp.ident id; print_space ();
      print_string "is used with parameters"; print_space ();
      Printtyp.type_expr params; print_space ();
      print_string "wich are incompatible with constraints"; print_space ();
      Printtyp.type_expr cstrs; print_space ();
      close_box ()
  | Class_match_failure error ->
      Includeclass.report_error error
  | Unbound_val lab ->
      print_string "Unbound instance variable "; print_string lab
  | Unbound_type_var (printer, reason) ->
      open_vbox 0;
      open_box 0;
      print_string "Some type variables are unbound in this type:";
      print_break 1 2;
      printer ();
      close_box ();
      print_space ();
      open_box 0;
      begin match reason with
        Ctype.CC_Method (ty0, lab, ty) ->
    (* XXX Cas ou une row variable n'est pas liee... *)
          Printtyp.reset ();
          Printtyp.mark_loops ty; Printtyp.mark_loops ty0;
          print_string "The method"; print_space ();
          print_string lab; print_space ();
          print_string "has type"; print_break 1 2;
          Printtyp.type_expr ty; print_space ();
          print_string "where"; print_space ();
          Printtyp.type_expr ty0; print_space ();
          print_string "is unbound"
      | Ctype.CC_Value (ty0, lab, ty) ->
          Printtyp.reset ();
          Printtyp.mark_loops ty; Printtyp.mark_loops ty0;
          print_string "The instance variable"; print_space ();
          print_string lab; print_space ();
          print_string "has type"; print_break 1 2;
          Printtyp.type_expr ty; print_space ();
          print_string "where"; print_space ();
          Printtyp.type_expr ty0; print_space ();
          print_string "is unbound"
      end;
      close_box ();
      close_box ()
  | Make_nongen_seltype ty ->
      open_vbox 0;
      open_box 0;
      print_string "Self type should not occur in the non-generic type";
      print_break 1 2;
      Printtyp.type_scheme ty;
      close_box ();
      print_cut ();
      print_string "It would escape the scope of its class";
      close_box ()
  | Non_generalizable_class (id, clty) ->
      open_box 0;
      print_string "The type of this class,"; print_space();
      Printtyp.class_declaration id clty; print_string ","; print_space();
      print_string "contains type variables that cannot be generalized";
      close_box()
