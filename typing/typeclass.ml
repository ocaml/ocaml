(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
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
    Duplicate_method of string
  | Duplicate_variable of string
  | Duplicate_super_variable of string
  | Repeated_parameter
  | Virtual_class of string * string
  | Closed_class of string
  | Closed_ancestor of string * Path.t * string
  | Non_generalizable of Ident.t * type_expr list
  | Non_closed of Ident.t * type_expr list * type_expr *
                  Ctype.closed_schema_result
  | Mutable_var of string
  | Undefined_var of string
  | Variable_type_mismatch of string * (type_expr * type_expr) list
  | Method_type_mismatch of string * (type_expr * type_expr) list
  | Unconsistent_constraint
  | Unbound_class of Longident.t
  | Argument_type_mismatch of (type_expr * type_expr) list
  | Abbrev_type_clash of type_expr * type_expr * type_expr
  | Bad_parameters of Ident.t * type_expr * type_expr
  | Illdefined_class of string
  | Argument_arity_mismatch of Path.t * int * int
  | Parameter_arity_mismatch of Path.t * int * int
  | Parameter_mismatch of (type_expr * type_expr) list

exception Error of Location.t * error


                              (*****************)
                              (*  Common code  *)
                              (*****************)


let rec iter f env =
  function
    [] ->
      ([], env)
  | cl :: cl_rem ->
      let (cl', env') = f env cl in
      let (cl_rem', env'') = iter f env' cl_rem in
      (cl'::cl_rem', env'')

let check_mutable loc lab mut mut' =
  match mut, mut' with
    (Immutable, Mutable) ->
      raise(Error(loc, Mutable_var lab))
  | _ ->
      ()

let rec add_methods env self concr concr_lst t =
  match (Ctype.repr t).desc with
    Tfield (lab, _, t') ->
      Ctype.filter_method env lab self;
      if Concr.mem lab concr_lst then
        (Ctype.filter_method env lab concr; ());
      add_methods env self concr concr_lst t'
  | _ ->
      ()

(* Make sure taht [self] has at least the methods of [obj]. *)
let equalize_methods env self obj =
  match (Ctype.expand_root env obj).desc with
    Tobject (ty, _) ->
      let rec equalize_methods_rec t =
        match (Ctype.repr t).desc with
          Tfield (lab, _, t') ->
            Ctype.filter_method env lab self;
            equalize_methods_rec t'
        | _ ->
            ()
      in
        equalize_methods_rec ty
  | _ ->
      fatal_error "Typeclass.equalize_methods"


let rec type_meth env loc self ty =
  match (Ctype.repr ty).desc with
    Tfield (lab, ty, ty') ->
      let ty0 = Ctype.filter_method env lab self in
      begin try
        Ctype.unify env ty ty0
      with Ctype.Unify trace ->
        raise(Error(loc, Method_type_mismatch (lab, trace)))
      end;
      type_meth env loc self ty'
  | _ ->
      ()

let vals_remove lab vals =
  Vars.fold (fun l k v -> if lab = l then v else Vars.add l k v)
    vals Vars.empty

let insert_value env lab priv mut ty loc vals =
  begin try
    let (mut', ty') = Vars.find lab vals in
    check_mutable loc lab mut mut';
    try Ctype.unify env ty ty' with Ctype.Unify trace ->
      raise(Error(loc, Variable_type_mismatch(lab, trace)))
  with Not_found -> () end;
  if priv = Private then
    vals_remove lab vals
  else
    Vars.add lab (mut, ty) vals

let rec closed_scheme t =
  match (Ctype.repr t).desc with
    Tfield (lab, _, t') ->
    	Ctype.newty (Tfield (lab, Ctype.newvar (), closed_scheme t'))
  | Tnil ->
    	Ctype.newty Tnil
  | _ ->
      fatal_error "Typeclass.closed_scheme"

let change_value_status lab priv mut loc vals =
  try
    let (mut', ty') = Vars.find lab vals in
    check_mutable loc lab mut mut';
    if priv = Private then
      (vals_remove lab vals, ty')
    else
      (Vars.add lab (mut, ty') vals, ty')
  with Not_found ->
    raise(Error(loc, Undefined_var lab))

let missing_method env ty ty' =
  let rec missing_method_rec met=
    match (Ctype.repr met).desc with
      Tfield(lab, _, met') ->
        begin try
      	  Ctype.filter_method env lab ty;
	  missing_method_rec met'
        with Ctype.Unify _ ->
      	  lab
        end
    | _ ->
        fatal_error "Typeclass.missing_method (1)"
  in
  match (Ctype.expand_root env ty').desc with
    Tobject (met, _) ->
      missing_method_rec met
  | _ ->
      fatal_error "Typeclass.missing_method (2)"

let generalize_class cl_sig =
  List.iter Ctype.generalize cl_sig.cty_params;
  List.iter Ctype.generalize cl_sig.cty_args;
  Vars.iter (fun l (m, t) -> Ctype.generalize t) cl_sig.cty_vars;
  Ctype.generalize cl_sig.cty_self;
  match cl_sig.cty_new with Some ty -> Ctype.generalize ty | None -> ()


                              (***********************)
                              (*  Class translation  *)
                              (***********************)


let make_stub env (cl, obj_id, cl_id) =
  Ctype.begin_def ();

  (* Create self (class type) *)
  let self = Ctype.newobj (Ctype.newvar ()) in
  let concr = Ctype.newobj (Ctype.newvar ()) in

  (* Find concrete methods and marks methods *)
  let concr_meths =
    List.fold_left
      (function meths ->
      	 function
	   Pcf_inher (nm, _, _, _, loc) ->
             let (_, anc) =
      	       try
      	         Env.lookup_class nm env
	       with Not_found ->
	         raise(Error(loc, Unbound_class nm))
             in
      	       begin match (Ctype.expand_root env anc.cty_self).desc with
                 Tobject (ty, _) ->
                   add_methods env self concr anc.cty_concr ty;
	           Concr.union anc.cty_concr meths
               | _ -> fatal_error "Typeclass.make_stub"
               end
	 | Pcf_val _ ->
	     meths
	 | Pcf_virt (lab, _, _) ->
	     Ctype.filter_method env lab self;
	     meths
	 | Pcf_meth (lab, _, _) ->
	     Ctype.filter_method env lab self;
             Ctype.filter_method env lab concr;
	     Concr.add lab meths)
      Concr.empty cl.pcl_field
  in
  Ctype.close_object concr;

  Ctype.end_def ();
  Ctype.generalize self;
  Ctype.generalize concr;

  (* Temporary object type *)
  let temp_obj_params =
    List.map (fun _ -> Ctype.newvar ()) (fst cl.pcl_param)
  in
  let temp_obj = Ctype.instance self in
  let obj_temp_abbrev =
    { type_params = temp_obj_params;
      type_arity = List.length temp_obj_params;
      type_kind = Type_abstract;
      type_manifest = Some temp_obj }
  in let temp_env = Env.add_type obj_id obj_temp_abbrev env
  in let abbrev =
    Ctype.newty (Tconstr (Path.Pident obj_id, temp_obj_params, ref Mnil))
  in

  (* Temporary class type *)
  let (temp_cl_params, temp_cl) =
    if cl.pcl_closed = Closed then
      (temp_obj_params,
       Ctype.newty (Tconstr(Path.Pident obj_id, temp_obj_params, ref Mnil)))
    else begin
      let params = List.map (fun _ -> Ctype.newvar ()) (fst cl.pcl_param) in
      let ty = Ctype.instance self in
      Ctype.set_object_name ty params obj_id;
      (params, ty)
    end
  in
  let cl_temp_abbrev =
    { type_params = temp_cl_params;
      type_arity = List.length temp_cl_params;
      type_kind = Type_abstract;
      type_manifest = Some temp_cl }
  in let temp_env = Env.add_type cl_id cl_temp_abbrev temp_env
  in let cl_abbrev =
    Ctype.newty (Tconstr (Path.Pident cl_id, temp_cl_params, ref Mnil))
  in

  (* Temporary type for new *)
  let new_args = List.map (fun _ -> Ctype.newvar ()) cl.pcl_args in
  let new_ty =
    if cl.pcl_kind = Concrete then
      Some (List.fold_right
              (fun arg ty -> Ctype.newty (Tarrow(arg, ty)))
              new_args abbrev)
    else
      None  
  in let cl_temp_sig =
    { cty_params = [];
      cty_args = []; cty_vars = Vars.empty;
      cty_self = self; cty_concr = concr_meths;
      cty_new = new_ty }
  in let (id, temp_env) = Env.enter_class cl.pcl_name cl_temp_sig temp_env in

  ((cl, id, cl_id, obj_id, self, concr, concr_meths, new_args, new_ty,
    temp_cl, temp_cl_params, cl_abbrev, temp_obj, temp_obj_params, abbrev),
   temp_env)

let type_class_field env var_env self cl (met_env, fields, vars_sig) =
  function
    Pcf_inher (cl_name, params, args, super, loc) ->
      (* Find class type *)
      let (path, cl_type) =
        try
          Env.lookup_class cl_name env
        with Not_found ->
          raise(Error(loc, Unbound_class cl_name))
      in
      let (params', args', vars', self') = Ctype.instance_class cl_type in

      (* Unify parameters *)
      if List.length params <> List.length params' then
        raise(Error(loc, Parameter_arity_mismatch
                    (path, List.length params', List.length params)));
      List.iter2
        (fun sty ty ->
           let ty' = Typetexp.transl_simple_type var_env false sty in
           try Ctype.unify var_env ty' ty with Ctype.Unify trace ->
             raise(Error(sty.ptyp_loc, Parameter_mismatch trace)))
        params params';

      (* Type arguments *)
      if List.length args <> List.length args' then
        raise(Error(loc, Argument_arity_mismatch
                        (path, List.length args', List.length args)));
      let args = List.map2 (type_expect var_env) args args' in

      (* Variables *)
      let (vars, vars_sig, met_env) =
        Vars.fold
          (fun lab (mut, ty) (l, v_sig, env) ->
             let (id, env) =
               Env.enter_value lab
                 {val_type = ty; val_kind = Val_ivar mut} env
             in
             ((lab, id)::l,
              insert_value var_env lab Public mut ty loc v_sig,
              env))
          vars' ([], vars_sig, met_env)
      in

      (* Self type *)
      let ty' = Ctype.expand_root var_env self' in
      begin match ty'.desc with
        Tobject (fi, _) ->
          if ty' != Ctype.expand_root var_env self then begin
            if not (Ctype.opened_object self') then
              begin try
                Ctype.unify var_env self (Ctype.newobj (closed_scheme fi))
              with Ctype.Unify _ ->
                let lab = missing_method var_env self' self in
                raise(Error(loc, Closed_ancestor (cl.pcl_name, path, lab)))
              end;
            ty'.desc <- Tlink self;
            type_meth var_env loc self fi
          end
      | _ ->
          fatal_error "Typeclass.transl_class"
      end;

      (* Super methods *)
      let (met, met_env) =
        match super with
          None ->
            ([], met_env)
        | Some name ->
            let ty = Ctype.newobj (Ctype.newvar ()) in
            let used_methods =
              Concr.fold
                (fun lab rem ->
                   Ctype.unify met_env (Ctype.filter_method met_env lab ty)
                                       (Ctype.filter_method met_env lab self);
                   (lab, Ident.create lab)::rem)
                cl_type.cty_concr
                []
            in
            Ctype.close_object ty;
            let (id, met_env) =
              Env.enter_value name
                {val_type = ty; val_kind = Val_anc used_methods} met_env
            in
            (used_methods, met_env)
      in
      (met_env, Cf_inher (path, args, vars, met)::fields, vars_sig)

  | Pcf_val (lab, priv, mut, sinit, loc) ->
      begin match sinit with
        Some sexp ->
          let exp = type_exp var_env sexp in
          let (id, met_env) =
            Env.enter_value lab
              {val_type = exp.exp_type; val_kind = Val_ivar mut} met_env
          in
          (met_env, Cf_val (lab, id, priv, Some exp)::fields,
      	   insert_value var_env lab priv mut exp.exp_type loc vars_sig)
      | None ->
      	  let (vars_sig, ty) =
            change_value_status lab priv mut loc vars_sig
          in
          let (id, met_env) =
            Env.enter_value lab
              {val_type = ty; val_kind = Val_ivar mut} met_env
          in
          (met_env, Cf_val (lab, id, priv, None)::fields, vars_sig)
      end

  | Pcf_virt (lab, ty, loc) ->
      let ty = transl_simple_type met_env false ty in
      let ty' = Ctype.filter_method var_env lab self in
      begin try Ctype.unify var_env ty ty' with Ctype.Unify trace ->
        raise(Error(loc, Method_type_mismatch (lab, trace)))
      end;
      (met_env, fields, vars_sig)

  | Pcf_meth (lab, expr, loc)  ->
      let ty = Ctype.filter_method var_env lab self in
      let texp = type_method met_env self cl.pcl_self expr ty in
      (met_env, Cf_meth (lab, texp)::fields, vars_sig)
  
let transl_class temp_env env
  (cl, id, cl_id, obj_id, self, concr, concr_meths, new_args, new_ty,
   temp_cl, temp_cl_params, cl_abbrev, temp_obj, temp_obj_params, abbrev)
    =
  reset_type_variables ();
  Ctype.begin_def ();

  (* Self type *)
  let self = Ctype.instance self in

  (* Introduce parameters *)
  let params =
    try
      List.map (enter_type_variable true) (fst cl.pcl_param)
    with Already_bound ->
      raise(Error(snd cl.pcl_param, Repeated_parameter))
  in

  (* Bind self type variable *)
  begin match cl.pcl_self_ty with
      Some v -> Ctype.unify temp_env self (enter_type_variable false v)
    | None   -> ()
  end;

  (* Add constraints *)
  List.iter
    (function (v, ty, loc) ->
       try
         Ctype.unify temp_env
           (type_variable loc v) (transl_simple_type temp_env false ty)
       with Ctype.Unify _ ->
         raise(Error(loc, Unconsistent_constraint)))
    cl.pcl_cstr;

  (* Type arguments and fields *)
  let (args, var_env) = type_pattern_list temp_env cl.pcl_args in
  let arg_sig = List.map (fun exp -> exp.pat_type) args in
  let (_, fields, vars_sig) =
    List.fold_left (type_class_field env var_env self cl)
      (temp_env, [], Vars.empty)
      cl.pcl_field
  in

  (* Closeness of self (1) *)
  if cl.pcl_closed = Closed then
    Ctype.close_object self;

  (* Generalize class *)
  Ctype.end_def ();
  List.iter Ctype.generalize params;
  List.iter Ctype.generalize arg_sig;
  Vars.iter (fun l (m, t) -> Ctype.generalize t) vars_sig;
  Ctype.generalize self;

  (* Temporary class abbreviation *)
  let (cl_params, cl_ty) = Ctype.instance_parameterized_type params self in
  begin try Ctype.unify temp_env temp_cl cl_ty with Ctype.Unify _ ->
    Ctype.remove_object_name temp_cl;
    raise(Error(cl.pcl_loc, Abbrev_type_clash (cl_abbrev, cl_ty, temp_cl)))
  end;
  begin try
    List.iter2 (Ctype.unify temp_env) temp_cl_params cl_params
  with Ctype.Unify _ ->
    raise(Error(cl.pcl_loc,
      	  Bad_parameters (cl_id, cl_abbrev,
      	                  Ctype.newty (Tconstr (Path.Pident cl_id, cl_params,
      	       	       	       	       	        ref Mnil)))))
  end;

  (* Object abbreviation and arguments for new *)
  let (obj_params, arg_sig', obj_ty) =
    Ctype.instance_parameterized_type_2 params arg_sig self
  in
  begin try Ctype.unify temp_env abbrev obj_ty with Ctype.Unify _ ->
    raise(Error(cl.pcl_loc, Abbrev_type_clash (abbrev, obj_ty, temp_obj)))
  end;
  begin try
    List.iter2 (Ctype.unify temp_env) temp_obj_params obj_params
  with Ctype.Unify _ ->
    raise(Error(cl.pcl_loc,
          Bad_parameters (obj_id, abbrev,
       	       	          Ctype.newty (Tconstr (Path.Pident obj_id, obj_params,
      	       	       	       	       	        ref Mnil)))))
  end;
  Ctype.close_object temp_obj;
  List.iter2
    (fun ty (exp, ty') ->
       begin try
	 Ctype.unify temp_env ty' ty
       with Ctype.Unify trace ->
	 raise(Error(exp.pat_loc, Argument_type_mismatch trace))
       end)
    new_args (List.combine args arg_sig');

  (* Fill interface / implementation *)
  let cl_imp =
    { cl_args = args;
      cl_field = List.rev fields;
      cl_loc = cl.pcl_loc }
  in let cl_sig =
    { cty_params = params;
      cty_args = arg_sig;
      cty_vars = vars_sig;
      cty_self = self;
      cty_concr = concr_meths;
      cty_new = new_ty }
  in let new_env = Env.add_class id cl_sig env in

  ((cl, id, cl_id, obj_id, cl_sig, cl_imp, concr, temp_obj,
    temp_obj_params),
   new_env)

let build_new_type temp_env env
  (cl, id, cl_id, obj_id, cl_sig, cl_imp, concr, temp_obj,
   temp_obj_params)
    =
  (* Modify constrainsts to ensure the object abbreviation is well-formed *)
  let (params, args, vars, self) = Ctype.instance_class cl_sig in
  List.iter2 (Ctype.unify temp_env) params temp_obj_params;
					(* Never fails *)

  (* Closeness of self (2) *)
  if (cl.pcl_closed <> Closed) & not (Ctype.opened_object self) then
    raise(Error(cl.pcl_loc, Closed_class cl.pcl_name));

  (* Check whether the class can be concrete *)
  if cl.pcl_kind = Concrete then begin
    let concr = Ctype.instance concr in
    try
      Ctype.unify temp_env concr temp_obj
    with Ctype.Unify _ ->
      let lab = missing_method temp_env concr temp_obj in
      raise(Error(cl.pcl_loc, Virtual_class (cl.pcl_name, lab)))
  end;

  equalize_methods temp_env self temp_obj;

  (* self should not be an abbreviation (printtyp) *)
  let exp_self = Ctype.expand_root temp_env self in

  (* Final class type *)
  let cl_sig =
    { cty_params = params;
      cty_args = args;
      cty_vars = vars;
      cty_self = exp_self;
      cty_concr = cl_sig.cty_concr;
      cty_new = cl_sig.cty_new }	(* new is still monomorphic *)
  in let new_env = Env.add_class id cl_sig env in

  ((cl, id, cl_id, obj_id, cl_sig, cl_imp), new_env)

let make_abbrev env
  (cl, id, cl_id, obj_id, cl_sig, cl_imp)
  =
  (* Class type abbreviation *)
  let cl_abbrev =
    { type_params = cl_sig.cty_params;
      type_arity = List.length cl_sig.cty_params;
      type_kind = Type_abstract;
      type_manifest = Some
        (if cl.pcl_closed = Closed then
	  Ctype.newgenty (Tconstr(Path.Pident obj_id, cl_sig.cty_params,
                                  ref Mnil))
        else begin
          Ctype.set_object_name cl_sig.cty_self cl_sig.cty_params obj_id;
          cl_sig.cty_self
	end) }
  in let new_env = Env.add_type cl_id cl_abbrev env in

  (* Object type abbreviation *)
  Ctype.begin_def ();
  let (obj_ty_params, obj_ty) =
    Ctype.instance_parameterized_type cl_sig.cty_params cl_sig.cty_self
  in
  Ctype.close_object obj_ty;
  Ctype.end_def ();
  List.iter Ctype.generalize obj_ty_params;
  if not (List.for_all Ctype.closed_schema obj_ty_params) then
    raise(Error(cl.pcl_loc,
                Non_generalizable(obj_id, obj_ty_params)));
  begin match Ctype.closed_schema_verbose obj_ty with
    None -> ()
  | Some v ->
      raise(Error(cl.pcl_loc,
                  Non_closed(obj_id, obj_ty_params, obj_ty, v)))
  end;
  Ctype.generalize obj_ty;
  let obj_abbrev =
    { type_params = obj_ty_params;
      type_arity = List.length obj_ty_params;
      type_kind = Type_abstract;
      type_manifest = Some (Ctype.unroll_abbrev obj_id obj_ty_params obj_ty) }
  in let new_env = Env.add_type obj_id obj_abbrev new_env in

  ((id, cl_sig, cl_id, cl_abbrev, obj_id, obj_abbrev, cl_imp), new_env)

let transl_classes env cl =
  let info =
    List.map
      (function cl ->
        (cl, Ident.create cl.pcl_name, Ident.create ("#" ^ cl.pcl_name)))
    cl
  in
  Ctype.init_def (Ident.current_time());
  Ctype.begin_def ();
  let (info, temp_env) = iter make_stub env info in
  let (info, _) = iter (transl_class temp_env) env info in
  let (info, new_env) = iter (build_new_type temp_env) env info in
  Ctype.end_def ();
  List.iter (fun (_, _, _, _, cl_sig, _) -> generalize_class cl_sig) info;
  let (info, new_env) = iter make_abbrev new_env info in
  (info, new_env)


                              (****************************)
                              (*  Class type translation  *)
                              (****************************)


let make_stub env (cl, obj_id, cl_id) =
  Ctype.begin_def ();

  (* Create self (class type) *)
  let self = Ctype.newobj (Ctype.newvar ()) in
  let concr = Ctype.newobj (Ctype.newvar ()) in

  (* Find concrete methods and marks methods *)
  let concr_meths =
    List.fold_left
      (function meths ->
      	 function
	   Pctf_inher (nm, _, loc) ->
             let (_, anc) =
      	       try
      	         Env.lookup_class nm env
	       with Not_found ->
	         raise(Error(loc, Unbound_class nm))
             in
      	       begin match (Ctype.expand_root env anc.cty_self).desc with
                 Tobject (ty, _) ->
                   add_methods env self concr anc.cty_concr ty;
	           Concr.union anc.cty_concr meths
               | _ -> fatal_error "Typeclass.make_stub (type)"
               end
	 | Pctf_val _ ->
	     meths
	 | Pctf_virt (lab, _, _) ->
	     Ctype.filter_method env lab self;
	     meths
	 | Pctf_meth (lab, _, _) ->
	     Ctype.filter_method env lab self;
             Ctype.filter_method env lab concr;
	     Concr.add lab meths)
      Concr.empty cl.pcty_field
  in
  Ctype.close_object concr;

  Ctype.end_def ();
  Ctype.generalize self;
  Ctype.generalize concr;

  (* Temporary object type *)
  let temp_obj_params =
    List.map (fun _ -> Ctype.newvar ()) (fst cl.pcty_param)
  in
  let temp_obj = Ctype.instance self in
  let obj_temp_abbrev =
    { type_params = temp_obj_params;
      type_arity = List.length temp_obj_params;
      type_kind = Type_abstract;
      type_manifest = Some temp_obj }
  in
  let temp_env = Env.add_type obj_id obj_temp_abbrev env in
  let abbrev =
    Ctype.newty (Tconstr (Path.Pident obj_id, temp_obj_params, ref Mnil))
  in

  (* Temporary class type *)
  let (temp_cl_params, temp_cl) =
    if cl.pcty_closed = Closed then
      (temp_obj_params,
       Ctype.newty (Tconstr(Path.Pident obj_id, temp_obj_params, ref Mnil)))
    else begin
      let params = List.map (fun _ -> Ctype.newvar ()) (fst cl.pcty_param) in
      let ty = Ctype.instance self in
      Ctype.set_object_name ty params obj_id;
      (params, ty)
    end
  in
  let cl_temp_abbrev =
    { type_params = temp_cl_params;
      type_arity = List.length temp_cl_params;
      type_kind = Type_abstract;
      type_manifest = Some temp_cl }
  in let temp_env = Env.add_type cl_id cl_temp_abbrev temp_env
  in let cl_abbrev =
    Ctype.newty (Tconstr (Path.Pident cl_id, temp_cl_params, ref Mnil))
  in

  (* Temporary class type *)
  let cl_temp_sig =
    { cty_params = [];
      cty_args = []; cty_vars = Vars.empty;
      cty_self = self; cty_concr = concr_meths;
      cty_new = None }
  in
  let (id, temp_env) = Env.enter_class cl.pcty_name cl_temp_sig temp_env in

  ((cl, id, cl_id, obj_id, self, concr, concr_meths, temp_cl,
    temp_cl_params, cl_abbrev, temp_obj, temp_obj_params, abbrev),
   temp_env)

let type_class_field env var_env self cl vars_sig =
  function
    Pctf_inher (cl_name, params, loc) ->
      (* Find class type *)
      let (path, cl_type) =
        try
          Env.lookup_class cl_name env
        with Not_found ->
          raise (Error (loc, Unbound_class cl_name))
      in
      let (params', _, vars', self') = Ctype.instance_class cl_type in

      (* Unify parameters *)
      if List.length params <> List.length params' then
        raise(Error(loc, Parameter_arity_mismatch
                    (path, List.length params', List.length params)));
      List.iter2
        (fun sty ty ->
           let ty' = Typetexp.transl_simple_type var_env false sty in
           try Ctype.unify var_env ty' ty with Ctype.Unify trace ->
             raise(Error(sty.ptyp_loc, Parameter_mismatch trace)))
        params params';

      let vars_sig =
        Vars.fold
          (fun lab (mut, ty) v_sig ->
             insert_value var_env lab Public mut ty loc v_sig)
          vars' vars_sig
      in

      (* Self type *)
      let ty' = Ctype.expand_root var_env self' in
      begin match ty'.desc with
	Tobject (fi, _) ->
	  if not (Ctype.opened_object self') then
	    begin try
	      Ctype.unify var_env self (Ctype.newobj (closed_scheme fi))
	    with Ctype.Unify _ ->
	      let lab = missing_method var_env self' self in
	      raise(Error(loc, Closed_ancestor (cl.pcty_name, path, lab)))
	      end;
	    ty'.desc <- Tlink self;
	    type_meth var_env loc self fi
	| _ ->
	    fatal_error "Typeclass.type_class_field (type)"
      end;

      vars_sig

  | Pctf_val (lab, priv, mut, sty, loc) ->
      begin match sty with
      	Some sty ->
          let ty = transl_simple_type var_env false sty in
          insert_value var_env lab priv mut ty loc vars_sig
      | None ->
      	  fst (change_value_status lab priv mut loc vars_sig)
      end

  | Pctf_virt (lab, sty, loc) ->
      let ty = transl_simple_type var_env false sty in
      let ty' = Ctype.filter_method var_env lab self in
      begin try Ctype.unify var_env ty ty' with Ctype.Unify trace ->
        raise(Error(loc, Method_type_mismatch (lab, trace)))
      end;
      vars_sig

  | Pctf_meth (lab, Some sty, loc) ->
      let ty = transl_simple_type var_env false sty in
      let ty' = Ctype.filter_method var_env lab self in
      begin try Ctype.unify var_env ty ty' with Ctype.Unify trace ->
        raise(Error(loc, Method_type_mismatch (lab, trace)))
      end;
      vars_sig

  | Pctf_meth (lab, None, loc) ->
      Ctype.filter_method var_env lab self;
      vars_sig

let transl_class temp_env env
  (cl, id, cl_id, obj_id, self, concr, concr_meths, temp_cl,
   temp_cl_params, cl_abbrev, temp_obj, temp_obj_params, abbrev)
    =
  reset_type_variables ();
  Ctype.begin_def ();

  (* Self type *)
  let self = Ctype.instance self in

  (* Introduce parameters *)
  let params =
    try
      List.map (enter_type_variable true) (fst cl.pcty_param)
    with Already_bound ->
      raise(Error(snd cl.pcty_param, Repeated_parameter))
  in

  (* Bind self type variable *)
  begin match cl.pcty_self with
      Some v -> Ctype.unify temp_env self (enter_type_variable false v)
    | None   -> ()
  end;

  (* Add constraints *)
  List.iter
    (function (v, ty, loc) ->
       try
         Ctype.unify temp_env
           (type_variable loc v) (transl_simple_type temp_env false ty)
       with Ctype.Unify _ ->
         raise(Error(loc, Unconsistent_constraint)))
    cl.pcty_cstr;

  (* Translate argument types *)
  let arg_sig = List.map (transl_simple_type temp_env false) cl.pcty_args in

  (* Translate fields *)
  let vars_sig =
    List.fold_left (type_class_field env temp_env self cl)
      Vars.empty cl.pcty_field
  in

  (* Closeness of self *)
  if cl.pcty_closed = Closed then
    Ctype.close_object self;

  (* Generalize class *)
  Ctype.end_def ();
  List.iter Ctype.generalize params;
  List.iter Ctype.generalize arg_sig;
  Vars.iter (fun l (m, t) -> Ctype.generalize t) vars_sig;
  Ctype.generalize self;

  (* Temporary class abbreviation *)
  let (cl_params, cl_ty) = Ctype.instance_parameterized_type params self in
  begin try Ctype.unify temp_env temp_cl cl_ty with Ctype.Unify _ ->
    Ctype.remove_object_name temp_cl;
    raise(Error(cl.pcty_loc, Abbrev_type_clash (cl_abbrev, cl_ty, temp_cl)))
  end;
  begin try
    List.iter2 (Ctype.unify temp_env) temp_cl_params cl_params
  with Ctype.Unify _ ->
    raise(Error(cl.pcty_loc,
      	  Bad_parameters (cl_id, cl_abbrev,
      	                  Ctype.newty (Tconstr (Path.Pident cl_id, cl_params,
      	       	       	       	       	        ref Mnil)))))
  end;

  (* Object abbreviation and arguments for new *)
  let (obj_params, arg_sig', obj_ty) =
    Ctype.instance_parameterized_type_2 params arg_sig self
  in
  begin try Ctype.unify temp_env abbrev obj_ty with Ctype.Unify _ ->
    raise(Error(cl.pcty_loc, Abbrev_type_clash (abbrev, obj_ty, temp_obj)))
  end;
  begin try
    List.iter2 (Ctype.unify temp_env) temp_obj_params obj_params
  with Ctype.Unify _ ->
    raise(Error(cl.pcty_loc,
          Bad_parameters (obj_id, abbrev,
       	       	          Ctype.newty (Tconstr (Path.Pident obj_id, obj_params,
      	       	       	       	       	        ref Mnil)))))
  end;
  Ctype.close_object temp_obj;

  (* Temporary class signature *)
  let cl_sig =
    { cty_params = params;
      cty_args = arg_sig;
      cty_vars = vars_sig;
      cty_self = self;
      cty_concr = concr_meths;
      cty_new = None }
  in
  let new_env = Env.add_class id cl_sig env in

  ((cl, id, cl_id, obj_id, cl_sig, concr, abbrev, temp_obj,
    temp_obj_params),
   new_env)

let build_new_type temp_env env
  (cl, id, cl_id, obj_id, cl_sig, concr, abbrev, temp_obj,
   temp_obj_params)
    =
  (* Modify constrainsts to ensure the object abbreviation is well-formed *)
  let (params, args, vars, self) = Ctype.instance_class cl_sig in
  List.iter2 (Ctype.unify temp_env) params temp_obj_params;
					(* Never fails *)

  (* Closeness of self (2) *)
  if (cl.pcty_closed <> Closed) & not (Ctype.opened_object self) then
    raise(Error(cl.pcty_loc, Closed_class cl.pcty_name));

  (* Check whether the class can be concrete *)
  if cl.pcty_kind = Concrete then begin
    let concr = Ctype.instance concr in
    try
      Ctype.unify temp_env concr temp_obj
    with Ctype.Unify _ ->
      let lab = missing_method temp_env concr temp_obj in
      raise(Error(cl.pcty_loc, Virtual_class (cl.pcty_name, lab)))
  end;

  equalize_methods temp_env self temp_obj;

  (* self should not be an abbreviation (printtyp) *)
  let exp_self = Ctype.expand_root temp_env self in

  let new_ty =
    if cl.pcty_kind = Concrete then
      Some (List.fold_right
              (fun arg ty -> Ctype.newty (Tarrow(arg, ty)))
              args abbrev)
    else
      None
  in

  (* Final class type *)
  let cl_sig =
    { cty_params = params;
      cty_args = args;
      cty_vars = vars;
      cty_self = exp_self;
      cty_concr = cl_sig.cty_concr;
      cty_new = new_ty }
  in let new_env = Env.add_class id cl_sig env in

  ((cl, id, cl_id, obj_id, cl_sig), new_env)

let make_abbrev env
  (cl, id, cl_id, obj_id, cl_sig)
    =
  (* Class type abbreviation *)
  let cl_abbrev =
    { type_params = cl_sig.cty_params;
      type_arity = List.length cl_sig.cty_params;
      type_kind = Type_abstract;
      type_manifest = Some
        (if cl.pcty_closed = Closed then
	  Ctype.newgenty (Tconstr(Path.Pident obj_id, cl_sig.cty_params,
                                  ref Mnil))
        else begin
          Ctype.set_object_name cl_sig.cty_self cl_sig.cty_params obj_id;
          cl_sig.cty_self
	end) }
  in let new_env = Env.add_type cl_id cl_abbrev env in

  (* Object type abbreviation *)
  Ctype.begin_def ();
  let (obj_ty_params, obj_ty) =
    Ctype.instance_parameterized_type cl_sig.cty_params cl_sig.cty_self
  in
  Ctype.close_object obj_ty;
  Ctype.end_def ();
  List.iter Ctype.generalize obj_ty_params;
  begin match Ctype.closed_schema_verbose obj_ty with
    None -> ()
  | Some v ->
      raise(Error(cl.pcty_loc,
                  Non_closed(obj_id, obj_ty_params, obj_ty, v)))
  end;
  Ctype.generalize obj_ty;
  let obj_abbrev =
    { type_params = obj_ty_params;
      type_arity = List.length obj_ty_params;
      type_kind = Type_abstract;
      type_manifest = Some (Ctype.unroll_abbrev obj_id obj_ty_params obj_ty) }
  in let new_env = Env.add_type obj_id obj_abbrev new_env in

  ((id, cl_sig, cl_id, cl_abbrev, obj_id, obj_abbrev), new_env)

let rec transl_class_types env cl =
  let info =
    List.map
      (function cl ->
         (cl, Ident.create cl.pcty_name, Ident.create ("#" ^ cl.pcty_name)))
      cl
  in
  Ctype.init_def(Ident.current_time());
  Ctype.begin_def();
  let (info, temp_env) = iter make_stub env info in
  let (info, _) = iter (transl_class temp_env) env info in
  let (info, new_env) = iter (build_new_type temp_env) env info in
  Ctype.end_def ();
  List.iter (fun (_, _, _, _, cl_sig) -> generalize_class cl_sig) info;
  let (info, new_env) = iter make_abbrev new_env info in
  (info, new_env)


(* Error report *)

open Format

let report_error = function
    Duplicate_method s ->
      print_string "Two methods are named"; print_space ();
      print_string s
  | Duplicate_variable s ->
      print_string "Two instance variables are named"; print_space ();
      print_string s
  | Duplicate_super_variable s ->
      print_string "Two ancestors are named"; print_space ();
      print_string s
  | Repeated_parameter ->
      print_string "A type parameter occurs several times"
  | Virtual_class (cl, met) ->
      print_string "The class"; print_space ();
      print_string cl; print_space ();
      print_string "should be virtual: its method"; print_space ();
      print_string met; print_space ();
      print_string "is undefined"
  | Closed_class cl ->
      print_string "The class"; print_space ();
      print_string cl; print_space ();
      print_string "is closed, but not marked closed"
  | Closed_ancestor (cl, anc, met) ->
      print_string "The class"; print_space ();
      print_string cl; print_space ();
      print_string "inherits from the closed class"; print_space ();
      Printtyp.path anc; print_space ();
      print_string "which has no method"; print_space ();
      print_string met
  | Non_generalizable (id, params) ->
      open_box 0;
      Printtyp.reset ();
      List.iter Printtyp.mark_loops params;
      print_string "The";
      List.iter (fun w -> print_space (); print_string w)
        ["type"; "parameters"; "of"; "this"; "class"; "contains";
         "type"; "variables"; "that"; "cannot"; "be"; "generalized:"];
      print_break 1 2;
      Printtyp.type_scheme
        {desc = Tconstr(Path.Pident id, params, ref Mnil); level = 0};
      close_box ()
  | Non_closed (id, args, typ, var) ->
      open_box 0;
      Printtyp.reset ();
      List.iter Printtyp.mark_loops args;
      Printtyp.mark_loops typ;
      begin match var with
        Ctype.Var v ->
          print_string "The type variable"; print_space ();
          Printtyp.type_expr v; print_space ();
          print_string "is not bound in implicit type definition"
      | _ ->
          print_string "Unbound row variable in implicit type definition"
      end;
      print_break 1 2;
      open_box 0;
      Printtyp.type_expr
        (Ctype.newty (Tconstr(Path.Pident id, args, ref Mnil)));
      print_space (); print_string "="; print_space ();
      Printtyp.type_expr typ;
      close_box ();
      close_box ();
      print_space ();
      print_string "It should be captured by a class type parameter"
  | Mutable_var v ->
      print_string "The variable"; print_space ();
      print_string v; print_space ();
      print_string "was mutable and is redefined as immutable"
  | Undefined_var v ->
      print_string "The variable"; print_space ();
      print_string v; print_space ();
      print_string "is undefined"
  | Variable_type_mismatch (v, trace) ->
      Printtyp.unification_error trace
        (function () ->
           print_string "The variable ";
           print_string v; print_space ();
           print_string "has type")
        (function () ->
           print_string "but is expected to have type")
  | Method_type_mismatch (m, trace) ->
      Printtyp.unification_error trace
        (function () ->
           print_string "The method ";
           print_string m; print_space ();
           print_string "has type")
        (function () ->
           print_string "but is expected to have type")
  | Unconsistent_constraint ->
      print_string "The class constraints are not consistent"
  | Unbound_class cl ->
      print_string "Unbound class"; print_space ();
      Printtyp.longident cl
  | Argument_type_mismatch trace ->
      Printtyp.unification_error trace
        (function () ->
           print_string "This argument has type")
        (function () ->
           print_string "but is expected to have type")
  | Abbrev_type_clash (abbrev, actual, expected) ->
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
  | Illdefined_class s ->
      print_string "The class "; print_string s;
      print_string " is ill-defined"
  | Parameter_mismatch trace ->
      Printtyp.unification_error trace
        (function () ->
           print_string "The type parameter")
        (function () ->
           print_string "does not meet its constraint: it should be")
  | Argument_arity_mismatch(p, expected, provided) ->
      open_box 0;
      print_string "The class "; Printtyp.path p;
      print_space(); print_string "expects "; print_int expected;
      print_string " argument(s),"; print_space();
      print_string "but is here applied to "; print_int provided;
      print_string " argument(s)";
      close_box()
  | Parameter_arity_mismatch(p, expected, provided) ->
      open_box 0;
      print_string "The class "; Printtyp.path p;
      print_space(); print_string "expects "; print_int expected;
      print_string " type parameter(s),"; print_space();
      print_string "but is here applied to "; print_int provided;
      print_string " type parameter(s)";
      close_box()
