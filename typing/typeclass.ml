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
  | Non_closed of Ident.t * type_expr list * type_expr
  | Mutable_var of string
  | Undefined_var of string
  | Variable_type_mismatch of string * type_expr * type_expr
  | Method_type_mismatch of string * type_expr * type_expr
  | Unconsistent_constraint
  | Unbound_class of Longident.t
  | Argument_type_mismatch of type_expr * type_expr
  | Abbrev_type_clash of type_expr * type_expr * type_expr
  | Bad_parameters of Ident.t * type_expr * type_expr
  | Illdefined_class of string
  | Argument_arity_mismatch of Path.t * int * int
  | Parameter_arity_mismatch of Path.t * int * int
  | Parameter_mismatch of type_expr * type_expr

exception Error of Location.t * error

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
      if List.exists ((=) lab) concr_lst then
        (Ctype.filter_method env lab concr; ());
      add_methods env self concr concr_lst t'
  | _ ->
      ()

let make_stub env cl =
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
	           Sort.merge (<) meths anc.cty_concr
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
	     Sort.merge (<) meths [lab])
      [] cl.pcl_field
  in
  Ctype.close_object concr;

  Ctype.end_def ();
  Ctype.generalize self;
  Ctype.generalize concr;

  (* Temporary object type *)
  let temp_obj_params = List.map (fun _ -> Ctype.newvar ()) cl.pcl_param in
  let temp_obj = Ctype.instance self in
  let obj_temp_abbrev =
    { type_params = temp_obj_params;
      type_arity = List.length temp_obj_params;
      type_kind = Type_abstract;
      type_manifest = Some temp_obj }
  in let (obj_id, temp_env) = Env.enter_type cl.pcl_name obj_temp_abbrev env
  in let abbrev =
    Ctype.newty (Tconstr (Path.Pident obj_id, temp_obj_params, ref []))
  in

  (* Temporary class type *)
  let (temp_cl_params, temp_cl) =
    if cl.pcl_closed = Closed then
      (temp_obj_params,
       Ctype.newty (Tconstr(Path.Pident obj_id, temp_obj_params, ref [])))
    else begin
      let params = List.map (fun _ -> Ctype.newvar ()) cl.pcl_param in
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
  in let (cl_id, temp_env) =
    Env.enter_type ("#" ^ cl.pcl_name) cl_temp_abbrev temp_env
  in let cl_abbrev =
    Ctype.newty (Tconstr (Path.Pident cl_id, temp_cl_params, ref []))
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

let rec type_meth env loc self ty =
  match (Ctype.repr ty).desc with
    Tfield (lab, ty, ty') ->
      let ty0 = Ctype.filter_method env lab self in
      begin try
        Ctype.unify env ty0 ty
      with Ctype.Unify ->
        raise(Error(loc, Method_type_mismatch (lab, ty, ty0)))
      end;
      type_meth env loc self ty'
  | _ ->
      ()

let missing_method env ty ty' =
  let rec missing_method_rec met=
    match (Ctype.repr met).desc with
      Tfield(lab, _, met') ->
        begin try
      	  Ctype.filter_method env lab ty;
	  missing_method_rec met'
        with Ctype.Unify ->
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

let rec closed_scheme t =
  match (Ctype.repr t).desc with
    Tfield (lab, _, t') ->
    	Ctype.newty (Tfield (lab, Ctype.newvar (), closed_scheme t'))
  | Tnil ->
    	Ctype.newty Tnil
  | _ ->
      fatal_error "Typeclass.closed_scheme"

let vals_remove lab vals =
  Vars.fold (fun l k v -> if lab = l then v else Vars.add l k v)
    vals Vars.empty

let insert_value env lab priv mut ty loc vals =
  begin try
    let (mut', ty') = Vars.find lab vals in
    check_mutable loc lab mut mut';
    try Ctype.unify env ty ty' with Ctype.Unify ->
      raise(Error(loc, Variable_type_mismatch(lab, ty, ty')))
  with Not_found -> () end;
  if priv = Private then
    vals_remove lab vals
  else
    Vars.add lab (mut, ty) vals

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
           try Ctype.unify var_env ty ty' with Ctype.Unify ->
             raise(Error(sty.ptyp_loc, Parameter_mismatch(ty', ty))))
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
                Ctype.unify var_env self
                  (Ctype.newobj (closed_scheme fi))
              with Ctype.Unify ->
                let lab = missing_method var_env self' self in
                raise(Error(loc, Closed_ancestor
                                   (cl.pcl_name, path, lab)))
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
              List.map
                (fun lab ->
                   Ctype.unify met_env (Ctype.filter_method met_env lab ty)
                                       (Ctype.filter_method met_env lab self);
                   (lab, Ident.create lab))
                cl_type.cty_concr
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
      let ty' = Ctype.filter_method met_env lab self in
      begin try Ctype.unify met_env ty ty' with Ctype.Unify ->
        raise(Error(loc, Method_type_mismatch (lab, ty, ty')))
      end;
      (met_env, fields, vars_sig)

  | Pcf_meth (lab, expr, loc)  ->
      let (texp, ty) = type_method met_env self cl.pcl_self expr in
      let ty' = Ctype.filter_method met_env lab self in
        begin try Ctype.unify met_env ty ty' with Ctype.Unify ->
          raise(Error(loc, Method_type_mismatch (lab, ty, ty')))
        end;
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
    try List.map enter_type_variable cl.pcl_param with Already_bound ->
      raise(Error(cl.pcl_loc, Repeated_parameter)) in

  (* Bind self type variable *)
  begin try
    match cl.pcl_self_ty with
      Some v -> Ctype.unify temp_env self (enter_type_variable v)
    | None   -> ()
  with Already_bound ->
    raise(Error(cl.pcl_loc, Repeated_parameter))
  end;

  (* Add constraints *)
  List.iter
    (function (v, ty, loc) ->
       try
         Ctype.unify temp_env
           (type_variable loc v) (transl_simple_type temp_env false ty)
       with Ctype.Unify ->
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
  begin try Ctype.unify temp_env temp_cl cl_ty with Ctype.Unify ->
    Ctype.remove_object_name temp_cl;
    raise(Error(cl.pcl_loc, Abbrev_type_clash (cl_abbrev, cl_ty, temp_cl)))
  end;
  begin try
    List.iter2 (Ctype.unify temp_env) temp_cl_params cl_params
  with Ctype.Unify ->
    raise(Error(cl.pcl_loc,
      	  Bad_parameters (cl_id, cl_abbrev,
      	                  Ctype.newty (Tconstr (Path.Pident cl_id, cl_params,
      	       	       	       	       	        ref [])))))
  end;

  (* Object abbreviation and arguments for new *)
  let (obj_params, arg_sig', obj_ty) =
    Ctype.instance_parameterized_type_2 params arg_sig self
  in
  begin try Ctype.unify temp_env abbrev obj_ty with Ctype.Unify ->
    raise(Error(cl.pcl_loc, Abbrev_type_clash (abbrev, obj_ty, temp_obj)))
  end;
  begin try
    List.iter2 (Ctype.unify temp_env) temp_obj_params obj_params
  with Ctype.Unify ->
    raise(Error(cl.pcl_loc,
          Bad_parameters (obj_id, abbrev,
       	       	          Ctype.newty (Tconstr (Path.Pident obj_id, obj_params,
      	       	       	       	       	        ref [])))))
  end;
  Ctype.close_object temp_obj;
  List.iter2
    (fun ty (exp, ty') ->
       begin try
	 Ctype.unify temp_env ty ty'
       with Ctype.Unify ->
	 raise(Error(exp.pat_loc, Argument_type_mismatch(ty', ty)))
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
  ((cl, id, cl_id, obj_id, cl_sig, cl_imp,
    concr, abbrev, new_args, temp_obj, temp_obj_params),
   new_env)

let build_new_type temp_env env
     (cl, id, cl_id, obj_id, cl_sig, cl_imp,
      concr, abbrev, new_args, temp_obj, temp_obj_params)
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
    with Ctype.Unify ->
    	let lab = missing_method temp_env concr temp_obj in
      raise(Error(cl.pcl_loc,
    	       	    Virtual_class (cl.pcl_name, lab)))
  end;

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

let generalize_class (_, _, _, _, cl_sig, _) =
  List.iter Ctype.generalize cl_sig.cty_params;
  List.iter Ctype.generalize cl_sig.cty_args;
  Vars.iter (fun l (m, t) -> Ctype.generalize t) cl_sig.cty_vars;
  Ctype.generalize cl_sig.cty_self;
  match cl_sig.cty_new with Some ty -> Ctype.generalize ty | None -> ()

let make_abbrev env
  (cl, id, cl_id, obj_id, cl_sig, cl_imp)
  =
  (* Class type abbreviation *)
  Ctype.begin_def ();
  let (params, self) =
    Ctype.instance_parameterized_type cl_sig.cty_params cl_sig.cty_self
  in let (cl_ty, cstr) = Ctype.prune self params
  in let cl_ty_params = List.map fst cstr in
  Ctype.end_def ();
  Ctype.generalize cl_ty;
  List.iter Ctype.generalize cl_ty_params;
  let cl_abbrev =
    { type_params = cl_ty_params;
      type_arity = List.length cl_ty_params;
      type_kind = Type_abstract;
      type_manifest = Some
        (if cl.pcl_closed = Closed then
	  Ctype.newgenty (Tconstr(Path.Pident obj_id, cl_ty_params, ref []))
        else begin
          Ctype.set_object_name cl_ty cl_ty_params obj_id;
	  cl_ty
	end) }
  in let new_env = Env.add_type cl_id cl_abbrev env in

  (* Object type abbreviation *)
  Ctype.begin_def ();
  let (obj_ty_params, obj_ty) =
    Ctype.instance_parameterized_type cl_ty_params cl_ty
  in
  Ctype.close_object obj_ty;
  Ctype.end_def ();
  List.iter Ctype.generalize obj_ty_params;
  if not (Ctype.closed_schema obj_ty) then
    raise(Error(cl.pcl_loc,
                Non_closed(obj_id, obj_ty_params, obj_ty)));
  Ctype.generalize obj_ty;
  let obj_abbrev =
    { type_params = obj_ty_params;
      type_arity = List.length obj_ty_params;
      type_kind = Type_abstract;
      type_manifest = Some (Ctype.unroll_abbrev obj_id obj_ty_params obj_ty) }
  in let new_env = Env.add_type obj_id obj_abbrev new_env in

  ((id, cl_sig, cl_id, cl_abbrev, obj_id, obj_abbrev, cl_imp), new_env)

let rec iter f env =
  function
    [] ->
      ([], env)
  | cl :: cl_rem ->
      let (cl', env') = f env cl in
      let (cl_rem', env'') = iter f env' cl_rem in
      (cl'::cl_rem', env'')

let transl_classes env cl =
  Ctype.begin_def ();
  let (info, temp_env) = iter make_stub env cl in
  let (info, _) = iter (transl_class temp_env) env info in
  let (info, new_env) = iter (build_new_type temp_env) env info in
  Ctype.end_def ();
  List.iter generalize_class info;
  let (info, new_env) = iter make_abbrev new_env info in
  (info, new_env)


(*----------------------------------------------------------------------*)

(* Represent object types as abstract types *)
let enter_class env cl =
  let abstr_type =
    { type_params = [];
      type_arity = List.length cl.pcty_param;
      type_kind = Type_abstract;
      type_manifest = None }
  in
  let (obj_id, ext_env) = Env.enter_type cl.pcty_name abstr_type env in
  ((cl, obj_id), ext_env)

let insert_value env lab priv mut ty loc val_sig val_redef =
  let val_sig' =
    if priv = Private then
      vals_remove lab val_sig
    else
      Vars.add lab (mut, ty) val_sig
  in
  try
    let (mut', ty') = Vars.find lab val_sig in
    check_mutable loc lab mut mut';
    (val_sig', (lab, ty', ty, loc)::val_redef)
  with Not_found ->
    (val_sig', val_redef)

let change_value_status lab priv mut loc val_sig =
  try
    let (mut', ty') = Vars.find lab val_sig in
    check_mutable loc lab mut mut';
    if priv = Private then
      vals_remove lab val_sig
    else
      Vars.add lab (mut, ty') val_sig
  with Not_found ->
    raise(Error(loc, Undefined_var lab))

let insert_meth env self lab ty loc meth_redef =
  let ty0 = Ctype.filter_method env lab self in
  if not
    (List.exists (function (lab', _, _, _) -> lab = lab') meth_redef)
  then
    Ctype.unify env ty ty0;
  (lab, ty0, ty, loc)::meth_redef

let rec type_meth env loc self methods ty =
  match (Ctype.repr ty).desc with
    Tfield (lab, ty, ty') ->
      insert_meth env self lab ty loc (type_meth env loc self methods ty')
  | _ ->
      methods

let type_class_type_field env temp_env cl self
    (val_sig, val_redef, meth_redef) =
  function
    Pctf_inher (name, params, loc) ->
      let (path, cl_sig) =
        try Env.lookup_class name env with Not_found ->
          raise (Error (loc, Unbound_class name))
      in
      let (cstr, args, vals, super) = Ctype.prune_class_type cl_sig in
      List.iter2
        (fun t1 (t2, _) ->
           Ctype.unify temp_env
             (Typetexp.transl_simple_type temp_env false t1) t2)
        params cstr;
      let super = Ctype.expand_root temp_env super in
      let (val_sig, val_redef) =
        Vars.fold
          (fun lab (mut, ty) (v_sig, v_redef) ->
             insert_value temp_env lab Public mut ty loc v_sig v_redef)
          vals
          (val_sig, val_redef)
      in
      let meth_redef =
        match super.desc with
	  Tobject (fi, _) ->
	    if not (Ctype.opened_object super) then
	      begin try
		Ctype.unify temp_env self (Ctype.newobj (closed_scheme fi))
	      with Ctype.Unify ->
		let lab = missing_method temp_env super self in
		raise(Error(loc,
			    Closed_ancestor (cl.pcty_name, path, lab)))
	      end;
	    super.desc <- Tlink self;
	    type_meth temp_env loc self meth_redef fi
	| _ ->
	    fatal_error "Typeclass.type_class_type_field"
      in
      (val_sig, val_redef, meth_redef)

  | Pctf_val (lab, priv, mut, sty, loc) ->
      begin match sty with
      	Some sty ->
          let ty = transl_simple_type temp_env false sty in
          let (val_sig, val_redef) =
            insert_value temp_env lab priv mut ty loc val_sig val_redef
          in
          (val_sig, val_redef, meth_redef)
      | None ->
      	  (change_value_status lab priv mut loc val_sig, val_redef, meth_redef)
      end

  | Pctf_virt (lab, sty, loc) ->
      let ty = transl_simple_type temp_env false sty in
      (val_sig, val_redef, insert_meth temp_env self lab ty loc meth_redef)

  | Pctf_meth (lab, sty, loc) ->
      let ty = transl_simple_type temp_env false sty in
      (val_sig, val_redef, insert_meth temp_env self lab ty loc meth_redef)

(* Build class and object types *)
let build_abbrevs temp_env env (cl, obj_id) =
  reset_type_variables ();
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
	           Sort.merge (<) meths anc.cty_concr
               | _ -> fatal_error "Typeclass.make_stub"
               end
	 | Pctf_val _ ->
	     meths
	 | Pctf_virt (lab, _, _) ->
	     Ctype.filter_method env lab self;
	     meths
	 | Pctf_meth (lab, _, _) ->
	     Ctype.filter_method env lab self;
             Ctype.filter_method env lab concr;
	     Sort.merge (<) meths [lab])
      [] cl.pcty_field
  in
  Ctype.close_object concr;

  (* Introduce parameters *)
  let params =
    try List.map enter_type_variable cl.pcty_param with Already_bound ->
      raise(Error(cl.pcty_loc, Repeated_parameter))
  in

  (* Bind self type variable *)
  begin try
    match cl.pcty_self with
      Some v -> Ctype.unify temp_env self (enter_type_variable v)
    | None   -> ()
  with Already_bound ->
    raise(Error(cl.pcty_loc, Repeated_parameter))
  end;

  (* Translate argument types *)
  let args = List.map (transl_simple_type temp_env false) cl.pcty_args in

  (* Translate fields *)
  let (val_sig, val_redef, meth_redef) =
    List.fold_left (type_class_type_field env temp_env cl self)
      (Vars.empty, [], [])
      cl.pcty_field
  in

  (* Closeness of self *)
  if cl.pcty_closed = Closed then
    Ctype.close_object self;
  if (cl.pcty_closed <> Closed) & not (Ctype.opened_object self) then
    raise(Error(cl.pcty_loc, Closed_class cl.pcty_name));

  (* Generalize class *)
  Ctype.end_def ();
  List.iter Ctype.generalize params;
  List.iter Ctype.generalize args;
  Vars.iter (fun _ (_, ty) -> Ctype.generalize ty) val_sig;
  Ctype.generalize self;
  Ctype.generalize concr;
  List.iter
    (function (_, ty, ty', _) -> Ctype.generalize ty; Ctype.generalize ty')
    val_redef;
  List.iter
    (function (_, ty, ty', _) -> Ctype.generalize ty; Ctype.generalize ty')
    meth_redef;

  (* Object type abbreviation *)
  Ctype.begin_def ();
  let (obj_params, obj_ty) = Ctype.instance_parameterized_type params self in
  Ctype.close_object obj_ty;
  Ctype.end_def ();
  List.iter Ctype.generalize obj_params;
  if not (Ctype.closed_schema obj_ty) then
    raise(Error(cl.pcty_loc,
                Non_closed(obj_id, obj_params, obj_ty)));
  Ctype.generalize obj_ty;
  let obj_abbrev =
    { type_params = obj_params;
      type_arity = List.length obj_params;
      type_kind = Type_abstract;
      type_manifest = Some obj_ty }
  in let ext_env = Env.add_type obj_id obj_abbrev env in

  (* Class type abbreviation *)
  let cl_abbrev =
    { type_params = List.map Ctype.repr params;
      type_arity = List.length params;
      type_kind = Type_abstract;
      type_manifest = Some
        (if cl.pcty_closed = Closed then
	  Ctype.newgenty (Tconstr(Path.Pident obj_id, params, ref []))
        else begin
          Ctype.set_object_name self params obj_id;
	  self
	end) }
  in
  let (cl_id, ext_env) =
    Env.enter_type ("#" ^ cl.pcty_name) cl_abbrev ext_env
  in

  (* Set class type name *)

  (* Temporary class signature *)
  let temp_cl_sig =
    { cty_params = params;
      cty_args = args;
      cty_vars = val_sig;
      cty_self = self;
      cty_concr = concr_meths;
      cty_new = None }
  in

  ((cl, cl_id, cl_abbrev, obj_id, obj_abbrev, obj_params, obj_ty, temp_cl_sig,
    val_redef, meth_redef, concr),
   ext_env)

let check_abbrev env
    (cl, cl_id, cl_abbrev, obj_id, obj_abbrev, obj_params, obj_ty, temp_cl_sig,
     val_redef, meth_redef, concr) =
  begin try Ctype.correct_abbrev env obj_id obj_params obj_ty with
    Ctype.Nonlinear_abbrev ->
      raise(Error(cl.pcty_loc, Illdefined_class cl.pcty_name))
  end;
  env

let check_field_redef env error (lab, ty, ty', loc) =
  if not (Ctype.equal env [] ty [] ty') then
    raise(Error(loc, error lab ty ty'))

let build_class_type env
    (cl, cl_id, cl_abbrev, obj_id, obj_abbrev, obj_params, obj_ty, temp_cl_sig,
     val_redef, meth_redef, concr) =

  (* Check variable and method redefining *)
  List.iter
    (check_field_redef env (fun l t t' -> Variable_type_mismatch(l, t', t)))
    val_redef;
  List.iter
    (check_field_redef env (fun l t t' -> Method_type_mismatch(l, t', t)))
    meth_redef;

  (* Class type skeleton *)
  reset_type_variables ();
  Ctype.begin_def ();
  let (params, args, vals, self) = Ctype.instance_class temp_cl_sig in

  (* Re-introduce parameters and bind self type variable *)
  List.iter2
    (fun v ty -> Ctype.unify env (enter_type_variable v) ty)
    cl.pcty_param params;
  begin match cl.pcty_self with
    Some v -> Ctype.unify env (enter_type_variable v) self
  | None   -> ()
  end;

  (* Add constraints *)
  List.iter
    (function (v, ty, loc) ->
       try
         Ctype.unify env
           (type_variable loc v) (transl_simple_type env false ty)
       with Ctype.Unify ->
       	 raise(Error(loc, Unconsistent_constraint)))
    cl.pcty_cstr;

  Ctype.end_def ();
  List.iter Ctype.generalize params;
  List.iter Ctype.generalize args;
  Vars.iter (fun l (m, t) -> Ctype.generalize t) vals;
  Ctype.generalize self;

  (* Class type *)
  let cl_sig =
    { cty_params = params;
      cty_args = args;
      cty_vars = vals;
      cty_self = self;
      cty_concr = temp_cl_sig.cty_concr;
      cty_new = None }
  in let (id, new_env) = Env.enter_class cl.pcty_name cl_sig env in

  (* Build type of [new cl] *)
  cl_sig.cty_new <-
    if cl.pcty_kind = Concrete then begin
      let concr = Ctype.instance concr in
      let temp_obj = Ctype.instance obj_ty in
      begin try
        Ctype.unify env concr temp_obj
      with Ctype.Unify ->
      	let lab = missing_method env concr temp_obj in
	raise(Error(cl.pcty_loc,
      	       	    Virtual_class (cl.pcty_name, lab)))
      end;
      Ctype.begin_def ();
      let (params, args, _, self) = Ctype.instance_class cl_sig in
      let abbrev =
        Ctype.newty (Tconstr (Path.Pident obj_id, params, ref []))
      in
      Ctype.unify env self abbrev;
      let ty_new =
        List.fold_right (fun arg ty -> Ctype.newty (Tarrow(arg, ty)))
      	  args abbrev
      in
      Ctype.end_def ();
      Ctype.generalize ty_new;
      Some ty_new
    end else
      None;

  ((id, cl_sig, cl_id, cl_abbrev, obj_id, obj_abbrev), new_env)

let rec transl_class_types env class_type_list =
  (* Type classes are first abstract *)
  let (info, temp_env) = iter enter_class env class_type_list in
  let (info, new_env) = iter (build_abbrevs temp_env) env info in
  List.iter (check_abbrev new_env) info;
  let (info, final_env) = iter build_class_type new_env info in
  (info, final_env)

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
      print_string "should be virtual: its methods"; print_space ();
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
  | Non_closed (id, args, typ) ->
      open_hovbox 0;
      Printtyp.reset ();
      Printtyp.mark_loops typ;
      print_string
        "Some type variables are not bound in implicit type definition";
      print_space ();
      open_hovbox 0;
      Printtyp.type_expr (Ctype.newty (Tconstr(Path.Pident id, args, ref [])));
      print_space (); print_string "="; print_space ();
      Printtyp.type_expr typ;
      close_box ();
      close_box ()
  | Mutable_var v ->
      print_string "The variable"; print_space ();
      print_string v; print_space ();
      print_string "was mutable and is redefined as immutable"
  | Undefined_var v ->
      print_string "The variable"; print_space ();
      print_string v; print_space ();
      print_string "is undefined"
  | Variable_type_mismatch (v, actual, expected) ->
      open_hovbox 0;
      Printtyp.reset ();
      Printtyp.mark_loops actual; Printtyp.mark_loops expected;
      print_string "The variable ";
      print_string v; print_space ();
      print_string "has type"; print_space ();
      Printtyp.type_expr actual;
      print_space ();
      print_string "but is expected to have type"; print_space ();
      Printtyp.type_expr expected;
      close_box ()
  | Method_type_mismatch (m, actual, expected) ->
      open_hovbox 0;
      Printtyp.reset ();
      Printtyp.mark_loops actual; Printtyp.mark_loops expected;
      print_string "The method ";
      print_string m; print_space ();
      print_string "has type"; print_space ();
      Printtyp.type_expr actual;
      print_space ();
      print_string "but is expected to have type"; print_space ();
      Printtyp.type_expr expected;
      close_box ()
  | Unconsistent_constraint ->
      print_string "The class constraints are not consistent"
  | Unbound_class cl ->
      print_string "Unbound class"; print_space ();
      Printtyp.longident cl
  | Argument_type_mismatch (actual, expected) ->
      open_hovbox 0;
      Printtyp.reset ();
      Printtyp.mark_loops actual; Printtyp.mark_loops expected;
      print_string "This argument has type"; print_space ();
      Printtyp.type_expr actual;
      print_space ();
      print_string "but is expected to have type"; print_space ();
      Printtyp.type_expr expected;
      close_box ()
  | Abbrev_type_clash (abbrev, actual, expected) ->
      open_hovbox 0;
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
      open_hovbox 0;
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
  | Parameter_mismatch(actual, expected) ->
      Printtyp.reset ();
      Printtyp.mark_loops actual; Printtyp.mark_loops expected;
      open_hovbox 0;
      print_string "The type parameter"; print_space ();
      Printtyp.type_expr actual; print_space ();
      print_string "does not meet its constraint: it should be";
      print_space ();
      Printtyp.type_expr expected
  | Argument_arity_mismatch(p, expected, provided) ->
      open_hovbox 0;
      print_string "The class "; Printtyp.path p;
      print_space(); print_string "expects "; print_int expected;
      print_string " argument(s),"; print_space();
      print_string "but is here applied to "; print_int provided;
      print_string " argument(s)";
      close_box()
  | Parameter_arity_mismatch(p, expected, provided) ->
      open_hovbox 0;
      print_string "The class "; Printtyp.path p;
      print_space(); print_string "expects "; print_int expected;
      print_string " parameter(s),"; print_space();
      print_string "but is here applied to "; print_int provided;
      print_string " parameter(s)";
      close_box()
