(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Typechecking for the core language *)

open Misc
open Asttypes
open Parsetree
open Types
open Typedtree
open Btype
open Ctype


type error =
    Unbound_value of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Constructor_arity_mismatch of Longident.t * int * int
  | Label_mismatch of Longident.t * (type_expr * type_expr) list
  | Pattern_type_clash of (type_expr * type_expr) list
  | Multiply_bound_variable
  | Orpat_not_closed
  | Expr_type_clash of (type_expr * type_expr) list
  | Apply_non_function of type_expr
  | Label_multiply_defined of Longident.t
  | Label_missing
  | Label_not_mutable of Longident.t
  | Bad_format of string
  | Undefined_method_err of string
  | Unbound_class of Longident.t
  | Virtual_class of Longident.t
  | Unbound_instance_variable of string
  | Instance_variable_not_mutable of string
  | Not_subtype of (type_expr * type_expr) list * (type_expr * type_expr) list
  | Outside_class
  | Value_multiply_overridden of string
  | Coercion_failure of type_expr * type_expr * (type_expr * type_expr) list
  | Too_many_arguments

exception Error of Location.t * error

(* Typing of constants *)

let type_constant = function
    Const_int _ -> instance Predef.type_int
  | Const_char _ -> instance Predef.type_char
  | Const_string _ -> instance Predef.type_string
  | Const_float _ -> instance Predef.type_float

(* Typing of patterns *)

let unify_pat env pat expected_ty =
  try
    unify env pat.pat_type expected_ty
  with Unify trace ->
    raise(Error(pat.pat_loc, Pattern_type_clash(trace)))

let pattern_variables = ref ([]: (Ident.t * type_expr) list)

let enter_variable loc name ty =
  if List.exists (fun (id, ty) -> Ident.name id = name) !pattern_variables
  then raise(Error(loc, Multiply_bound_variable));
  let id = Ident.create name in
  pattern_variables := (id, ty) :: !pattern_variables;
  id

let rec type_pat env sp =
  match sp.ppat_desc with
    Ppat_any ->
      { pat_desc = Tpat_any;
        pat_loc = sp.ppat_loc;
        pat_type = newvar() }
  | Ppat_var name ->
      let ty = newvar() in
      let id = enter_variable sp.ppat_loc name ty in
      { pat_desc = Tpat_var id;
        pat_loc = sp.ppat_loc;
        pat_type = ty }
  | Ppat_alias(sp, name) ->
      let p = type_pat env sp in
      let id = enter_variable sp.ppat_loc name p.pat_type in
      { pat_desc = Tpat_alias(p, id);
        pat_loc = sp.ppat_loc;
        pat_type = p.pat_type }
  | Ppat_constant cst ->
      { pat_desc = Tpat_constant cst;
        pat_loc = sp.ppat_loc;
        pat_type = type_constant cst }
  | Ppat_tuple spl ->
      let pl = List.map (type_pat env) spl in
      { pat_desc = Tpat_tuple pl;
        pat_loc = sp.ppat_loc;
        pat_type = newty (Ttuple(List.map (fun p -> p.pat_type) pl)) }
  | Ppat_construct(lid, sarg) ->
      let constr =
        try
          Env.lookup_constructor lid env
        with Not_found ->
          raise(Error(sp.ppat_loc, Unbound_constructor lid)) in
      let sargs =
        match sarg with
          None -> []
        | Some {ppat_desc = Ppat_tuple spl} when constr.cstr_arity > 1 -> spl
        | Some({ppat_desc = Ppat_any} as sp) when constr.cstr_arity > 1 ->
            replicate_list sp constr.cstr_arity
        | Some sp -> [sp] in
      if List.length sargs <> constr.cstr_arity then
        raise(Error(sp.ppat_loc, Constructor_arity_mismatch(lid,
                                     constr.cstr_arity, List.length sargs)));
      let args = List.map (type_pat env) sargs in
      let (ty_args, ty_res) = instance_constructor constr in
      List.iter2 (unify_pat env) args ty_args;
      { pat_desc = Tpat_construct(constr, args);
        pat_loc = sp.ppat_loc;
        pat_type = ty_res }
  | Ppat_record lid_sp_list ->
      let ty = newvar() in
      let type_label_pat (lid, sarg) =
        let label =
          try
            Env.lookup_label lid env
          with Not_found ->
            raise(Error(sp.ppat_loc, Unbound_label lid)) in
        let (ty_arg, ty_res) = instance_label label in
        begin try
          unify env ty_res ty
        with Unify trace ->
          raise(Error(sp.ppat_loc, Label_mismatch(lid, trace)))
        end;
        let arg = type_pat env sarg in
        unify_pat env arg ty_arg;
        (label, arg)
      in
      { pat_desc = Tpat_record(List.map type_label_pat lid_sp_list);
        pat_loc = sp.ppat_loc;
        pat_type = ty }
  | Ppat_or(sp1, sp2) ->
      let initial_pattern_variables = !pattern_variables in
      let p1 = type_pat env sp1 in
      let p2 = type_pat env sp2 in
      if !pattern_variables != initial_pattern_variables then
        raise(Error(sp.ppat_loc, Orpat_not_closed));
      unify_pat env p2 p1.pat_type;
      { pat_desc = Tpat_or(p1, p2);
        pat_loc = sp.ppat_loc;
        pat_type = p1.pat_type }
  | Ppat_constraint(sp, sty) ->
      let p = type_pat env sp in
      let ty = Typetexp.transl_simple_type env false sty in
      unify_pat env p ty;
      p

let add_pattern_variables env =
  let pv = !pattern_variables in
  pattern_variables := [];
  List.fold_right
    (fun (id, ty) env ->
      Env.add_value id {val_type = ty; val_kind = Val_reg} env)
    pv env

let type_pattern env spat =
  pattern_variables := [];
  let pat = type_pat env spat in
  let new_env = add_pattern_variables env in
  (pat, new_env)

let type_pattern_list env spatl =
  pattern_variables := [];
  let patl = List.map (type_pat env) spatl in
  let new_env = add_pattern_variables env in
  (patl, new_env)

let rec iter_pattern f p =
  f p;
  match p.pat_desc with
    Tpat_any | Tpat_var _ | Tpat_constant _ ->
      ()
  | Tpat_alias (p, _) ->
      iter_pattern f p
  | Tpat_tuple pl ->
      List.iter (iter_pattern f) pl
  | Tpat_construct (_, pl) ->
      List.iter (iter_pattern f) pl
  | Tpat_record fl ->
      List.iter (fun (_, p) -> iter_pattern f p) fl
  | Tpat_or (p, p') ->
      iter_pattern f p;
      iter_pattern f p'

(* Generalization criterion for expressions *)

let rec is_nonexpansive exp =
  match exp.exp_desc with
    Texp_ident(_,_) -> true
  | Texp_constant _ -> true
  | Texp_let(rec_flag, pat_exp_list, body) ->
      List.for_all (fun (pat, exp) -> is_nonexpansive exp) pat_exp_list &
      is_nonexpansive body
  | Texp_function _ -> true
  | Texp_tuple el ->
      List.for_all is_nonexpansive el
  | Texp_construct(_, el) ->
      List.for_all is_nonexpansive el
  | Texp_record lbl_exp_list ->
      List.for_all
        (fun (lbl, exp) -> lbl.lbl_mut = Immutable & is_nonexpansive exp)
        lbl_exp_list
  | Texp_field(exp, lbl) -> is_nonexpansive exp
  | Texp_array [] -> true
  | Texp_new _ -> true
  | _ -> false

(* Typing of printf formats *)

let type_format loc fmt =
  let len = String.length fmt in
  let ty_input = newvar()
  and ty_result = newvar() in
  let rec skip_args j =
    if j >= len then j else
      match fmt.[j] with
        '0' .. '9' | ' ' | '.' | '-' -> skip_args (j+1)
      | _ -> j in
  let rec scan_format i =
    if i >= len then ty_result else
    match fmt.[i] with
      '%' ->
        let j = skip_args(i+1) in
        begin match String.unsafe_get fmt j with
        (* We're using unsafe_get here so that if j = String.length fmt,
           we'll fall in the catch-all case of the match *)
          '%' ->
            scan_format (j+1)
        | 's' ->
            newty (Tarrow(instance Predef.type_string, scan_format (j+1)))
        | 'c' ->
            newty (Tarrow(instance Predef.type_char, scan_format (j+1)))
        | 'd' | 'o' | 'x' | 'X' | 'u' ->
            newty (Tarrow(instance Predef.type_int, scan_format (j+1)))
        | 'f' | 'e' | 'E' | 'g' | 'G' ->
            newty (Tarrow(instance Predef.type_float, scan_format (j+1)))
        | 'b' ->
            newty (Tarrow(instance Predef.type_bool, scan_format (j+1)))
        | 'a' ->
            let ty_arg = newvar() in
            newty (Tarrow (newty (Tarrow(ty_input,
      	       	       	       	       	 newty (Tarrow (ty_arg, ty_result)))),
                           newty (Tarrow (ty_arg, scan_format (j+1)))))
        | 't' ->
            newty (Tarrow(newty (Tarrow(ty_input, ty_result)),
      	       	       	  scan_format (j+1)))
        | c ->
            raise(Error(loc, Bad_format(String.sub fmt i (j-i))))
        end
    | _ -> scan_format (i+1) in
  newty
    (Tconstr(Predef.path_format, [scan_format 0; ty_input; ty_result], ref Mnil))

(* Typing of expressions *)

let unify_exp env exp expected_ty =
  try
    unify env exp.exp_type expected_ty
  with Unify trace ->
    raise(Error(exp.exp_loc, Expr_type_clash(trace)))

let rec type_exp env sexp =
  match sexp.pexp_desc with
    Pexp_ident lid ->
      begin try
        let (path, desc) = Env.lookup_value lid env in
        { exp_desc =
            begin match (desc.val_kind, lid) with
	      (Val_ivar _, Longident.Lident lab) ->
      	        let (path_self, _) =
                  Env.lookup_value (Longident.Lident "*self*") env
                in
                Texp_instvar (path_self, path)
	    | _ ->
	        Texp_ident(path, desc)
      	    end;
          exp_loc = sexp.pexp_loc;
          exp_type = instance desc.val_type;
          exp_env = env }
      with Not_found ->
        raise(Error(sexp.pexp_loc, Unbound_value lid))
      end
  | Pexp_constant cst ->
      { exp_desc = Texp_constant cst;
        exp_loc = sexp.pexp_loc;
        exp_type = type_constant cst;
        exp_env = env }
  | Pexp_let(rec_flag, spat_sexp_list, sbody) ->
      let (pat_exp_list, new_env) = type_let env rec_flag spat_sexp_list in
      let body = type_exp new_env sbody in
      { exp_desc = Texp_let(rec_flag, pat_exp_list, body);
        exp_loc = sexp.pexp_loc;
        exp_type = body.exp_type;
        exp_env = env }
  | Pexp_function caselist ->
      let ty_arg = newvar() and ty_res = newvar() in
      let cases = type_cases env ty_arg ty_res caselist in
      Parmatch.check_unused cases;
      Parmatch.check_partial sexp.pexp_loc cases;
      { exp_desc = Texp_function cases;
        exp_loc = sexp.pexp_loc;
        exp_type = newty (Tarrow(ty_arg, ty_res));
        exp_env = env }
  | Pexp_apply(sfunct, sargs) ->
      let funct = type_exp env sfunct in
      let rec type_args ty_fun = function
        [] ->
          ([], ty_fun)
      | sarg1 :: sargl ->
          let (ty1, ty2) =
            try
              filter_arrow env ty_fun
            with Unify _ ->
              raise(Error(sfunct.pexp_loc,
                          Apply_non_function funct.exp_type)) in
          let arg1 = type_expect env sarg1 ty1 in
          let (argl, ty_res) = type_args ty2 sargl in
          (arg1 :: argl, ty_res) in
      let (args, ty_res) = type_args funct.exp_type sargs in
      { exp_desc = Texp_apply(funct, args);
        exp_loc = sexp.pexp_loc;
        exp_type = ty_res;
        exp_env = env }
  | Pexp_match(sarg, caselist) ->
      let arg = type_exp env sarg in
      let ty_res = newvar() in
      let cases = type_cases env arg.exp_type ty_res caselist in
      Parmatch.check_unused cases;
      Parmatch.check_partial sexp.pexp_loc cases;
      { exp_desc = Texp_match(arg, cases);
        exp_loc = sexp.pexp_loc;
        exp_type = ty_res;
        exp_env = env }
  | Pexp_try(sbody, caselist) ->
      let body = type_exp env sbody in
      let cases =
        type_cases env (instance Predef.type_exn) body.exp_type caselist in
      Parmatch.check_unused cases;
      { exp_desc = Texp_try(body, cases);
        exp_loc = sexp.pexp_loc;
        exp_type = body.exp_type;
        exp_env = env }
  | Pexp_tuple sexpl ->
      let expl = List.map (type_exp env) sexpl in
      { exp_desc = Texp_tuple expl;
        exp_loc = sexp.pexp_loc;
        exp_type = newty (Ttuple(List.map (fun exp -> exp.exp_type) expl));
        exp_env = env }
  | Pexp_construct(lid, sarg) ->
      let constr =
        try
          Env.lookup_constructor lid env
        with Not_found ->
          raise(Error(sexp.pexp_loc, Unbound_constructor lid)) in
      let sargs =
        match sarg with
          None -> []
        | Some {pexp_desc = Pexp_tuple sel} when constr.cstr_arity > 1 -> sel
        | Some se -> [se] in
      if List.length sargs <> constr.cstr_arity then
        raise(Error(sexp.pexp_loc, Constructor_arity_mismatch(lid,
                                       constr.cstr_arity, List.length sargs)));
      let (ty_args, ty_res) = instance_constructor constr in
      let args = List.map2 (type_expect env) sargs ty_args in
      { exp_desc = Texp_construct(constr, args);
        exp_loc = sexp.pexp_loc;
        exp_type = ty_res;
        exp_env = env }
  | Pexp_record lid_sexp_list ->
      let ty = newvar() in
      let num_fields = ref 0 in
      let type_label_exp (lid, sarg) =
        let label =
          try
            Env.lookup_label lid env
          with Not_found ->
            raise(Error(sexp.pexp_loc, Unbound_label lid)) in
        let (ty_arg, ty_res) = instance_label label in
        begin try
          unify env ty_res ty
        with Unify trace ->
          raise(Error(sexp.pexp_loc, Label_mismatch(lid, trace)))
        end;
        let arg = type_expect env sarg ty_arg in
        num_fields := Array.length label.lbl_all;
        (label, arg) in
      let lbl_exp_list = List.map type_label_exp lid_sexp_list in
      let rec check_duplicates = function
        [] -> ()
      | (lid, sarg) :: remainder ->
          if List.mem_assoc lid remainder
          then raise(Error(sexp.pexp_loc, Label_multiply_defined lid))
          else check_duplicates remainder in
      check_duplicates lid_sexp_list;
      if List.length lid_sexp_list <> !num_fields then
        raise(Error(sexp.pexp_loc, Label_missing));
      { exp_desc = Texp_record lbl_exp_list;
        exp_loc = sexp.pexp_loc;
        exp_type = ty;
        exp_env = env }
  | Pexp_field(sarg, lid) ->
      let arg = type_exp env sarg in
      let label =
        try
          Env.lookup_label lid env
        with Not_found ->
          raise(Error(sexp.pexp_loc, Unbound_label lid)) in
      let (ty_arg, ty_res) = instance_label label in
      unify_exp env arg ty_res;
      { exp_desc = Texp_field(arg, label);
        exp_loc = sexp.pexp_loc;
        exp_type = ty_arg;
        exp_env = env }
  | Pexp_setfield(srecord, lid, snewval) ->
      let record = type_exp env srecord in
      let label =
        try
          Env.lookup_label lid env
        with Not_found ->
          raise(Error(sexp.pexp_loc, Unbound_label lid)) in
      if label.lbl_mut = Immutable then
        raise(Error(sexp.pexp_loc, Label_not_mutable lid));
      let (ty_arg, ty_res) = instance_label label in
      unify_exp env record ty_res;
      let newval = type_expect env snewval ty_arg in
      { exp_desc = Texp_setfield(record, label, newval);
        exp_loc = sexp.pexp_loc;
        exp_type = instance Predef.type_unit;
        exp_env = env }
  | Pexp_array(sargl) ->
      let ty = newvar() in
      let argl = List.map (fun sarg -> type_expect env sarg ty) sargl in
      { exp_desc = Texp_array argl;
        exp_loc = sexp.pexp_loc;
        exp_type = instance (Predef.type_array ty);
        exp_env = env }
  | Pexp_ifthenelse(scond, sifso, sifnot) ->
      let cond = type_expect env scond (instance Predef.type_bool) in
      begin match sifnot with
        None ->
          let ifso = type_expect env sifso (instance Predef.type_unit) in
          { exp_desc = Texp_ifthenelse(cond, ifso, None);
            exp_loc = sexp.pexp_loc;
            exp_type = instance Predef.type_unit;
            exp_env = env }
      | Some sifnot ->
          let ifso = type_exp env sifso in
          let ifnot = type_expect env sifnot ifso.exp_type in
          { exp_desc = Texp_ifthenelse(cond, ifso, Some ifnot);
            exp_loc = sexp.pexp_loc;
            exp_type = ifso.exp_type;
            exp_env = env }
      end
  | Pexp_sequence(sexp1, sexp2) ->
      let exp1 = type_statement env sexp1 in
      let exp2 = type_exp env sexp2 in
      { exp_desc = Texp_sequence(exp1, exp2);
        exp_loc = sexp.pexp_loc;
        exp_type = exp2.exp_type;
        exp_env = env }
  | Pexp_while(scond, sbody) ->
      let cond = type_expect env scond (instance Predef.type_bool) in
      let body = type_statement env sbody in
      { exp_desc = Texp_while(cond, body);
        exp_loc = sexp.pexp_loc;
        exp_type = instance Predef.type_unit;
        exp_env = env }
  | Pexp_for(param, slow, shigh, dir, sbody) ->
      let low = type_expect env slow (instance Predef.type_int) in
      let high = type_expect env shigh (instance Predef.type_int) in
      let (id, new_env) =
        Env.enter_value param {val_type = instance Predef.type_int;
                                val_kind = Val_reg} env in
      let body = type_statement new_env sbody in
      { exp_desc = Texp_for(id, low, high, dir, body);
        exp_loc = sexp.pexp_loc;
        exp_type = instance Predef.type_unit;
        exp_env = env }
  | Pexp_constraint(sarg, sty, sty') ->
      let (arg, ty') =
        match (sty, sty') with
      	  (None, None) ->               (* Case actually unused *)
            let arg = type_exp env sarg in
	    (arg, arg.exp_type)
	| (Some sty, None) ->
            let ty = Typetexp.transl_simple_type env false sty in
            (type_expect env sarg ty, ty)
	| (None, Some sty') ->
            let (ty', force) =
              Typetexp.transl_simple_type_delayed env sty'
            in
            let ty = enlarge_type env ty' in
            force ();
            let arg = type_exp env sarg in
            begin try Ctype.unify env arg.exp_type ty with Unify trace ->
              raise(Error(sarg.pexp_loc,
                    Coercion_failure(ty', full_expand env ty', trace)))
            end;
            (arg, ty')
	| (Some sty, Some sty') ->
            let (ty, force) =
              Typetexp.transl_simple_type_delayed env sty
            and (ty', force') =
              Typetexp.transl_simple_type_delayed env sty'
            in
            begin try
              let force'' = subtype env ty ty' in
              force (); force' (); force'' ()
            with Subtype (tr1, tr2) ->
	      raise(Error(sexp.pexp_loc, Not_subtype(tr1, tr2)))
            end;
	    (type_expect env sarg ty, ty')
      in
      { exp_desc = arg.exp_desc;
        exp_loc = arg.exp_loc;
        exp_type = ty';
        exp_env = env }
  | Pexp_when(scond, sbody) ->
      let cond = type_expect env scond (instance Predef.type_bool) in
      let body = type_exp env sbody in
      { exp_desc = Texp_when(cond, body);
        exp_loc = sexp.pexp_loc;
        exp_type = body.exp_type;
        exp_env = env }
  | Pexp_send (e, met) ->
      let object = type_exp env e in
      begin try
        let typ = filter_method env met object.exp_type in
        let exp =
      	  match object.exp_desc with
	    Texp_ident(path, {val_kind = Val_anc methods}) ->
      	      let (path, desc) =
                Env.lookup_value (Longident.Lident "*self*") env
              in
              let method_id = List.assoc met methods in
	      let method_type = newvar () in
	      let (obj_ty, res_ty) = filter_arrow env method_type in
	      unify env obj_ty desc.val_type;
	      unify env res_ty typ;
	      Texp_apply({exp_desc = Texp_ident(Path.Pident method_id,
      	       	       	       	                {val_type = method_type;
      	       	       	       	                 val_kind = Val_reg});
      	       	          exp_loc = sexp.pexp_loc;
      	       	          exp_type = method_type;
                          exp_env = env },
                         [{exp_desc = Texp_ident(path, desc);
      	       	       	   exp_loc = object.exp_loc;
      	       	       	   exp_type = desc.val_type;
                           exp_env = env }])
          | _ ->
	      Texp_send(object, met)
        in
          { exp_desc = exp;
            exp_loc = sexp.pexp_loc;
            exp_type = typ;
            exp_env = env }
      with Unify _ ->
      	raise(Error(e.pexp_loc,	Undefined_method_err met))
      end
  | Pexp_new cl ->
      let (cl_path, cl_typ) =
        try Env.lookup_class cl env with Not_found ->
          raise(Error(sexp.pexp_loc, Unbound_class cl))
      in
      	begin match cl_typ.cty_new with
	  None ->
	    raise(Error(sexp.pexp_loc, Virtual_class cl))
        | Some ty ->
            { exp_desc = Texp_new cl_path;
      	      exp_loc = sexp.pexp_loc;
	      exp_type = instance ty;
              exp_env = env }
        end
  | Pexp_setinstvar (lab, snewval) ->
      begin try
        let (path, desc) = Env.lookup_value (Longident.Lident lab) env in
        match desc.val_kind with
	  Val_ivar Mutable ->
	    let newval = type_expect env snewval desc.val_type in
      	    let (path_self, _) =
              Env.lookup_value (Longident.Lident "*self*") env
            in
            { exp_desc = Texp_setinstvar(path_self, path, newval);
              exp_loc = sexp.pexp_loc;
              exp_type = instance Predef.type_unit;
              exp_env = env }
	| Val_ivar _ ->
      	    raise(Error(sexp.pexp_loc, Instance_variable_not_mutable lab))
	| _ ->
            raise(Error(sexp.pexp_loc, Unbound_instance_variable lab))
      with
	Not_found ->
          raise(Error(sexp.pexp_loc, Unbound_instance_variable lab))
      end        
  | Pexp_override lst ->
      List.fold_right
      	(fun (lab, _) l ->
	   if List.exists ((=) lab) l then
	     raise(Error(sexp.pexp_loc,
      	       	       	 Value_multiply_overridden lab));
      	   lab::l)
	lst
	[];
      let (path_self, {val_type = self_ty}) =
      	try
          Env.lookup_value (Longident.Lident "*self*") env
	with Not_found ->
	  raise(Error(sexp.pexp_loc, Outside_class))
      in
      let type_override (lab, snewval) =
        begin try
          let (path, desc) = Env.lookup_value (Longident.Lident lab) env in
          match desc.val_kind with
	    Val_ivar _ ->
              (path, type_expect env snewval desc.val_type)
	  | _ ->
              raise(Error(sexp.pexp_loc, Unbound_instance_variable lab))
        with
	  Not_found ->
            raise(Error(sexp.pexp_loc, Unbound_instance_variable lab))
        end
      in
      let modifs = List.map type_override lst in
      { exp_desc = Texp_override(path_self, modifs);
      	exp_loc = sexp.pexp_loc;
	exp_type = self_ty;
        exp_env = env }

      (* let obj = Oo.copy self in obj.x <- e; obj *)

(* Typing of an expression with an expected type.
   Some constructs are treated specially to provide better error messages. *)

and type_expect env sexp ty_expected =
  match sexp.pexp_desc with
    Pexp_constant(Const_string s as cst) ->
      let exp =
        { exp_desc = Texp_constant cst;
          exp_loc = sexp.pexp_loc;
          exp_type =
            (* Terrible hack for format strings *)
            begin match (repr ty_expected).desc with
              Tconstr(path, _, _) when Path.same path Predef.path_format ->
                type_format sexp.pexp_loc s
            | _ -> instance Predef.type_string
            end;
          exp_env = env } in
      unify_exp env exp ty_expected;
      exp
  | Pexp_let(rec_flag, spat_sexp_list, sbody) ->
      let (pat_exp_list, new_env) = type_let env rec_flag spat_sexp_list in
      let body = type_expect new_env sbody ty_expected in
      { exp_desc = Texp_let(rec_flag, pat_exp_list, body);
        exp_loc = sexp.pexp_loc;
        exp_type = body.exp_type;
        exp_env = env }
  | Pexp_sequence(sexp1, sexp2) ->
      let exp1 = type_statement env sexp1 in
      let exp2 = type_expect env sexp2 ty_expected in
      { exp_desc = Texp_sequence(exp1, exp2);
        exp_loc = sexp.pexp_loc;
        exp_type = exp2.exp_type;
        exp_env = env }
  | _ ->
      let exp = type_exp env sexp in
      unify_exp env exp ty_expected;
      exp

(* Typing of statements (expressions whose values are discarded) *)

and type_statement env sexp =
    let exp = type_exp env sexp in
    match (repr exp.exp_type).desc with
      Tarrow(_, _) ->
        Location.print_warning sexp.pexp_loc
          "this function application is partial,\n\
           maybe some arguments are missing.";
        exp
    | _ -> exp

(* Typing of match cases *)

and type_cases env ty_arg ty_res caselist =
  List.map
    (fun (spat, sexp) ->
      let (pat, ext_env) = type_pattern env spat in
      unify_pat env pat ty_arg;
      let exp = type_expect ext_env sexp ty_res in
      (pat, exp))
    caselist

(* Typing of let bindings *)

and type_let env rec_flag spat_sexp_list =
  begin_def();
  let (pat_list, new_env) =
    type_pattern_list env (List.map (fun (spat, sexp) -> spat) spat_sexp_list)
  in
  let exp_env =
    match rec_flag with Nonrecursive -> env | Recursive -> new_env in
  let exp_list =
    List.map2
      (fun (spat, sexp) pat -> type_expect exp_env sexp pat.pat_type)
      spat_sexp_list pat_list in
  List.iter2
    (fun pat exp -> Parmatch.check_partial pat.pat_loc [pat, exp])
    pat_list exp_list;
  end_def();
  List.iter2
    (fun pat exp ->
       if not (is_nonexpansive exp) then
         iter_pattern (fun pat ->make_nongen pat.pat_type) pat)
    pat_list exp_list;
  List.iter
    (fun pat -> iter_pattern (fun pat -> generalize pat.pat_type) pat)
    pat_list;
  (List.combine pat_list exp_list, new_env)

(* Typing of toplevel bindings *)

let type_binding env rec_flag spat_sexp_list =
  Typetexp.reset_type_variables();
  type_let env rec_flag spat_sexp_list

(* Typing of toplevel expressions *)

let type_expression env sexp =
  Typetexp.reset_type_variables();
  begin_def();
  let exp = type_exp env sexp in
  end_def();
  if is_nonexpansive exp then generalize exp.exp_type
  else make_nongen exp.exp_type;
  exp

(* Typing of methods *)

let rec type_expect_fun env sexp ty_expected =
  match sexp.pexp_desc with
    Pexp_function caselist ->
      let (ty_arg, ty_res) =
        try filter_arrow env ty_expected with Unify _ ->
          raise(Error(sexp.pexp_loc, Too_many_arguments))
      in
      let cases =
        List.map
          (fun (spat, sexp) ->
             let (pat, ext_env) = type_pattern env spat in
             unify_pat env pat ty_arg;
             let exp = type_expect_fun ext_env sexp ty_res in
             (pat, exp))
          caselist
      in
      Parmatch.check_unused cases;
      Parmatch.check_partial sexp.pexp_loc cases;
      { exp_desc = Texp_function cases;
        exp_loc = sexp.pexp_loc;
        exp_type = newty (Tarrow(ty_arg, ty_res));
        exp_env = env }
  | _ ->
      type_expect env sexp ty_expected

let type_method env self self_name sexp ty_expected =
  let (obj, env) =
    Env.enter_value "*self*" {val_type = self; val_kind = Val_reg} env
  in
  let pattern =
    { pat_desc = Tpat_var obj;
      pat_loc = Location.none;
      pat_type = self }
  in
  let (pattern, env) =
    match self_name with
      None      ->
        (pattern, env)
    | Some name ->
        let (self_name, env) =
          Env.enter_value name {val_type = self; val_kind = Val_reg} env
        in
        ({ pat_desc = Tpat_alias (pattern, self_name);
	   pat_loc = Location.none;
	   pat_type = self },
	 env)
  in
  let exp = type_expect_fun env sexp ty_expected in
  { exp_desc = Texp_function [(pattern, exp)];
    exp_loc = sexp.pexp_loc;
    exp_type = newty (Tarrow(pattern.pat_type, exp.exp_type));
    exp_env = env }

(* Error report *)

open Format
open Printtyp

let report_error = function
    Unbound_value lid ->
      print_string "Unbound value "; longident lid
  | Unbound_constructor lid ->
      print_string "Unbound constructor "; longident lid
  | Unbound_label lid ->
      print_string "Unbound label "; longident lid
  | Constructor_arity_mismatch(lid, expected, provided) ->
      open_box 0;
      print_string "The constructor "; longident lid;
      print_space(); print_string "expects "; print_int expected;
      print_string " argument(s),"; print_space();
      print_string "but is here applied to "; print_int provided;
      print_string " argument(s)";
      close_box()
  | Label_mismatch(lid, trace) ->
      unification_error trace
        (function () ->
           print_string "The label "; longident lid;
           print_space(); print_string "belongs to the type")
        (function () ->
           print_string "but is here mixed with labels of type")
  | Pattern_type_clash trace ->
      unification_error trace
        (function () ->
           print_string "This pattern matches values of type")
        (function () ->
           print_string "but is here used to match values of type")
  | Multiply_bound_variable ->
      print_string "This variable is bound several times in this matching"
  | Orpat_not_closed ->
      print_string "A pattern with | must not bind variables"
  | Expr_type_clash trace ->
      unification_error trace
        (function () ->
           print_string "This expression has type")
        (function () ->
           print_string "but is here used with type")
  | Apply_non_function typ ->
      begin match (repr typ).desc with
        Tarrow(_, _) ->
          print_string "This function is applied to too many arguments"
      | _ ->
          print_string
            "This expression is not a function, it cannot be applied"
      end
  | Label_multiply_defined lid ->
      print_string "The label "; longident lid;
      print_string " is defined several times"
  | Label_missing ->
      print_string "Some labels are undefined"
  | Label_not_mutable lid ->
      print_string "The label "; longident lid;
      print_string " is not mutable"
  | Bad_format s ->
      print_string "Bad format `"; print_string s; print_string "'"
  | Undefined_method_err me ->
      print_string "This expression has no method ";
      print_string me
  | Unbound_class cl ->
      print_string "Unbound class "; longident cl
  | Virtual_class cl ->
      print_string "One cannot create instances of the virtual class ";
      longident cl
  | Unbound_instance_variable v ->
      print_string "Unbound instance variable ";
      print_string v
  | Instance_variable_not_mutable v ->
      print_string " The instance variable "; print_string v;
      print_string " is not mutable"
  | Not_subtype(tr1, tr2) ->
      reset ();
      List.iter
        (function (t, t') -> mark_loops t; if t != t' then mark_loops t')
        tr1;
      List.iter
        (function (t, t') -> mark_loops t; if t != t' then mark_loops t')
        tr2;
      trace true (fun _ -> print_string "is not a subtype of type") tr1;
      trace false (fun _ -> print_string "is not compatible with type") tr2
  | Outside_class ->
      print_string "Object duplication outside a class definition."
  | Value_multiply_overridden v ->
      print_string "The instance variable "; print_string v;
      print_string " is overridden several times"
  | Coercion_failure (ty, ty', trace) ->
      unification_error trace
        (function () ->
           mark_loops ty; if ty' != ty then mark_loops ty';
           print_string "This expression cannot be coerced to type";
           print_break 1 2;
           type_expansion ty ty';
           print_string ";";
           print_space ();
           print_string "it has type")
        (function () ->
           print_string "but is here used with type")
  | Too_many_arguments ->
      print_string "This function has too many arguments"
