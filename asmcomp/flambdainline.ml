(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Lambda
open Symbol
open Abstract_identifiers
open Flambda
open Flambdaapprox

module IntMap = Ext_types.IntMap

let new_var name =
  Variable.create ~current_compilation_unit:(Compilenv.current_unit ()) name

(* There are two types of informations propagated.
   - propagating top-down: in the env type
   - propagating following approximatively the evaluation order ~ bottom-up:
     in the ret type *)

type env = {
  env_approx : approx Variable.Map.t;
  global : (int, approx) Hashtbl.t;
  current_functions : Set_of_closures_id.Set.t;
  (* The functions currently being declared: used to avoid inlining
     recursively *)
  inlining_level : int;
  (* Number of times "inline" has been called recursively *)
  sb : Flambdasubst.t;
  inline_threshold : int;
  closure_depth : int;
}

let empty_env () =
  { env_approx = Variable.Map.empty;
    global = Hashtbl.create 10;
    current_functions = Set_of_closures_id.Set.empty;
    inlining_level = 0;
    sb = Flambdasubst.empty;
    inline_threshold = min !Clflags.inline_threshold 100;
    closure_depth = 0}

let local_env env =
  { env with
    env_approx = Variable.Map.empty;
    sb = Flambdasubst.empty }

type ret =
  { approx : approx;
    globals : approx IntMap.t;
    used_variables : Variable.Set.t;
    used_staticfail : Static_exception.Set.t;
  }

(* approximation utility functions *)

let ret (acc:ret) approx = { acc with approx }

let use_var acc var =
  { acc with used_variables = Variable.Set.add var acc.used_variables }

let exit_scope acc var =
  { acc with
    used_variables = Variable.Set.remove var acc.used_variables }

let use_staticfail acc i =
  { acc with used_staticfail = Static_exception.Set.add i acc.used_staticfail }

let exit_scope_catch acc i =
  { acc with used_staticfail = Static_exception.Set.remove i acc.used_staticfail }

let init_r () =
  { approx = value_unknown;
    globals = IntMap.empty;
    used_variables = Variable.Set.empty;
    used_staticfail = Static_exception.Set.empty }

let find id env =
  try Variable.Map.find id env.env_approx
  with Not_found ->
    Misc.fatal_error
      (Format.asprintf "unbound variable %a@." Variable.print id)

let present id env = Variable.Map.mem id env.env_approx
let add_approx id approx env =
  let approx =
    match approx.var with
    | Some var when present var env ->
        approx
    | _ ->
        { approx with var = Some id }
  in
  { env with env_approx = Variable.Map.add id approx env.env_approx }

let inlining_level_up env = { env with inlining_level = env.inlining_level + 1 }

let add_global i approx r =
  { r with globals = IntMap.add i approx r.globals }
let find_global i r =
  try IntMap.find i r.globals with
  | Not_found ->
      Misc.fatal_error
        (Format.asprintf "couldn't find global %i@." i)

(* Utility function to duplicate an expression and makes a function from it *)

let subst_toplevel sb lam =
  let subst id = try Variable.Map.find id sb with Not_found -> id in
  let f = function
    | Fvar (id,_) -> Fvar (subst id,Expr_id.create ())
    | Fset_of_closures (cl,d) ->
        Fset_of_closures (
          { cl with
            cl_specialised_arg = Variable.Map.map subst cl.cl_specialised_arg },
          Expr_id.create ())
    | e -> e
  in
  Flambdaiter.map_toplevel f lam

let make_closure_declaration id lam params =
  let free_variables = Flambdaiter.free_variables lam in
  let param_set = Variable.Set.of_list params in

  let sb =
    Variable.Set.fold
      (fun id sb -> Variable.Map.add id (Flambdasubst.freshen_var id) sb)
      free_variables Variable.Map.empty in
  let body = subst_toplevel sb lam in

  let subst id = Variable.Map.find id sb in

  let function_declaration =
    { stub = false;
      params = List.map subst params;
      free_variables = Variable.Set.map subst free_variables;
      body;
      dbg = Debuginfo.none } in

  let fv' =
    Variable.Map.fold (fun id id' fv' ->
        Variable.Map.add id' (Fvar(id,Expr_id.create ())) fv')
      (Variable.Map.filter (fun id _ -> not (Variable.Set.mem id param_set)) sb)
      Variable.Map.empty in

  let function_declarations =
    { ident = Set_of_closures_id.create (Compilenv.current_unit ());
      funs = Variable.Map.singleton id function_declaration;
      compilation_unit = Compilenv.current_unit () }
  in

  let closure =
    { cl_fun = function_declarations;
      cl_free_var = fv';
      cl_specialised_arg = Variable.Map.empty } in
  Fset_of_closures(closure, Expr_id.create ())

let check_constant_result r lam approx =
  let lam, approx = Flambdaapprox.check_constant_result lam approx in
  lam, ret r approx

let check_var_and_constant_result env r lam approx =
  let lam, r = check_constant_result r lam approx in
  let r = match lam with
    | Fvar(var,_) -> use_var r var
    | _ -> r
  in
  lam, r

let sequence l1 l2 annot =
  if Flambdaeffects.no_effects l1
  then l2
  else Fsequence(l1,l2,annot)

let really_import_approx approx =
  { approx with descr = Import.really_import approx.descr }

(* The main functions: iterate on the expression rewriting it and
   propagating up an approximation of the value.

   The naming conventions is:
   * [env] is the top down environment
   * [r] is the bottom up informations (used variables, expression
     approximation, ...)

   In general the pattern is to do a subset of these steps:
   * recursive call of loop on the arguments with the original
     environment:
       [let new_arg, r = loop env r arg]
   * generate fresh new identifiers (if subst.active is true) and
     add the substitution to the environment:
       [let new_id, env = new_subst_id id env]
   * associate in the environment the approximation of values to
     identifiers:
       [let env = add_approx id r.approx env]
   * recursive call of loop on the body of the expression, using
     the new environment
   * mark used variables:
       [let r = use_var r id]
   * remove variable related bottom up informations:
       [let r = exit_scope r id in]
   * rebuild the expression according to the informations about
     its content.
   * associate its description to the returned value:
       [ret r approx]
   * replace the returned expression by a contant or a direct variable
     acces (when possible):
       [check_var_and_constant_result env r expr approx]
 *)

let rec loop env r tree =
  let f, r = loop_direct env r tree in
  f, ret r (really_import_approx r.approx)

and loop_direct (env:env) r tree : 'a flambda * ret =
  match tree with
  | Fsymbol (sym,annot) ->
     begin
       match Flambdasubst.find_symbol_exn env.sb sym with
       | id' -> loop_direct env r (Fvar(id',annot))
       | exception Not_found -> check_constant_result r tree (Import.import_symbol sym)
     end
  | Fvar (id,annot) ->
      let id, tree =
        match Flambdasubst.find_var_exn env.sb id with
        | id' -> id', Fvar(id',annot)
        | exception Not_found -> id, tree
      in
      check_var_and_constant_result env r tree (find id env)
  | Fconst (cst,_) -> tree, ret r (const_approx cst)

  | Fapply ({ ap_function = funct; ap_arg = args;
              ap_kind = direc; ap_dbg = dbg }, annot) ->
      let funct, ({ approx = fapprox } as r) = loop env r funct in
      let args, approxs, r = loop_list env r args in
      apply ~local:false env r (funct,fapprox) (args,approxs) dbg annot

  | Fset_of_closures (cl, annot) ->
      closure env r cl annot
  | Fclosure ({fu_closure = flam;
                fu_fun;
                fu_relative_to = rel}, annot) ->
      let flam, r = loop env r flam in
      ffunction r flam fu_fun rel annot

  | Fvariable_in_closure (fenv_field, annot) as expr ->
      (* If the function from which those variables are extracted
         has been modified, we must rename the field access accordingly.
         The renaming information comes from the approximation of the
         argument. This means that we must have the informations about
         the closure (fenv_field.vc_closure) otherwise we could miss
         some renamings and generate wrong code. *)
      let arg, r = loop env r fenv_field.vc_closure in
      let closure, approx_fun_id = match r.approx.descr with
        | Value_closure { closure; fun_id } -> closure, fun_id
        | Value_unknown ->
            (* We must have the correct approximation of the value
               to avoid missing a substitution. *)
            Format.printf "Value unknown: %a@.%a@.%a@."
              Printflambda.flambda expr
              Printflambda.flambda arg
              Printflambda.flambda fenv_field.vc_closure;
            assert false
        | _ -> assert false in
      let module AR =
        Flambdasubst.Alpha_renaming_map_for_ids_and_bound_vars_of_closures
      in
      let env_var = AR.fv_off_id closure.ffunction_sb fenv_field.vc_var in
      let env_fun_id = AR.fun_off_id closure.ffunction_sb fenv_field.vc_fun in

      assert(Closure_id.equal env_fun_id approx_fun_id);

      let approx =
        try Var_within_closure.Map.find env_var closure.bound_var with
        | Not_found ->
            Format.printf "no field %a in closure %a@ %a@."
              Var_within_closure.print env_var
              Closure_id.print env_fun_id
              Printflambda.flambda arg;
            assert false in

      let expr =
        if arg == fenv_field.vc_closure
        then expr (* if the argument didn't change, the names didn't also *)
        else Fvariable_in_closure ({ vc_closure = arg; vc_fun = env_fun_id;
                                     vc_var = env_var }, annot) in
      check_var_and_constant_result env r expr approx

  | Flet(str, id, lam, body, annot) ->
      (* The different cases for rewriting Flet are, if the original code
         corresponds to [let id = lam in body]
         * [body] with id substituted by lam when possible (unused or constant)
         * [lam; body] when id is not used but lam is effectfull
         * [let id = lam in body] otherwise
       *)
      let init_used_var = r.used_variables in
      let lam, r = loop env r lam in
      let id, sb = Flambdasubst.new_subst_id env.sb id in
      let env = { env with sb; } in
      let def_used_var = r.used_variables in
      let body_env = match str with
        | Assigned ->
           (* if the variable is mutable, we don't propagate anything about it *)
           { env with env_approx = Variable.Map.add id value_unknown env.env_approx }
        | Not_assigned ->
           add_approx id r.approx env in
      (* To distinguish variables used by the body and the declaration,
         body is rewritten without the set of used variables from
         the declaration. *)
      let r_body = { r with used_variables = init_used_var } in
      let body, r = loop body_env r_body body in
      let expr, r =
        if Variable.Set.mem id r.used_variables
        then
          Flet (str, id, lam, body, annot),
          (* if lam is kept, adds its used variables *)
          { r with used_variables =
                     Variable.Set.union def_used_var r.used_variables }
        else if Flambdaeffects.no_effects lam
        then body, r
        else Fsequence(lam, body, annot),
             (* if lam is kept, adds its used variables *)
             { r with used_variables =
                        Variable.Set.union def_used_var r.used_variables } in
      expr, exit_scope r id
  | Fletrec(defs, body, annot) ->
      let defs, sb = Flambdasubst.new_subst_ids env.sb defs in
      let env = { env with sb; } in
      let def_env = List.fold_left (fun env_acc (id,lam) ->
          add_approx id value_unknown env_acc)
          env defs
      in
      let defs, body_env, r = List.fold_right (fun (id,lam) (defs, env_acc, r) ->
          let lam, r = loop def_env r lam in
          let defs = (id,lam) :: defs in
          let env_acc = add_approx id r.approx env_acc in
          defs, env_acc, r) defs ([],env,r) in
      let body, r = loop body_env r body in
      let r = List.fold_left (fun r (id,_) -> exit_scope r id) r defs in
      Fletrec (defs, body, annot),
      r
  | Fprim(Pgetglobal id, [], dbg, annot) as expr ->
      let approx =
        if Ident.is_predef_exn id
        then value_unknown
        else Import.import_global id in
      expr, ret r approx
  | Fprim(Pgetglobalfield(id,i), [], dbg, annot) as expr ->
      let approx =
        if id = Compilenv.current_unit_id ()
        then find_global i r
        else get_field i [really_import_approx (Import.import_global id)] in
      check_constant_result r expr approx
  | Fprim(Psetglobalfield i, [arg], dbg, annot) as expr ->
      let arg', r = loop env r arg in
      let expr = if arg == arg' then expr
        else Fprim(Psetglobalfield i, [arg'], dbg, annot) in
      let r = add_global i r.approx r in
      expr, ret r value_unknown
  | Fprim(Pfield i, [arg], dbg, annot) as expr ->
      let arg', r = loop env r arg in
      let expr =
        if arg == arg' then expr
        else Fprim(Pfield i, [arg'], dbg, annot) in
      let approx = get_field i [r.approx] in
      check_var_and_constant_result env r expr approx
  | Fprim(Psetfield _ as p, [arg], dbg, annot) ->
      let arg, r = loop env r arg in
      begin match r.approx.descr with
      | Value_unknown
      | Value_bottom -> ()
      | Value_block _ ->
          Misc.fatal_error "setfield on an non mutable block"
      | _ ->
          Misc.fatal_error "setfield on something strange"
      end;
      Fprim(p, [arg], dbg, annot), ret r value_unknown
  | Fprim(p, args, dbg, annot) as expr ->
      let (args', approxs, r) = loop_list env r args in
      let expr = if args' == args then expr else Fprim(p, args', dbg, annot) in
      let expr, approx = Flambdasimplify.primitive p (args, approxs) expr in
      expr, ret r approx
  | Fstaticraise(i, args, annot) ->
      let i = Flambdasubst.sb_exn env.sb i in
      let args, _, r = loop_list env r args in
      let r = use_staticfail r i in
      Fstaticraise (i, args, annot),
      ret r value_bottom
  | Fstaticcatch (i, vars, body, handler, annot) ->
      let i, sb = Flambdasubst.new_subst_exn env.sb i in
      let env = { env with sb; } in
      let body, r = loop env r body in
      if not (Static_exception.Set.mem i r.used_staticfail)
      then
        (* If the static exception is not used, we can drop the declaration *)
        body, r
      else
        let vars, sb = Flambdasubst.new_subst_ids' env.sb vars in
        let env = List.fold_left (fun env id -> add_approx id value_unknown env)
            { env with sb; } vars in
        let handler, r = loop env r handler in
        let r = List.fold_left exit_scope r vars in
        let r = exit_scope_catch r i in
        Fstaticcatch (i, vars, body, handler, annot),
        ret r value_unknown
  | Ftrywith(body, id, handler, annot) ->
      let body, r = loop env r body in
      let id, sb = Flambdasubst.new_subst_id env.sb id in
      let env = add_approx id value_unknown { env with sb; } in
      let handler, r = loop env r handler in
      let r = exit_scope r id in
      Ftrywith(body, id, handler, annot),
      ret r value_unknown
  | Fifthenelse(arg, ifso, ifnot, annot) ->
      (* When arg is the constant false or true (or something considered
         as true), we can drop the if and replace it by a sequence.
         if arg is not effectful we can also drop it. *)
      let arg, r = loop env r arg in
      begin match r.approx.descr with
      | Value_constptr 0 ->
          (* constant false, keep ifnot *)
          let ifnot, r = loop env r ifnot in
          sequence arg ifnot annot, r
      | Value_constptr _
      | Value_block _ ->
          (* constant true, keep ifso *)
          let ifso, r = loop env r ifso in
          sequence arg ifso annot, r
      | _ ->
          let ifso, r = loop env r ifso in
          let ifnot, r = loop env r ifnot in
          Fifthenelse(arg, ifso, ifnot, annot),
          ret r value_unknown
      end
  | Fsequence(lam1, lam2, annot) ->
      let lam1, r = loop env r lam1 in
      let lam2, r = loop env r lam2 in
      sequence lam1 lam2 annot,
      r
  | Fwhile(cond, body, annot) ->
      let cond, r = loop env r cond in
      let body, r = loop env r body in
      Fwhile(cond, body, annot),
      ret r value_unknown
  | Fsend(kind, met, obj, args, dbg, annot) ->
      let met, r = loop env r met in
      let obj, r = loop env r obj in
      let args, _, r = loop_list env r args in
      Fsend(kind, met, obj, args, dbg, annot),
      ret r value_unknown
  | Ffor(id, lo, hi, dir, body, annot) ->
      let lo, r = loop env r lo in
      let hi, r = loop env r hi in
      let id, sb = Flambdasubst.new_subst_id env.sb id in
      let env = add_approx id value_unknown { env with sb; } in
      let body, r = loop env r body in
      let r = exit_scope r id in
      Ffor(id, lo, hi, dir, body, annot),
      ret r value_unknown
  | Fassign(id, lam, annot) ->
      let lam, r = loop env r lam in
      let id = try Flambdasubst.find_var_exn env.sb id with
        | Not_found -> id in
      let r = use_var r id in
      Fassign(id, lam, annot),
      ret r value_unknown
  | Fswitch(arg, sw, annot) ->
      (* When arg is known to be a block with a fixed tag or a fixed integer,
         we can drop the switch and replace it by a sequence.
         if arg is not effectful we can also drop it. *)
      let arg, r = loop env r arg in
      let get_failaction () =
        (* If the switch is applied to a staticaly known value that is
           outside of each match case:
           * if there is a default action take that case
           * otherwise this is something that is guaranteed not to
             be reachable by the type checker: for instance
             [type 'a t = Int : int -> int t | Float : float -> float t
              match Int 1 with
              | Int _ -> ...
              | Float f as v ->
                  match v with       This match is unreachable
                  | Float f -> ...]
         *)
        match sw.fs_failaction with
        | None -> Funreachable (Expr_id.create ())
        | Some f -> f in
      begin match r.approx.descr with
      | Value_int i
      | Value_constptr i ->
          let lam = try List.assoc i sw.fs_consts with
            | Not_found -> get_failaction () in
          let lam, r = loop env r lam in
          sequence arg lam annot, r
      | Value_block(tag,_) ->
          let lam = try List.assoc tag sw.fs_blocks with
            | Not_found -> get_failaction () in
          let lam, r = loop env r lam in
          sequence arg lam annot, r
      | _ ->
          let f (i,v) (acc, r) =
            let lam, r = loop env r v in
            ((i,lam)::acc, r) in
          let fs_consts, r = List.fold_right f sw.fs_consts ([], r) in
          let fs_blocks, r = List.fold_right f sw.fs_blocks ([], r) in
          let fs_failaction, r = match sw.fs_failaction with
            | None -> None, r
            | Some l -> let l, r = loop env r l in Some l, r in
          let sw =
            { sw with fs_failaction; fs_consts; fs_blocks; } in
          Fswitch(arg, sw, annot),
          ret r value_unknown
      end
  | Fstringswitch(arg, sw, def, annot) ->
      let arg, r = loop env r arg in
      let sw, r = List.fold_right (fun (str, lam) (sw,r) ->
          let lam, r = loop env r lam in
          (str,lam)::sw, r)
          sw
          ([], r)
      in
      let def, r =
        match def with
        | None -> def, r
        | Some def ->
            let def, r = loop env r def in
            Some def, r
      in
      Fstringswitch(arg, sw, def, annot),
      ret r value_unknown
  | Funreachable _ -> tree, ret r value_bottom
  | Fevent _ -> assert false

and loop_list env r l = match l with
  | [] -> [], [], r
  | h::t ->
      let t', approxs, r = loop_list env r t in
      let h', r = loop env r h in
      let approxs = r.approx :: approxs in
      if t' == t && h' == h
      then l, approxs, r
      else h' :: t', approxs, r

and closure env r cl annot =
  let ffuns = cl.cl_fun in
  let fv = cl.cl_free_var in
  let spec_args = cl.cl_specialised_arg in

  let env = { env with closure_depth = env.closure_depth + 1 } in
  let spec_args = Variable.Map.map (Flambdasubst.subst_var env.sb) spec_args in
  let approxs = Variable.Map.map (fun id -> find id env) spec_args in

  let fv, r = Variable.Map.fold (fun id lam (fv,r) ->
      let lam, r = loop env r lam in
      Variable.Map.add id (lam, r.approx) fv, r) fv (Variable.Map.empty, r) in

  let env = local_env env in

  let prev_closure_symbols = Variable.Map.fold (fun id _ map ->
      let cf = Closure_id.wrap id in
      let sym = Compilenv.closure_symbol cf in
      SymbolMap.add sym id map) ffuns.funs SymbolMap.empty in

  let module AR =
    Flambdasubst.Alpha_renaming_map_for_ids_and_bound_vars_of_closures
  in
  let fv, sb, ffunction_sb = AR.subst_free_vars fv env.sb in
  let ffuns, sb, ffunction_sb = AR.ffuns_subst ffunction_sb sb ffuns in
  let env = { env with sb; } in

  let spec_args =
    Variable.Map.map_keys (Flambdasubst.subst_var env.sb) spec_args
  in
  let approxs =
    Variable.Map.map_keys (Flambdasubst.subst_var env.sb) approxs
  in
  let prev_closure_symbols =
    SymbolMap.map (Flambdasubst.subst_var env.sb) prev_closure_symbols
  in

  let env =
    { env with
      current_functions =
        Set_of_closures_id.Set.add ffuns.ident env.current_functions;
    }
  in
  (* we use the previous closure for evaluating the functions *)

  let internal_closure =
    { ffunctions = ffuns;
      bound_var = Variable.Map.fold (fun id (_,desc) map ->
          Var_within_closure.Map.add (Var_within_closure.wrap id) desc map)
          fv Var_within_closure.Map.empty;
      kept_params = Variable.Set.empty;
      ffunction_sb;
    }
  in
  let closure_env = Variable.Map.fold
      (fun id _ env -> add_approx id
          (value_closure { fun_id = (Closure_id.wrap id);
                           closure = internal_closure }) env)
      ffuns.funs env in
  let funs, used_params, r =
    Variable.Map.fold (fun fid ffun (funs,used_params,r) ->
        let closure_env = Variable.Map.fold
            (fun id (_,desc) env ->
               if Variable.Set.mem id ffun.free_variables
               then begin
                 add_approx id desc env
               end
               else env) fv closure_env in
        let closure_env = List.fold_left (fun env id ->
            let approx = try Variable.Map.find id approxs
              with Not_found -> value_unknown in
            add_approx id approx env) closure_env ffun.params in

        (***** TODO: find something better
               Warning if multiply recursive function ******)
        (* Format.printf "body:@ %a@." Printflambda.flambda ffun.body; *)
        let body = Flambdaiter.map_toplevel (function
            | Fsymbol (sym,_) when SymbolMap.mem sym prev_closure_symbols ->
                Fvar(SymbolMap.find sym prev_closure_symbols,Expr_id.create ())
            | e -> e) ffun.body in
        (* We replace recursive calls using the function symbol
           This is done before substitution because we could have something like:
             List.iter (List.iter some_fun) l
           And we need to distinguish the inner iter from the outer one
        *)

        let closure_env =
          if ffun.stub
          then { closure_env with inline_threshold = -10000 }
          else closure_env in

        let body, r = loop closure_env r body in
        let used_params = List.fold_left (fun acc id ->
            if Variable.Set.mem id r.used_variables
            then Variable.Set.add id acc
            else acc) used_params ffun.params in

        let r = Variable.Set.fold (fun id r -> exit_scope r id)
            ffun.free_variables r in
        let free_variables = Flambdaiter.free_variables body in
        Variable.Map.add fid { ffun with body; free_variables } funs,
        used_params, r)
      ffuns.funs (Variable.Map.empty, Variable.Set.empty, r) in

  let spec_args = Variable.Map.filter
      (fun id _ -> Variable.Set.mem id used_params)
      spec_args in

  let r = Variable.Map.fold (fun id' v acc -> use_var acc v) spec_args r in
  let ffuns = { ffuns with funs } in

  let kept_params = Flambdaiter.arguments_kept_in_recursion ffuns in

  let closure = { internal_closure with ffunctions = ffuns; kept_params } in
  let r = Variable.Map.fold (fun id _ r -> exit_scope r id) ffuns.funs r in
  Fset_of_closures ({cl_fun = ffuns; cl_free_var = Variable.Map.map fst fv;
             cl_specialised_arg = spec_args}, annot),
  ret r (value_unoffseted_closure closure)

and ffunction r flam off rel annot =
  let module AR =
    Flambdasubst.Alpha_renaming_map_for_ids_and_bound_vars_of_closures
  in
  let off_id closure off =
    let off = AR.fun_off_id closure.ffunction_sb off in
    (try ignore (find_declaration off closure.ffunctions)
     with Not_found ->
       Misc.fatal_error (Format.asprintf "no function %a in the closure@ %a@."
                           Closure_id.print off Printflambda.flambda flam));
    off
  in
  let closure = match r.approx.descr with
    | Value_set_of_closures closure -> closure
    | Value_closure { closure } -> closure
    | _ ->
        Format.printf "%a@.%a@." Closure_id.print off Printflambda.flambda flam;
        assert false in
  let off = off_id closure off in
  let rel = Misc.may_map (off_id closure) rel in
  let ret_approx = value_closure { fun_id = off; closure } in

  Fclosure ({fu_closure = flam; fu_fun = off; fu_relative_to = rel}, annot),
  ret r ret_approx

(* Apply a function to its parameters: if the function is known, we will go to the special cases:
   direct apply of parial apply
   local: if local is true, the application is of the shape: apply (offset (closure ...)).
          i.e. it should not duplicate the function
*)
and apply env r ~local (funct,fapprox) (args,approxs) dbg eid =
  match fapprox.descr with
  | Value_closure { fun_id; closure } ->
      let clos = closure.ffunctions in
      let func =
        try find_declaration fun_id clos with
        | Not_found ->
            Format.printf "missing %a@." Closure_id.print fun_id;
            assert false
      in
      let nargs = List.length args in
      let arity = function_arity func in
      if nargs = arity
      then direct_apply env r ~local clos funct fun_id func fapprox closure (args,approxs) dbg eid
      else
      if nargs > arity
      then
        let h_args, q_args = Misc.split_at arity args in
        let h_approxs, q_approxs = Misc.split_at arity approxs in
        let expr, r = direct_apply env r ~local clos funct fun_id func fapprox closure (h_args,h_approxs)
            dbg (Expr_id.create ()) in
        loop env r (Fapply({ ap_function = expr; ap_arg = q_args;
                             ap_kind = Indirect; ap_dbg = dbg}, eid))
      else
      if nargs > 0 && nargs < arity
      then
        let partial_fun = partial_apply funct fun_id func args dbg eid in
        loop env r partial_fun
      else

        Fapply({ ap_function = funct; ap_arg = args;
                 ap_kind = Indirect; ap_dbg = dbg}, eid),
        ret r value_unknown

  | _ ->
      Fapply ({ap_function = funct; ap_arg = args;
               ap_kind = Indirect; ap_dbg = dbg}, eid),
      ret r value_unknown

and partial_apply funct fun_id func args ap_dbg eid =
  let arity = function_arity func in
  let remaining_args = arity - (List.length args) in
  assert(remaining_args > 0);
  let param_sb = List.map (fun id -> Flambdasubst.freshen_var id) func.params in
  let applied_args, remaining_args = Misc.map2_head
      (fun arg id' -> id', arg) args param_sb in
  let call_args = List.map (fun id' -> Fvar(id', Expr_id.create ())) param_sb in
  let funct_id = new_var "partial_called_fun" in
  let new_fun_id = new_var "partial_fun" in
  let expr = Fapply ({ ap_function = Fvar(funct_id, Expr_id.create ());
                       ap_arg = call_args;
                       ap_kind = Direct fun_id; ap_dbg }, Expr_id.create ()) in
  let fset_of_closures = make_closure_declaration new_fun_id expr remaining_args in
  let offset = Fclosure ({fu_closure = fset_of_closures;
                           fu_fun = Closure_id.wrap new_fun_id;
                           fu_relative_to = None}, Expr_id.create ()) in
  let with_args = List.fold_right (fun (id', arg) expr ->
      Flet(Not_assigned, id', arg, expr, Expr_id.create ()))
      applied_args offset in
  Flet(Not_assigned, funct_id, funct, with_args, Expr_id.create ())


and functor_like env clos approxs =
  env.closure_depth = 0 &&
  List.for_all (function { descr = Value_unknown } -> false | _ -> true) approxs &&
  Variable.Set.is_empty (recursive_functions clos)

and direct_apply env r ~local clos funct fun_id func fapprox closure (args,approxs) ap_dbg eid =
  let max_level = 3 in
  let fun_size =
    if func.stub || functor_like env clos approxs
    then Some 0
    else Flambdacost.lambda_smaller' func.body
        ~than:((env.inline_threshold + List.length func.params) * 2) in
  match fun_size with
  | None ->
      Fapply ({ap_function = funct; ap_arg = args;
               ap_kind = Direct fun_id; ap_dbg}, eid),
      ret r value_unknown
  | Some fun_size ->
      let fun_var = find_declaration_variable fun_id clos in
      let recursive = Variable.Set.mem fun_var (recursive_functions clos) in
      let inline_threshold = env.inline_threshold in
      let env = { env with inline_threshold = env.inline_threshold - fun_size } in
      if func.stub || functor_like env clos approxs ||
         (not recursive && env.inlining_level <= max_level)
      then
        (* try inlining if the function is not too far above the threshold *)
        let body, r_inline = inline env r clos funct fun_id func args ap_dbg eid in
        if func.stub || functor_like env clos approxs ||
           (Flambdacost.lambda_smaller body
              ~than:(inline_threshold + List.length func.params))
        then
          (* if the definitive size is small enought: keep it *)
          body, r_inline
        else Fapply ({ ap_function = funct; ap_arg = args;
                       ap_kind = Direct fun_id; ap_dbg}, eid),
             ret r value_unknown
             (* do not use approximation: there can be renamed offsets.
                A better solution would be to use the generic approximation
                of the function *)
      else
        let kept_params = closure.kept_params in
        if
          recursive && not (Set_of_closures_id.Set.mem clos.ident env.current_functions)
          && not (Variable.Set.is_empty kept_params)
          && Var_within_closure.Map.is_empty closure.bound_var (* closed *)
          && env.inlining_level <= max_level

        then begin
          let f id approx acc =
            match approx.descr with
            | Value_unknown
            | Value_bottom -> acc
            | _ ->
                if Variable.Set.mem id kept_params
                then Variable.Map.add id approx acc
                else acc in
          let worth = List.fold_right2 f func.params approxs Variable.Map.empty in

          if not (Variable.Map.is_empty worth) && not local
          then
            duplicate_apply env r funct clos fun_id func fapprox closure
              (args,approxs) kept_params ap_dbg
          else
            Fapply ({ap_function = funct; ap_arg = args;
                     ap_kind = Direct fun_id; ap_dbg}, eid),
            ret r value_unknown
        end
        else
          Fapply ({ap_function = funct; ap_arg = args;
                   ap_kind = Direct fun_id; ap_dbg}, eid),
          ret r value_unknown

(* Inlining for recursive functions: duplicates the function
   declaration and specialise it *)
and duplicate_apply env r funct clos fun_id func fapprox closure_approx
    (args,approxs) kept_params ap_dbg =
  let env = inlining_level_up env in
  let clos_id = new_var "dup_closure" in
  let make_fv var fv =
    Variable.Map.add var
      (Fvariable_in_closure
         ({ vc_closure = Fvar(clos_id, Expr_id.create ());
            vc_fun = fun_id;
            vc_var = Var_within_closure.wrap var },
          Expr_id.create ())) fv
  in

  let variables_in_closure =
    variables_bound_by_the_closure fun_id clos in

  let fv = Variable.Set.fold make_fv variables_in_closure Variable.Map.empty in

  let env = add_approx clos_id fapprox env in

  (* TODO: remove specialisation from here and factorise with the other case *)

  let (spec_args, args, env_func) =
    let f (id,arg) approx (spec_args,args,env_func) =
      let new_id = Flambdasubst.freshen_var id in
      let args = (new_id, arg) :: args in
      let env_func = add_approx new_id approx env_func in
      let spec_args =
        match approx.descr with
        | Value_unknown
        | Value_bottom -> spec_args
        | _ ->
            if Variable.Set.mem id kept_params
            then Variable.Map.add id new_id spec_args
            else spec_args in
      spec_args, args, env_func
    in
    let params = List.combine func.params args in
    List.fold_right2 f params approxs (Variable.Map.empty,[],env) in

  let args_exprs = List.map (fun (id,_) -> Fvar(id,Expr_id.create ())) args in

  let clos_expr = (Fset_of_closures({ cl_fun = clos;
                              cl_free_var = fv;
                              cl_specialised_arg = spec_args}, Expr_id.create ())) in

  let r = exit_scope r clos_id in
  let expr = Fclosure({fu_closure = clos_expr; fu_fun = fun_id;
                        fu_relative_to = None}, Expr_id.create ()) in
  let expr = Fapply ({ ap_function = expr; ap_arg = args_exprs;
                       ap_kind = Direct fun_id; ap_dbg },
                     Expr_id.create ()) in
  let expr = List.fold_left
      (fun expr (id,arg) ->
         Flet(Not_assigned, id, arg, expr, Expr_id.create ()))
      expr args in
  let expr = Flet(Not_assigned, clos_id, funct, expr, Expr_id.create ()) in
  let r = List.fold_left (fun r (id,_) -> exit_scope r id) r args in
  loop { env with sb = Flambdasubst.activate env.sb } r expr

(* Duplicates the body of the called function *)
and inline env r clos lfunc fun_id func args dbg eid =
  let env = inlining_level_up env in
  let clos_id = new_var "inlined_closure" in

  let variables_in_closure =
    variables_bound_by_the_closure fun_id clos in

  let body =
    func.body
    |> List.fold_right2 (fun id arg body ->
        Flet(Not_assigned, id, arg, body, Expr_id.create ~name:"inline arg" ()))
      func.params args
    |> Variable.Set.fold (fun id body ->
        Flet(Not_assigned, id,
             Fvariable_in_closure
               ({ vc_closure = Fvar(clos_id, Expr_id.create ());
                  vc_fun = fun_id;
                  vc_var = Var_within_closure.wrap id },
                Expr_id.create ()),
             body, Expr_id.create ()))
      variables_in_closure
    |> Variable.Map.fold (fun id _ body ->
        Flet(Not_assigned, id,
             Fclosure ({ fu_closure = Fvar(clos_id, Expr_id.create ());
                          fu_fun = Closure_id.wrap id;
                          fu_relative_to = Some fun_id },
                        Expr_id.create ()),
             body, Expr_id.create ()))
      clos.funs
  in
  loop { env with sb = Flambdasubst.activate env.sb } r
       (Flet(Not_assigned, clos_id, lfunc, body, Expr_id.create ()))

let inline tree =
  let env = empty_env () in
  let result, r = loop env (init_r ()) tree in
  if not (Variable.Set.is_empty r.used_variables)
  then begin
    Format.printf "remaining variables: %a@.%a@."
      Variable.Set.print r.used_variables
      Printflambda.flambda result
  end;
  assert(Variable.Set.is_empty r.used_variables);
  assert(Static_exception.Set.is_empty r.used_staticfail);
  result
