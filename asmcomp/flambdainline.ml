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

(* CR mshinwell for pchambart: What about adding module [Env] as a sub-module
   of this source file, and giving it a signature?  Then we could nicely
   collect together all of the environment-related stuff.
*)
module Env : sig

  (* CR pchambart for mshinwell: abstracting this type would require redefining
     all the substitutions functions in this env module.
     Whould this be more readable ? *)
  type t = private {
    env_approx : Flambdaapprox.t Variable.Map.t;
    current_functions : Set_of_closures_id.Set.t;
    (* The functions currently being declared: used to avoid inlining
       recursively *)
    inlining_level : int;
    (* Number of times "inline" has been called recursively *)
    sb : Flambdasubst.t;
    inline_threshold : Flambdacost.inline_threshold ;
    closure_depth : int;
  }

  val empty : unit -> t

  val local : t -> t

  val inlining_level_up : t -> t
  (* This environment is used to rewrite code for inlining. This is
     used by the inlining heuristics to decide wether to continue.
     Unconditionnaly inlined does not take this into account. *)

  val find : Variable.t -> t -> Flambdaapprox.t
  (* Recover informations about the potential values of a variable.
     Fails if no information was present in the environment *)

  val present : t -> Variable.t -> bool

  val activate_substitution : t -> t
  (* Every variables declaration in the code rewriten using this environment
     will be alpha renamed *)
  val disactivate_substitution : t -> t

  val add_approx : Variable.t -> Flambdaapprox.t -> t -> t

  val clear_approx : Variable.t -> t -> t
  (* Explicitely record the fact that this variable does not carry any
     informations. Used for mutable variables *)

  val enter_set_of_closures_declaration : Set_of_closures_id.t -> t -> t

  val inside_set_of_closures_declaration : Set_of_closures_id.t -> t -> bool

  val at_toplevel : t -> bool
  (** Not inside a closure declaration.
      Toplevel code is the one evaluated when the compilation unit is loaded *)

  val set_sb : Flambdasubst.t -> t -> t

  val increase_closure_depth : t -> t

  val never_inline : t -> t

  val set_inline_threshold : Flambdacost.inline_threshold -> t -> t

end = struct

  type t = {
    env_approx : Flambdaapprox.t Variable.Map.t;
    current_functions : Set_of_closures_id.Set.t;
    (* The functions currently being declared: used to avoid inlining
       recursively *)
    inlining_level : int;
    (* Number of times "inline" has been called recursively *)
    sb : Flambdasubst.t;
    inline_threshold : Flambdacost.inline_threshold ;
    closure_depth : int;
  }

  let empty () =
    { env_approx = Variable.Map.empty;
      current_functions = Set_of_closures_id.Set.empty;
      inlining_level = 0;
      sb = Flambdasubst.empty;
      inline_threshold =
        Flambdacost.Can_inline (min !Clflags.inline_threshold 100);
      closure_depth = 0}

  let local env =
    { env with
      env_approx = Variable.Map.empty;
      sb = Flambdasubst.new_substitution env.sb }

  let inlining_level_up env = { env with inlining_level = env.inlining_level + 1 }

  let find id env =
    try Variable.Map.find id env.env_approx
    with Not_found ->
      Misc.fatal_error
        (Format.asprintf "unbound variable %a@." Variable.print id)

  let present env var = Variable.Map.mem var env.env_approx

  let activate_substitution env =
    { env with sb = Flambdasubst.activate env.sb }
  let disactivate_substitution env =
    { env with sb = Flambdasubst.empty }

  let add_approx id approx env =
    let approx =
      match approx.var with
      | Some var when present env var ->
        approx
      | _ ->
        { approx with var = Some id }
    in
    { env with env_approx = Variable.Map.add id approx env.env_approx }

  let clear_approx id env =
    { env with env_approx = Variable.Map.add id value_unknown env.env_approx }

  let enter_set_of_closures_declaration ident env =
    { env with
      current_functions =
        Set_of_closures_id.Set.add ident env.current_functions; }

  let inside_set_of_closures_declaration closure_id env =
    Set_of_closures_id.Set.mem closure_id env.current_functions

  let at_toplevel env =
    env.closure_depth = 0

  let set_sb sb env =
    { env with sb; }

  let increase_closure_depth env =
    { env with closure_depth = env.closure_depth + 1; }

  let never_inline env =
    { env with inline_threshold = Flambdacost.Never_inline }

  let set_inline_threshold inline_threshold env =
    { env with inline_threshold }
end

open Env

type ret =
  { approx : Flambdaapprox.t;
    globals : Flambdaapprox.t IntMap.t;
    used_variables : Variable.Set.t;
    used_staticfail : Static_exception.Set.t;
    benefit : Flambdacost.benefit;
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

let benefit r f =
  { r with benefit = f r.benefit }

let clear_benefit r =
  { r with benefit = Flambdacost.no_benefit }

let init_r =
  { approx = value_unknown;
    globals = IntMap.empty;
    used_variables = Variable.Set.empty;
    used_staticfail = Static_exception.Set.empty;
    benefit = Flambdacost.no_benefit }

let add_global i approx r =
  { r with globals = IntMap.add i approx r.globals }
let find_global i r =
  try IntMap.find i r.globals with
  | Not_found ->
      Misc.fatal_error
        (Format.asprintf "couldn't find global %i@." i)


let subst_toplevel sb lam =
  let subst id = try Variable.Map.find id sb with Not_found -> id in
  let f = function
    | Fvar (id,_) -> Fvar (subst id,Expr_id.create ())
    | Fset_of_closures (cl,_) ->
        Fset_of_closures (
          { cl with
            cl_specialised_arg = Variable.Map.map subst cl.cl_specialised_arg },
          Expr_id.create ())
    | e -> e
  in
  Flambdaiter.map_toplevel f lam

(* Utility function to duplicate an expression and makes a function from it *)
(* CR mshinwell for pchambart: can we kick this function out of this source
   file?  It seems generic, and maybe useful elsewhere in due course.
      pchambart: It certainly could.
 *)
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

  Fclosure
    ({ fu_closure =
         Fset_of_closures
           ({ cl_fun =
                { ident = Set_of_closures_id.create (Compilenv.current_unit ());
                  funs = Variable.Map.singleton id function_declaration;
                  compilation_unit = Compilenv.current_unit () };
              cl_free_var = fv';
              cl_specialised_arg = Variable.Map.empty },
            Expr_id.create ());
       fu_fun = Closure_id.wrap id;
       fu_relative_to = None},
     Expr_id.create ())

let check_constant_result r lam approx =
  let lam, approx = Flambdaapprox.check_constant_result lam approx in
  lam, ret r approx

let check_var_and_constant_result env r original_lam approx =
  let lam, approx =
    check_var_and_constant_result
      ~is_present_in_env:(Env.present env) original_lam approx in
  let r = ret r approx in
  let r = match lam with
    | Fvar(var,_) ->
        benefit (use_var r var)
          (Flambdacost.remove_code original_lam)
    | Fconst _ ->
        benefit r (Flambdacost.remove_code original_lam)
    | _ -> r
  in
  lam, r

let sequence l1 l2 annot =
  if Flambdaeffects.no_effects l1
  then l2
  else Fsequence(l1,l2,annot)

let really_import_approx approx =
  { approx with descr = Import.really_import approx.descr }

let populate_closure_approximations
      ~(function_declaration : _ function_declaration)
      ~(free_var_info : (_ * Flambdaapprox.t) Variable.Map.t)
      ~(parameter_approximations : Flambdaapprox.t Variable.Map.t)
      env =
  (* This adds only the minimal set of approximations to the closures.
     This is not stricly needed to restrict it like that, but it helps
     catching potential substitution bugs. *)

  (* Add approximations of used free variables *)
  let env =
    Variable.Map.fold
      (fun id (_,desc) env ->
       if Variable.Set.mem id function_declaration.free_variables
       then Env.add_approx id desc env
       else env) free_var_info env in
  (* Add known approximations of function parameters *)
  let env =
    List.fold_left
      (fun env id ->
       let approx = try Variable.Map.find id parameter_approximations
                    with Not_found -> value_unknown in
       Env.add_approx id approx env)
      env function_declaration.params in
  env

let which_function_parameters_can_we_specialize ~params ~args
      ~approximations_of_args ~kept_params =
  assert (List.length params = List.length args);
  assert (List.length args = List.length approximations_of_args);
  List.fold_right2 (fun (id, arg) approx (spec_args, args) ->
      let new_id, args =
        (* If the argument expression is not a variable, we declare a new one.
           This is needed for adding arguments to cl_specialised_arg which
           requires a variable *)
        match arg with
        | Fvar (var,_) ->
            var, args
        | _ ->
            let new_id = Flambdasubst.freshen_var id in
            let args = (new_id, arg) :: args in
            new_id, args in
      let spec_args =
        if Flambdaapprox.useful approx && Variable.Set.mem id kept_params then
          Variable.Map.add id new_id spec_args
        else
          spec_args
      in
      spec_args, args)
    (List.combine params args) approximations_of_args
    (Variable.Map.empty, [])

let fold_over_exprs_for_variables_bound_by_closure ~fun_id ~clos_id ~clos
      ~init ~f =
  Variable.Set.fold (fun var acc ->
      let expr =
        Fvariable_in_closure
          ({ vc_closure = Fvar (clos_id, Expr_id.create ());
             vc_fun = fun_id;
             vc_var = Var_within_closure.wrap var;
           },
           Expr_id.create ())
      in
      f ~acc ~var ~expr)
    (variables_bound_by_the_closure fun_id clos) init

(* CR mshinwell for pchambart: throughout, [kept_params] needs a better name.
   Those are the parameters for which it is safe to specialise the function
   body to one known approximation.
   We know that recursive calls won't modify them: they are alias of the
   parameters.
   For instance x is in kept_params:
     [let rec f x y = (f x y) + (f x (y+1))]
   partial application also works
   [let rec f x l = List.iter (f x) l]

   Maybe naming it for what it is used for instead of what it is may be better ?
   Does parameter_safe_for_specialisation sound ok ?
 *)

let should_inline_function_known_to_be_recursive ~func ~clos ~env ~closure
      ~approxs ~kept_params ~max_level =
  assert (List.length func.params = List.length approxs);
  (not (Env.inside_set_of_closures_declaration clos.ident env))
    && (not (Variable.Set.is_empty closure.kept_params))
    && Var_within_closure.Map.is_empty closure.bound_var (* closed *)
    && env.inlining_level <= max_level
    && List.exists2 (fun id approx ->
          Flambdaapprox.useful approx && Variable.Set.mem id kept_params)
        func.params approxs

(* Make an informed guess at whether [clos], with approximations [approxs],
   looks to be a functor. *)
let functor_like env clos approxs =
  Env.at_toplevel env &&
    List.for_all Flambdaapprox.known approxs &&
    Variable.Set.is_empty (recursive_functions clos)

let transform_closure_expression r fu_closure closure_id rel annot =
  let module AR =
    Flambdasubst.Alpha_renaming_map_for_ids_and_bound_vars_of_closures
  in
  (* CXR mshinwell for pchambart: we should rename [off_id] now.
        pchambart: done *)
  let subst_closure_id closure closure_id =
    let closure_id = AR.subst_closure_id closure.ffunction_sb closure_id in
    (try ignore (find_declaration closure_id closure.ffunctions)
     with Not_found ->
       Misc.fatal_error (Format.asprintf "no function %a in the closure@ %a@."
                           Closure_id.print closure_id
                           Printflambda.flambda fu_closure));
    closure_id
  in
  match r.approx.descr with
  | Value_set_of_closures set_of_closures
  | Value_closure { set_of_closures } ->
    let closure_id = subst_closure_id set_of_closures closure_id in
    let rel = Misc.may_map (subst_closure_id set_of_closures) rel in
    let ret_approx = value_closure { fun_id = closure_id; set_of_closures } in
    Fclosure ({fu_closure; fu_fun = closure_id; fu_relative_to = rel}, annot),
    ret r ret_approx
  | Value_unresolved sym ->
    (* If the set_of_closure comes from a symbol that can't be recovered,
       we know that it comes from another compilation unit, hence it cannot
       have been transformed during this rewriting. So it is safe to keep
       this expression unchanged. *)
    Fclosure ({fu_closure; fu_fun = closure_id; fu_relative_to = rel}, annot),
    ret r (value_unresolved sym)
  | _ ->
    Format.printf "%a@.%a@." Closure_id.print closure_id
      Printflambda.flambda fu_closure;
    assert false

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
       [let env = Env.add_approx id r.approx env]
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

and loop_direct (env:Env.t) r tree : 'a flambda * ret =
  match tree with
  | Fsymbol (sym,_annot) ->
      check_constant_result r tree (Import.import_symbol sym)
  | Fvar (id,annot) ->
      let id = Flambdasubst.subst_var env.sb id in
      let tree = Fvar(id,annot) in
      check_var_and_constant_result env r tree (find id env)
  | Fconst (cst,_) -> tree, ret r (const_approx cst)
  | Fapply ({ ap_function = funct; ap_arg = args;
              ap_kind = _; ap_dbg = dbg }, annot) ->
      let funct, ({ approx = fapprox } as r) = loop env r funct in
      let args, approxs, r = loop_list env r args in
      transform_application_expression env r (funct, fapprox) (args, approxs)
        dbg annot
  | Fset_of_closures (set_of_closures, annot) ->
      transform_set_of_closures_expression env r set_of_closures annot
  | Fclosure (closure, annot) ->
      let flam, r = loop env r closure.fu_closure in
      transform_closure_expression r flam closure.fu_fun
          closure.fu_relative_to annot
  | Fvariable_in_closure (fenv_field, annot) as expr ->
      (* CR mshinwell for pchambart: I think we need a small example here.
         I also rewrote the comment, please check it is correct.  Also, I
         think we need to explain thoroughly why it is the case that a
         [Value_closure] approximation is always present here.
         pchambart: I tried to write an example, but ocaml syntax is
           too high level to explain that, and the concrete syntax is too
           heavy (and there is no syntax for maps...)
      *)
      (* This kind of expression denotes an access to a variable bound in
         a closure.  Variables in the closure ([fenv_field.vc_closure]) may
         have been alpha-renamed since [expr] was constructed; as such, we
         must ensure the same happens to [expr].  The renaming information is
         contained within the approximation deduced from [vc_closure] (as
         such, that approximation *must* identify which closure it is).

         For instance in some imaginary syntax for flambda:

           [let f x =
              let g y ~closure:{a} = a + y in
              let closure = { a = x } in
                g 12 ~closure]

         when [f] is traversed, [g] can be inlined, resulting in the
         expression

           [let f z =
              let g y ~closure:{a} = a + y in
              let closure = { a = x } in
                closure.a + 12]

         [closure.a] being a notation for:

           [Fvariable_in_closure{vc_closure = closure; vc_fun = g; vc_var = a}]

         If [f] is inlined later, the resulting code will be

           [let x = ... in
            let g' y' ~closure':{a'} = a' + y' in
            let closure' = { a' = x } in
              closure'.a' + 12]

         in particular the field [a] of the closure has been alpha renamed to [a'].
         This information must be carried from the declaration to the use.

         If the function is declared outside of the alpha renamed part, there is
         no need for renaming in the [Ffunction] and [Fvariable_in_closure].
         This is not usualy the case, except when the closure declaration is a
         symbol.

         What ensures that This information is available at [Fvariable_in_closure]
         point is that those constructions can only be introduced by inlining,
         which requires those same informations. For this to still be valid,
         other transformation must avoid transforming the information flow in
         a way that the inline function can't propagate it.
      *)
      (* CXR mshinwell: this may be a stupid question, but why is "arg" called
         "arg"?
           pchambart: it is some kind of argument for the Fvariable_in_closure
         construction. This is clearly a bad name. I will rename to "vc_closure".
           done
      *)
      let vc_closure, r = loop env r fenv_field.vc_closure in
      begin match r.approx.descr with
      | Value_closure { set_of_closures; fun_id } ->
        let module AR =
          Flambdasubst.Alpha_renaming_map_for_ids_and_bound_vars_of_closures
        in
        let env_var =
          AR.subst_variable_in_closure
            set_of_closures.ffunction_sb
            fenv_field.vc_var
        in
        let env_fun_id =
          AR.subst_closure_id
            set_of_closures.ffunction_sb
            fenv_field.vc_fun
        in

        assert(Closure_id.equal env_fun_id fun_id);

        let approx =
          try Var_within_closure.Map.find env_var set_of_closures.bound_var with
          | Not_found ->
            Format.printf "no field %a in closure %a@ %a@."
              Var_within_closure.print env_var
              Closure_id.print env_fun_id
              Printflambda.flambda vc_closure;
            assert false in

        let expr =
          if vc_closure == fenv_field.vc_closure
          then expr (* if the argument didn't change, the names didn't also *)
          else Fvariable_in_closure ({ vc_closure; vc_fun = env_fun_id;
                                       vc_var = env_var }, annot) in
        check_var_and_constant_result env r expr approx

      | Value_unresolved sym ->
        (* This value comes from a symbol for which we couldn't find any
           information. This tells us that this function couldn't have been
           renamed. So we can keep it unchanged *)
        Fvariable_in_closure ({ fenv_field with vc_closure }, annot),
        ret r (value_unresolved sym)
      | Value_unknown ->
        (* We must have the correct approximation of the value to ensure
           we take account of all alpha-renamings. *)
        Format.printf "[Fvariable_in_closure] without suitable \
                       approximation : %a@.%a@.%a@."
          Printflambda.flambda expr
          Printflambda.flambda vc_closure
          Printflambda.flambda fenv_field.vc_closure;
        assert false
      | _ -> assert false
      end
  | Flet(str, id, lam, body, annot) ->
      (* The different cases for rewriting [Flet] are, if the original code
         corresponds to [let id = lam in body],
         * [body] with [id] substituted by [lam] when possible (unused or
           constant);
         * [lam; body] when id is not used but [lam] has a side effect;
         * [let id = lam in body] otherwise.
       *)
      let init_used_var = r.used_variables in
      let lam, r = loop env r lam in
      let id, sb = Flambdasubst.new_subst_id env.sb id in
      let env = Env.set_sb sb env in
      let def_used_var = r.used_variables in
      let body_env = match str with
        | Assigned ->
           (* if the variable is mutable, we don't propagate anything about it *)
           Env.clear_approx id env
        | Not_assigned ->
           Env.add_approx id r.approx env in
      (* To distinguish variables used by the body and the declaration,
         [body] is rewritten without the set of used variables from
         the declaration. *)
      let r_body = { r with used_variables = init_used_var } in
      let body, r = loop body_env r_body body in
      let expr, r =
        if Variable.Set.mem id r.used_variables
        then
          Flet (str, id, lam, body, annot),
          (* if [lam] is kept, add its used variables *)
          { r with used_variables =
                     Variable.Set.union def_used_var r.used_variables }
        (* CR mshinwell for pchambart: This looks like a copy of
           the function called [sequence], above
              pchambart: it almost a copy, but we can't return the
           same 'r' in both cases as in other uses of [sequence].
           In fact in the other cases, it should also avoid preventing
           the elimination of unused variables like here, but it didn't
           seem as important as for the let.
           I should find a nice pattern to allow to do that elsewhere
           without too much syntactic noise. *)
        else if Flambdaeffects.no_effects lam
        then
          let r = benefit r (Flambdacost.remove_code lam) in
          body, r
        else Fsequence(lam, body, annot),
             (* if [lam] is kept, add its used variables *)
             { r with used_variables =
                        Variable.Set.union def_used_var r.used_variables } in
      expr, exit_scope r id
  | Fletrec(defs, body, annot) ->
      let defs, sb = Flambdasubst.new_subst_ids env.sb defs in
      let env = Env.set_sb sb env in
      let def_env = List.fold_left (fun env_acc (id,_lam) ->
          Env.add_approx id value_unknown env_acc)
          env defs
      in
      let defs, body_env, r = List.fold_right (fun (id,lam) (defs, env_acc, r) ->
          let lam, r = loop def_env r lam in
          let defs = (id,lam) :: defs in
          let env_acc = Env.add_approx id r.approx env_acc in
          defs, env_acc, r) defs ([],env,r) in
      let body, r = loop body_env r body in
      let r = List.fold_left (fun r (id,_) -> exit_scope r id) r defs in
      Fletrec (defs, body, annot),
      r
  | Fprim(Pgetglobal id, [], _dbg, _annot) as expr ->
      let approx =
        if Ident.is_predef_exn id
        then value_unknown
        else Import.import_global id in
      expr, ret r approx
  | Fprim(Pgetglobalfield(id,i), [], _dbg, _annot) as expr ->
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
  | Fprim((Psetfield _ | Parraysetu _ | Parraysets _) as p, block :: args, dbg, annot) ->
      let block, r = loop env r block in
      if Flambdaapprox.is_certainly_immutable r.approx
      then Misc.fatal_error "Assignement on non-mutable value";
      (* CXR mshinwell for pchambart: This is slightly mysterious---I think
         we need a comment explaining what the approximation for a
         mutable block looks like.
         Also, this match should be explicitly exhaustive; same elsewhere.
         pchambart:
           This was completely wrong and never triggered.
           done commenting.
      *)
      (* Mutable blocks are always represented by [Value_unknown] or
         [Value_bottom]. If something else is propagated here, then
         whe know that some miscompilation could happen.
         This is probably an user using [Obj.magic] or [Obj.set_field] in
         an inappropriate situation.
         Such a situation could be
         [let x = (1,1) in
          Obj.set_field (Obj.repr x) 0 (Obj.repr 2);
          assert(fst x = 2)]
         The user would probably expect the assertion to be true, but the
         compiler could propagate the value of [x].
         This certainly won't always prevent this kind of errors, but may
         prevent most of them.

         This may not be completely correct as some correct unreachable
         code branch could also trigger it. But the likelyness seems small
         enouth to prefer to catch those errors.

         example of such a problematic pattern could be
         [type a = { a : int }
          type b = { mutable b : int }
          type _ t =
            | A : a t
            | B : b t
          let f (type x) (v:x t) (r:x) =
            match v with
            | A -> r.a
            | B -> r.b <- 2; 3

         let v =
         let r =
           ref A in
           r := A; (* Some pattern that the compiler can't understand *)
           f !r { a = 1 }]
         when inlining [f], the B branch is unreachable, yet the compiler
         can't prove it and needs to keep it.
      *)
      let args, _, r = loop_list env r args in
      Fprim(p, block :: args, dbg, annot), ret r value_unknown
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
      let env = Env.set_sb sb env in
      let body, r = loop env r body in
      if not (Static_exception.Set.mem i r.used_staticfail)
      then
        (* If the static exception is not used, we can drop the declaration *)
        body, r
      else
        let vars, sb = Flambdasubst.new_subst_ids' env.sb vars in
        let env = List.fold_left (fun env id -> Env.add_approx id value_unknown env)
            (Env.set_sb sb env) vars in
        let handler, r = loop env r handler in
        let r = List.fold_left exit_scope r vars in
        let r = exit_scope_catch r i in
        Fstaticcatch (i, vars, body, handler, annot),
        ret r value_unknown
  | Ftrywith(body, id, handler, annot) ->
      let body, r = loop env r body in
      let id, sb = Flambdasubst.new_subst_id env.sb id in
      let env = Env.add_approx id value_unknown (Env.set_sb sb env) in
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
          let r = benefit r Flambdacost.remove_branch in
          sequence arg ifnot annot, r
      | Value_constptr _
      | Value_block _ ->
          (* constant true, keep ifso *)
          let ifso, r = loop env r ifso in
          let r = benefit r Flambdacost.remove_branch in
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
      let env = Env.add_approx id value_unknown (Env.set_sb sb env) in
      let body, r = loop env r body in
      let r = exit_scope r id in
      Ffor(id, lo, hi, dir, body, annot),
      ret r value_unknown
  | Fassign(id, lam, annot) ->
      let lam, r = loop env r lam in
      let id = Flambdasubst.subst_var env.sb id in
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
          let r = benefit r Flambdacost.remove_branch in
          sequence arg lam annot, r
      | Value_block(tag,_) ->
          let lam = try List.assoc tag sw.fs_blocks with
            | Not_found -> get_failaction () in
          let lam, r = loop env r lam in
          let r = benefit r Flambdacost.remove_branch in
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

(* CR mshinwell for pchambart: Could you write some more explanation as to
   how this function works?  We can probably refactor it some after that. *)
(* Transforms closure definitions by applying [loop] on the code of every
   one of the set and on the expressions of the free variables.
   If the substitution is activated, alpha renaming also occur on everything
   defined by the set of closures:
   * Variables bound by a closure of the set
   * closure identifiers
   * parameters

   The rewriting occur in a clean environment without any of the variables
   defined outside reachable. This prevents the simplification of variable
   access [check_var_and_constant_result] to occur on unsound cases.

   The rewriting occurs in an environment filled with:
   * The approximation of the free variables
   * An explicitely unknown approximation for function parameters,
     except for those where it is known to be safe: those present in the
     [cl_specialised_arg] set.
   * An approximation for the closures in the set. It contains the code of
     the functions before rewriting.

   The approximation of the currently defined closures is available to
   allow marking recursives calls as direct and in some cases, allow
   inlining of one closure from the set inside another one. For this to
   be correct an alpha renaming is first applied on the expressions by
   [subst_function_declarations_and_free_variables].

   For instance when rewriting the declaration

     [let rec f_1 x_1 =
        let y_1 = x_1 + 1 in
        g_1 y_1
      and g_1 z_1 = f_1 (f_1 z_1)]

   When rewriting this function, the first substitution will contain
   some mapping:
   { f_1 -> f_2;
     g_1 -> g_2;
     x_1 -> x_2;
     z_1 -> z_2 }

   And the approximation for the closure will contain

   { f_2:
       fun x_2 ->
         let y_1 = x_2 + 1 in
         g_2 y_1
     g_2:
       fun z_2 -> f_2 (f_2 z_2) }

   Note that no substitution is applied to the let-bound variable [y_1].
   If [f_2] where to be inlined inside [g_2], we known that a new substitution
   will be introduced in the current scope for [y_1] each time.


   If the function where a recursive one comming from another compilation
   unit, the code already went through [Flambdasym] that could have
   replaced the function variable by the symbol identifying the function
   (this occur if the function contains only constants in its closure).
   To handle that case, we first replace those symbols by the original
   variable.
*)
and transform_set_of_closures_expression env r cl annot =
  let ffuns =
    Flambdasubst.rewrite_recursive_calls_with_symbols env.sb cl.cl_fun
  in
  let fv = cl.cl_free_var in

  let env = Env.increase_closure_depth env in
  let cl_specialised_arg =
    Variable.Map.map
      (Flambdasubst.subst_var env.sb)
      cl.cl_specialised_arg
  in
  let fv, r = Variable.Map.fold (fun id lam (fv,r) ->
      let lam, r = loop env r lam in
      Variable.Map.add id (lam, r.approx) fv, r) fv (Variable.Map.empty, r)
  in
  let environment_before_cleaning = env in
  (* Remove every variable binding from the environment.
     This isn't necessary, but allows to catch bugs
     concerning variable escaping their scope. *)
  let env = Env.local env in

  let module AR =
    Flambdasubst.Alpha_renaming_map_for_ids_and_bound_vars_of_closures
  in
  let fv, ffuns, sb, ffunction_sb =
    AR.subst_function_declarations_and_free_variables env.sb fv ffuns in
  let env = Env.set_sb sb env in

  let apply_substitution = Flambdasubst.subst_var env.sb in
  let cl_specialised_arg =
    Variable.Map.map_keys apply_substitution cl_specialised_arg
  in
  let parameter_approximations =
    (* The approximation of arguments that are known to be always the same *)
    Variable.Map.map_keys apply_substitution
      (Variable.Map.map (fun id -> find id environment_before_cleaning)
         cl_specialised_arg)
  in

  let env = Env.enter_set_of_closures_declaration ffuns.ident env in

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

  (* Populate the environment with the approximation of each closure.
     This part of the environment is shared between all of the closures in
     the set of closures. *)
  let set_of_closures_env = Variable.Map.fold
      (fun id _ env -> Env.add_approx id
          (value_closure { fun_id = (Closure_id.wrap id);
                           set_of_closures = internal_closure }) env)
      ffuns.funs env in

  (* rewrite the function *)
  let rewrite_function fid ffun (funs,used_params,r) =

    let closure_env =
      populate_closure_approximations
        ~function_declaration:ffun
        ~free_var_info:fv
        ~parameter_approximations
        set_of_closures_env in

    (* We do not inline inside stubs: a stub is always inlined, allowing to
       inline inside a stub could result to forcing more code than expected
       to be inlined. *)
    let closure_env =
      if ffun.stub
      then Env.never_inline closure_env
      else closure_env in

    let body, r = loop closure_env r ffun.body in
    let used_params =
      List.fold_left
        (fun acc id ->
         if Variable.Set.mem id r.used_variables
         then Variable.Set.add id acc
         else acc) used_params ffun.params in

    let r =
      Variable.Set.fold
        (fun id r -> exit_scope r id)
        ffun.free_variables r in
    let free_variables = Flambdaiter.free_variables body in
    Variable.Map.add fid { ffun with body; free_variables } funs,
    used_params, r in


  let funs, used_params, r =
    Variable.Map.fold rewrite_function
      ffuns.funs (Variable.Map.empty, Variable.Set.empty, r) in

  (* Parameters that are not used by the function may have any corresponding
     specialized arguments removed from [cl_specialised_arg]. *)
  let cl_specialised_arg = Variable.Map.filter
      (fun id _ -> Variable.Set.mem id used_params)
      cl_specialised_arg in

  let r = Variable.Map.fold (fun _id' v acc -> use_var acc v) cl_specialised_arg r in
  let ffuns = { ffuns with funs } in

  let kept_params = Flambdaiter.arguments_kept_in_recursion ffuns in

  let closure = { internal_closure with ffunctions = ffuns; kept_params } in
  let r = Variable.Map.fold (fun id _ r -> exit_scope r id) ffuns.funs r in
  Fset_of_closures ({cl_fun = ffuns; cl_free_var = Variable.Map.map fst fv;
             cl_specialised_arg}, annot),
  ret r (value_set_of_closures closure)

(* Transform an flambda function application based on information provided
   by an approximation of the function being applied.

   If the approximation does not identify which closure is being applied, the
   application remains as-is.

   Otherwise, we determine whether the application is actually a full or
   partial application (note that previously it may have appeared as partial,
   but now we may know from the approximation that it is full).  The
   interesting case is that of a full application: we then consider whether
   the function can be inlined.  (See [direct_apply], below.)
*)
and transform_application_expression env r (funct, fapprox)
      (args, approxs) dbg eid =
  let no_transformation () =
    Fapply ({ap_function = funct; ap_arg = args;
             ap_kind = Indirect; ap_dbg = dbg}, eid),
      ret r value_unknown
  in
  match fapprox.descr with
  | Value_closure { fun_id; set_of_closures } ->
      let clos = set_of_closures.ffunctions in
      let func =
        try find_declaration fun_id clos with
        | Not_found ->
            Format.printf "approximation references non-existent closure %a@."
                Closure_id.print fun_id;
            assert false
      in
      let nargs = List.length args in
      let arity = function_arity func in
      if nargs = arity then
        direct_apply env r clos funct fun_id func set_of_closures
          (args, approxs) dbg eid
      else if nargs > arity then
        let h_args, q_args = Misc.split_at arity args in
        let h_approxs, _q_approxs = Misc.split_at arity approxs in
        let expr, r =
          direct_apply env r clos funct fun_id func set_of_closures
            (h_args,h_approxs) dbg (Expr_id.create ())
        in
        loop env r (Fapply({ ap_function = expr; ap_arg = q_args;
                             ap_kind = Indirect; ap_dbg = dbg}, eid))
      else if nargs > 0 && nargs < arity then
        let partial_fun = partial_apply funct fun_id func args dbg in
        loop env r partial_fun
      else
        no_transformation ()
  | _ -> no_transformation ()

(* Examine a full application of a known closure to determine whether to
   inline. *)
and direct_apply env r clos funct fun_id func closure
    (args, approxs) ap_dbg eid =
  let no_transformation () =
    Fapply ({ap_function = funct; ap_arg = args;
             ap_kind = Direct fun_id; ap_dbg}, eid),
    ret r value_unknown
  in
  let max_level = 3 in
  (* If [unconditionally_inline] is [true], then the function will always be
     inlined, and the strategy used will be that for non-recursive functions.

     The cases where this happens are:
     1. Stub functions for handling tuplified functions (generated during closure
        conversion).
     2. Stub functions for handling default optional arguments (generated in
        bytecomp/simplify.ml).
     3. Functor-like functions, viz. [functor_like].

     In the third case, we know from the definition of the [functor_like]
     predicate that the function is non-recursive.  In the other two cases, the
     functions may actually be recursive, but not "directly recursive" (where we
     say a function [f] is "directly recursive" if [f] is free in the body of
     [f]).  It would in general be wrong to mark directly recursive functions as
     stubs, even if specific cases work correctly.
  *)
  (* CR mshinwell for mshinwell: finish the comment *)
  let unconditionally_inline = func.stub || functor_like env clos approxs in
  let num_params = List.length func.params in
  (* CR pchambart to pchambart: find a better name
     This is true if the function is directly an argument of the
     apply construction. *)
  let direct_apply = match funct with
    | Fclosure ({ fu_closure = Fset_of_closures _ }, _) -> true
    | _ -> false in
  let fun_cost =
    if unconditionally_inline || direct_apply then
      (* CXR mshinwell for mshinwell: this comment needs clarification *)
      (* A function is considered for inlining if it does not increase the code
         size too much. This size is verified after effectively duplicating
         and specialising the code in the current context. In that context,
         some local calls can have new opportunity for inlining, for instance.
         [let f g x = g x + 1
          let h x = ...
          let v = f h 1]
         When inlining [f], [g] becomes known and so [h] can be inlined too.
         Inlining only [f] will usualy fit the size constraint and will be
         beneficial. But depending on [h] it can or cannot be beneficial to
         inline it: If [h] is too big, it may be possible to inline it in [f],
         but that may prevent [f] from being inlinable after verification.
         To prevent that, the maximal size increase allowed to [h] is reduced
         by what is consumed by [f].
         In the case of stub functions, we know that the function is small
         enouth and has a high probability of reducing the size of the
         code around it, hence we know that trying to inline it won't prevent
         the surrounding function from being inlined.

         CR pchambart: The case of functors should not be always treated as
           stub functions. It won't often decrease the function size hence
           will probably prevent a function from being inlined, loosing the
           benefit of the potential inlining.
           It may be reasonnable to consider that reavealing an opportunity
           for inlining a functor as sufficient for forced inlining.
         CR pchambart: The heuristic is half broken as the potential local
           inlines are not accumulated. For instance, in the previous example
           if f was [let f g x = g (g x)], if g was just bellow the quota,
           it could considered the two times.
           To correct that, the threshold should be propagated through [r]
           rather than [env]
      *)
      env.inline_threshold
    else
      Flambdacost.can_try_inlining func.body env.inline_threshold
        ~bonus:num_params
  in
  match fun_cost with
  | Flambdacost.Never_inline -> no_transformation ()
  | (Flambdacost.Can_inline _) as remaining_inline_threshold ->
      let fun_var = find_declaration_variable fun_id clos in
      let recursive = Variable.Set.mem fun_var (recursive_functions clos) in
      (* CR mshinwell for mshinwell: add comment about stub functions *)
      (* CR mshinwell for pchambart: two variables called [threshold] and
         [inline_threshold] is confusing.
         pchambart: is [remaining_inline_threshold] better ? *)
      let inline_threshold = env.inline_threshold in
      let env = Env.set_inline_threshold remaining_inline_threshold env in
      let kept_params = closure.kept_params in
      (* Try inlining if the function is non-recursive and not too far above
         the threshold (or if the function is to be unconditionally inlined). *)
      if unconditionally_inline
        || (not recursive && env.inlining_level <= max_level)
      then
        let body, r_inlined =
          inline_non_recursive_function env (clear_benefit r) clos funct
            fun_id func args
        in
        (* Now we know how large the inlined version actually is, determine
           whether or not to keep it. *)
        if unconditionally_inline
        || direct_apply
        || Flambdacost.sufficient_benefit_for_inline
             body
             r_inlined.benefit
             inline_threshold
        then
          let r_inlined =
            benefit r_inlined (Flambdacost.benefit_union r.benefit)
          in
          body, r_inlined
        else
          (* r_inlined contains an approximation that may be invalid for the
             untransformed expression: it may reference functions that only
             exists if the body of the function is effectively inlined.
             If the function approximation contained an approximation that
             does not depends on the effective value of its arguments, it
             could be returned instead of [value_unknown] *)
          no_transformation ()
      else if recursive then
        match funct with
        | Fclosure ({ fu_closure = Fset_of_closures (set_of_closures, _)}, _) ->
            specialise_without_duplicating_recursive_functions env r clos
              fun_id func (args,approxs) set_of_closures kept_params ap_dbg
              (no_transformation ())
        | _ ->
            if should_inline_function_known_to_be_recursive ~func ~clos ~env
                ~closure ~approxs ~kept_params ~max_level
            then
              inline_recursive_functions env r funct clos fun_id func
                (args,approxs) kept_params ap_dbg
            else
              no_transformation ()
      else
        no_transformation ()

and partial_apply funct fun_id func args ap_dbg =
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
  let closures = make_closure_declaration new_fun_id expr remaining_args in
  let with_args = List.fold_right (fun (id', arg) expr ->
      Flet(Not_assigned, id', arg, expr, Expr_id.create ()))
      applied_args closures in
  Flet(Not_assigned, funct_id, funct, with_args, Expr_id.create ())

(* Inlining of a non-recursive function just yields a copy of the function's
   body.  The declaration itself is not duplicated.
   We handle below, one by one, the three kinds of identifiers that may
   occur in the body:
   1. the function's parameters;
   2. variables "bound by the closure" (see flambda.mli for definition);
   3. any other function identifiers bound by the declaration where this
      function was defined (for the case of simultaneous definition of
      functions).
*)
and inline_non_recursive_function env r clos lfunc fun_id func args =
  let r = benefit r Flambdacost.remove_call in
  let env = inlining_level_up env in
  let clos_id = new_var "inline_non_recursive_function" in
  (* 1. First, around the function's body, bind the parameters to the arguments
     that we saw at the call site. *)
  let bindings_for_params_around_body =
    List.fold_right2 (fun id arg body ->
        Flet (Not_assigned, id, arg, body,
          Expr_id.create ~name:"inline arg" ()))
      func.params args func.body
  in
  (* 2. Now add bindings for variables bound by the closure. *)
  let bindings_for_vars_bound_by_closure_and_params_around_body =
    fold_over_exprs_for_variables_bound_by_closure ~fun_id ~clos_id ~clos
      ~init:bindings_for_params_around_body ~f:(fun ~acc:body ~var ~expr ->
        Flet (Not_assigned, var, expr, body, Expr_id.create ()))
  in
  (* 3. Finally add bindings for the function declaration identifiers being
     introduced by the whole set of closures. *)
  let expr =
    Variable.Map.fold (fun id _ expr ->
        Flet (Not_assigned, id,
          Fclosure (
            { fu_closure = Fvar (clos_id, Expr_id.create ());
              fu_fun = Closure_id.wrap id;
              fu_relative_to = Some fun_id;
            }, Expr_id.create ()),
          expr, Expr_id.create ()))
      clos.funs bindings_for_vars_bound_by_closure_and_params_around_body
  in
  loop (activate_substitution env) r
       (Flet(Not_assigned, clos_id, lfunc, expr, Expr_id.create ()))

(* Inlining of recursive function(s) yields a copy of the functions'
   definitions (not just their bodies, unlike the non-recursive case) and
   a direct application of the new body.
   Note: the function really does need to be recursive (but possibly only via
   some mutual recursion) to end up in here; a simultaneous binding [that is
   non-recursive] is not sufficient.
*)
and inline_recursive_functions env r funct clos fun_id func
    (args, approxs) kept_params ap_dbg =
  let env = inlining_level_up env in
  let clos_id = new_var "inline_recursive_functions" in
  let fv =
    fold_over_exprs_for_variables_bound_by_closure ~fun_id ~clos_id ~clos
      ~init:Variable.Map.empty
      ~f:(fun ~acc ~var ~expr -> Variable.Map.add var expr acc)
  in
  let spec_args, args =
    which_function_parameters_can_we_specialize ~params:func.params
      ~args ~approximations_of_args:approxs ~kept_params
  in
  (* First we generate a copy of the function application, including the
     function declaration(s), but with variables (not yet bound) in place of
     the arguments. *)
  let duplicated_application =
    Fapply (
      { ap_function =
          Fclosure (
            { fu_closure = Fset_of_closures (
                { cl_fun = clos;
                  cl_free_var = fv;
                  cl_specialised_arg = spec_args;
                }, Expr_id.create ());
              fu_fun = fun_id;
              fu_relative_to = None;
            }, Expr_id.create ());
        ap_arg = List.map (fun (id, _) -> Fvar (id, Expr_id.create ())) args;
        ap_kind = Direct fun_id;
        ap_dbg;
      }, Expr_id.create ())
  in
  (* Now we bind the variables that will hold the arguments from the original
     application, together with the set-of-closures identifier. *)
  let expr =
    Flet (Not_assigned, clos_id, funct,
      List.fold_left (fun expr (id, arg) ->
          Flet (Not_assigned, id, arg, expr, Expr_id.create ()))
        duplicated_application args,
      Expr_id.create ())
  in
  loop (activate_substitution env) r expr

and specialise_without_duplicating_recursive_functions env r clos fun_id
    func (args, approxs) set_of_closures kept_params ap_dbg default =
  let spec_args, args =
    which_function_parameters_can_we_specialize ~params:func.params
      ~args ~approximations_of_args:approxs ~kept_params
  in
  if Variable.Set.equal
      (Variable.Map.keys set_of_closures.cl_specialised_arg)
      (Variable.Map.keys spec_args)
      (* if the function already has the right set of specialised arguments,
         then there is nothing to do to improve it here. *)
  then default
  else
    let application =
      Fapply (
        { ap_function =
            Fclosure (
              { fu_closure = Fset_of_closures (
                   { cl_fun = clos;
                     cl_free_var = set_of_closures.cl_free_var;
                     cl_specialised_arg = spec_args;
                   }, Expr_id.create ());
                fu_fun = fun_id;
                fu_relative_to = None;
              }, Expr_id.create ());
          ap_arg = List.map (fun (id, _) -> Fvar (id, Expr_id.create ())) args;
          ap_kind = Direct fun_id;
          ap_dbg;
        }, Expr_id.create ()) in
    let expr =
      List.fold_left (fun expr (id, arg) ->
          Flet (Not_assigned, id, arg, expr, Expr_id.create ()))
        application args
    in
    loop (disactivate_substitution env) r expr

let inline tree =
  let result, r = loop (Env.empty ()) init_r tree in
  if not (Variable.Set.is_empty r.used_variables)
  then begin
    Format.printf "remaining variables: %a@.%a@."
      Variable.Set.print r.used_variables
      Printflambda.flambda result
  end;
  assert (Variable.Set.is_empty r.used_variables);
  assert (Static_exception.Set.is_empty r.used_staticfail);
  Format.printf "benefit:@ %a@."
    Flambdacost.print_benefit r.benefit;
  result
