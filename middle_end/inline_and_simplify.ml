(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module A = Simple_value_approx
module B = Inlining_cost.Benefit
module E = Inline_and_simplify_aux.Env
module R = Inline_and_simplify_aux.Result

(** Values of two types hold the information propagated during simplification:
    - [E.t] "environments", top-down, almost always called "env";
    - [R.t] "results", bottom-up approximately following the evaluation order,
      almost always called "r".  These results come along with rewritten
      Flambda terms.
    The environments map variables to approximations, which enable various
    simplifications to be performed; for example, some variable may be known
    to always hold a particular constant.
*)

let ret = R.set_approx

let freshen_and_simplify_variable env var =
  let var = Freshening.apply_variable (E.freshening env) var in
  let var_opt =
    (* This squashes aliases of the form:
        let var1 = var2 in ... var2 ...
       by replacing [var2] in the body with [var1].  Simplification can then
       eliminate the [let].
    *)
    A.simplify_var_to_var_using_env (E.find_exn env var)
      ~is_present_in_env:(fun var -> E.mem env var)
  in
  match var_opt with
  | None -> var
  | Some var -> var

let simplify_named_using_approx r lam approx =
  let lam, _summary, approx = A.simplify_named approx lam in
  lam, R.set_approx r approx

let simplify_using_approx_and_env env r original_lam approx =
  let lam, summary, approx =
    A.simplify_using_env approx ~is_present_in_env:(E.mem env) original_lam
  in
  let r =
    let r = ret r approx in
    match summary with
    | Replaced_term -> R.map_benefit r (B.remove_code original_lam)
    | Nothing_done -> r
  in
  lam, r

let simplify_named_using_approx_and_env env r original_named approx =
  let named, summary, approx =
    A.simplify_named_using_env approx ~is_present_in_env:(E.mem env)
      original_named
  in
  let r =
    let r = ret r approx in
    match summary with
    | Replaced_term -> R.map_benefit r (B.remove_code_named original_named)
    | Nothing_done -> r
  in
  named, r

(* This adds only the minimal set of approximations to the closures.
   It is not strictly necessary to have this restriction, but it helps
   to catch potential substitution bugs. *)
let populate_closure_approximations
      ~(function_decl : Flambda.function_declaration)
      ~(free_vars : (_ * A.t) Variable.Map.t)
      ~(parameter_approximations : A.t Variable.Map.t)
      ~set_of_closures_env =
  (* Add approximations of used free variables *)
  let env =
    Variable.Map.fold (fun id (_, desc) env ->
       if Variable.Set.mem id function_decl.free_variables
       then E.add env id desc
       else env) free_vars set_of_closures_env in
  (* Add known approximations of function parameters *)
  let env =
    List.fold_left (fun env id ->
       let approx = try Variable.Map.find id parameter_approximations
                    with Not_found -> A.value_unknown in
       E.add env id approx)
      env function_decl.params in
  env

let debug_free_variables_check env tree ~name ~calculate_free_variables
      ~printer =
  (* CR mshinwell: add compiler option to enable this expensive check *)
  let fv = calculate_free_variables tree in
  Variable.Set.iter (fun var ->
      let var = Freshening.apply_variable (E.freshening env) var in
      match Inline_and_simplify_aux.Env.find_opt env var with
      | Some _ -> ()
      | None ->
        Misc.fatal_errorf "Unbound variable(s) (%s): %a fv=%a env=%a %s\n"
          name
          printer tree
          Variable.Set.print fv
          Inline_and_simplify_aux.Env.print env
          (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int)))
    fv

(* Determine whether a given closure ID corresponds directly to a variable
   (bound to a closure) in the given environment.  This happens when the body
   of a [let rec]-bound function refers to another in the same set of closures.
   If we succeed in this process, we can change [Project_closure]
   expressions into [Var] expressions, thus sharing closure projections. *)
let reference_recursive_function_directly env closure_id =
  let closure_id = Closure_id.unwrap closure_id in
  match E.find_opt env closure_id with
  | None -> None
  | Some approx -> Some (Flambda.Expr (Var closure_id), approx)

(* Simplify an expression that takes a set of closures and projects an
   individual closure from it. *)
let simplify_project_closure env r ~(project_closure : Flambda.project_closure)
      : Flambda.named * R.t =
  let set_of_closures =
    Freshening.apply_variable (E.freshening env)
      project_closure.set_of_closures
  in
  let set_of_closures_approx = E.find_exn env set_of_closures in
  match A.check_approx_for_set_of_closures set_of_closures_approx with
  | Wrong ->
    Misc.fatal_errorf "Wrong approximation when projecting closure: %a"
      Flambda_printers.project_closure project_closure
  | Unresolved symbol ->
    (* A set of closures coming from another compilation unit, whose .cmx is
       missing; as such, we cannot have rewritten the function and don't
       need to do any freshening. *)
    Project_closure {
      set_of_closures;
      closure_id = project_closure.closure_id;
    }, ret r (A.value_unresolved symbol)
  | Ok (set_of_closures_var, value_set_of_closures) ->
    let closure_id =
      A.freshen_and_check_closure_id value_set_of_closures
        project_closure.closure_id
    in
    match reference_recursive_function_directly env closure_id with
    | Some (flam, approx) -> flam, ret r approx
    | None ->
      let approx =
        A.value_closure ?set_of_closures_var value_set_of_closures
          closure_id
      in
      Project_closure { set_of_closures; closure_id; }, ret r approx

(* Simplify an expression that, given one closure within some set of
   closures, returns another closure (possibly the same one) within the
   same set. *)
let simplify_move_within_set_of_closures env r
      ~(move_within_set_of_closures : Flambda.move_within_set_of_closures)
      : Flambda.named * R.t =
  let closure =
    Freshening.apply_variable (E.freshening env)
      move_within_set_of_closures.closure
  in
  let closure_approx = E.find_exn env closure in
  match A.check_approx_for_closure closure_approx with
  | Wrong ->
    Misc.fatal_errorf "Wrong approximation when moving within set of \
        closures: %a"
      Flambda_printers.move_within_set_of_closures move_within_set_of_closures
  | Ok (_value_closure, set_of_closures_var, value_set_of_closures) ->
    let freshen =
      (* CR mshinwell: potentially misleading name---not freshening with new
         names, but with previously fresh names *)
      A.freshen_and_check_closure_id value_set_of_closures
    in
    let move_to = freshen move_within_set_of_closures.move_to in
    match reference_recursive_function_directly env move_to with
    | Some (flam, approx) -> flam, ret r approx
    | None ->
      let start_from = freshen move_within_set_of_closures.start_from in
      if Closure_id.equal start_from move_to then
        (* Moving from one closure to itself is a no-op.  We can return an
           [Var] since we already have a variable bound to the closure. *)
        Expr (Var closure), ret r closure_approx
      else
        match set_of_closures_var with
        | Some set_of_closures_var ->
          (* A variable bound to the set of closures is in scope, meaning we
             can rewrite the [Move_within_set_of_closures] to a
             [Project_closure]. *)
          (* CR mshinwell: does [set_of_closures_var] need freshening? *)
          let project_closure : Flambda.project_closure =
            { set_of_closures = set_of_closures_var;
              closure_id = move_to;
            }
          in
          let approx =
            A.value_closure ~set_of_closures_var value_set_of_closures move_to
          in
          Project_closure project_closure, ret r approx
        | None ->
          (* The set of closures is not available in scope, and we have no
             other information by which to simplify the move. *)
          let move_within : Flambda.move_within_set_of_closures =
            { closure; start_from; move_to; }
          in
          let approx = A.value_closure value_set_of_closures move_to in
          Move_within_set_of_closures move_within, ret r approx

(* Transform an expression denoting an access to a variable bound in
   a closure.  Variables in the closure ([project_var.closure]) may
   have been freshened since [expr] was constructed; as such, we
   must ensure the same happens to [expr].  The renaming information is
   contained within the approximation deduced from [closure] (as
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

     [Project_var{closure = closure; closure_id = g; var = a}]

   If [f] is inlined later, the resulting code will be

     [let x = ... in
      let g' y' ~closure':{a'} = a' + y' in
      let closure' = { a' = x } in
        closure'.a' + 12]

   in particular the field [a] of the closure has been alpha renamed to [a'].
   This information must be carried from the declaration to the use.

   If the function is declared outside of the alpha renamed part, there is
   no need for renaming in the [Ffunction] and [Project_var].
   This is not usualy the case, except when the closure declaration is a
   symbol.

   What ensures that this information is available at [Project_var]
   point is that those constructions can only be introduced by inlining,
   which requires that same information. For this to still be valid,
   other transformation must avoid transforming the information flow in
   a way that the inline function can't propagate it.
*)
let rec simplify_project_var env r ~(project_var : Flambda.project_var)
      : Flambda.named * R.t =
  let closure =
    Freshening.apply_variable (E.freshening env) project_var.closure
  in
  let approx = E.find_exn env closure in
  match A.check_approx_for_closure_allowing_unresolved approx with
  | Ok (value_closure, _set_of_closures_var, value_set_of_closures) ->
    let module F = Freshening.Project_var in
    let freshening = value_set_of_closures.freshening in
    let var = F.apply_var_within_closure freshening project_var.var in
    let closure_id = F.apply_closure_id freshening project_var.closure_id in
    let closure_id_in_approx = value_closure.closure_id in
    if not (Closure_id.equal closure_id closure_id_in_approx) then begin
      Misc.fatal_errorf "When simplifying [Project_var], the closure ID %a in \
          the approximation of the set of closures did not match the closure \
          ID %a in the [Project_var] term.  Approximation: %a@. \
          Var-within-closure being projected: %a@."
        Closure_id.print closure_id_in_approx
        Closure_id.print closure_id
        Simple_value_approx.print approx
        Var_within_closure.print var
    end;
    let approx = A.approx_for_bound_var value_set_of_closures var in
    let expr : Flambda.named = Project_var { closure; closure_id; var; } in
    simplify_named_using_approx_and_env env r expr approx
  | Unresolved symbol ->
    (* This value comes from a symbol for which we couldn't find any
       approximation, telling us that names within the closure couldn't
       have been renamed.  So we don't need to change the variable or
       closure ID in the [Project_var] expression. *)
    Project_var { project_var with closure }, ret r (A.value_unresolved symbol)
  | Wrong ->
    (* We must have the correct approximation of the value to ensure
       we take account of all freshenings. *)
    Misc.fatal_errorf "[Project_var] from a value with wrong \
        approximation: %a@.%a@.%a@."
      Flambda_printers.project_var project_var
      Variable.print closure
      Simple_value_approx.print approx

(* Transforms closure definitions by applying [loop] on the code of every
   one of the set and on the expressions of the free variables.
   If the substitution is activated, alpha renaming also occur on everything
   defined by the set of closures:
   * Variables bound by a closure of the set
   * closure identifiers
   * parameters

   The rewriting occurs in a clean environment without any of the variables
   defined outside reachable.  This helps increase robustness against accidental,
   potentially unsound simplification of variable accesses by
   [simplify_using_approx_and_env].

   The rewriting occurs in an environment filled with:
   * The approximation of the free variables
   * An explicitely unknown approximation for function parameters,
     except for those where it is known to be safe: those present in the
     [specialised_args] set.
   * An approximation for the closures in the set. It contains the code of
     the functions before rewriting.

   The approximation of the currently defined closures is available to
   allow marking recursives calls as direct and in some cases, allow
   inlining of one closure from the set inside another one. For this to
   be correct an alpha renaming is first applied on the expressions by
   [apply_function_decls_and_free_vars].

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
and simplify_set_of_closures original_env r
      (set_of_closures : Flambda.set_of_closures) : Flambda.named * R.t =
  let function_decls =
    let module Backend = (val (E.backend original_env) : Backend_intf.S) in
    (* CR mshinwell: Does this affect
       [reference_recursive_function_directly]? *)
    Freshening.rewrite_recursive_calls_with_symbols (E.freshening original_env)
      set_of_closures.function_decls
      ~make_closure_symbol:Backend.closure_symbol
  in
  let env = E.increase_closure_depth original_env in
  let free_vars =
    Variable.Map.map (fun external_var ->
        let external_var = freshen_and_simplify_variable env external_var in
        external_var, E.find_exn env external_var)
      set_of_closures.free_vars
  in
  let specialised_args =
    Variable.Map.map (freshen_and_simplify_variable env)
      set_of_closures.specialised_args
  in
  let environment_before_cleaning = env in
  (* [E.local] helps us to catch bugs whereby variables escape their scope. *)
  let env = E.local env in
  let free_vars, function_decls, sb, freshening =
    Freshening.apply_function_decls_and_free_vars (E.freshening env) free_vars
      function_decls
  in
  let env = E.set_freshening env sb in
  let specialised_args =
    Variable.Map.map_keys (freshen_and_simplify_variable env)
      specialised_args
  in
  let parameter_approximations =
    (* Approximations of parameters that are known to always hold the same
       argument throughout the body of the function. *)
    Variable.Map.map_keys (freshen_and_simplify_variable env)
      (Variable.Map.map (fun id -> E.find_exn environment_before_cleaning id)
        specialised_args)
  in
  let env =
    E.enter_set_of_closures_declaration function_decls.set_of_closures_id env
  in
  (* we use the previous closure for evaluating the functions *)
  let internal_value_set_of_closures : A.value_set_of_closures =
    { function_decls = function_decls;
      bound_vars = Variable.Map.fold (fun id (_, desc) map ->
          Var_within_closure.Map.add (Var_within_closure.wrap id) desc map)
          free_vars Var_within_closure.Map.empty;
      unchanging_params = Variable.Set.empty;
      specialised_args = Variable.Map.keys specialised_args;
      freshening;
    }
  in
  (* Populate the environment with the approximation of each closure.
     This part of the environment is shared between all of the closures in
     the set of closures. *)
  let set_of_closures_env =
    Variable.Map.fold (fun closure _ env ->
        let approx =
          A.value_closure ~closure_var:closure internal_value_set_of_closures
            (Closure_id.wrap closure)
        in
        E.add env closure approx)
      function_decls.funs env
  in
  let simplify_function fid (function_decl : Flambda.function_declaration)
        (funs, _used_params, r)
        (* CR mshinwell: check we really did not need _used_params, and
           remove it *)
        : Flambda.function_declaration Variable.Map.t * Variable.Set.t * R.t =
    let closure_env =
      populate_closure_approximations ~function_decl ~free_vars
        ~parameter_approximations ~set_of_closures_env
    in
    let body, r =
      E.enter_closure closure_env ~closure_id:(Closure_id.wrap fid)
        ~inline_inside:
          (Inlining_decision.should_inline_inside_declaration function_decl)
        ~where:Transform_set_of_closures_expression
        ~f:(fun body_env ->
          simplify body_env r function_decl.body)
    in
    let free_variables = Free_variables.calculate body in
    let used_params =
      Variable.Set.filter (fun param -> Variable.Set.mem param free_variables)
        (Variable.Set.of_list function_decl.params)
    in
    Variable.Map.add fid { function_decl with body; free_variables } funs,
      used_params, r
  in
  let funs, used_params, r =
    Variable.Map.fold simplify_function function_decls.funs
      (Variable.Map.empty, Variable.Set.empty, r)
  in
  let specialised_args =
    (* Remove any specialised arguments whose parameters are unused. *)
    Variable.Map.filter (fun id _ -> Variable.Set.mem id used_params)
      specialised_args
  in
  let function_decls = { function_decls with funs } in
  let unchanging_params =
    Invariant_params.unchanging_params_in_recursion function_decls
  in
  let value_set_of_closures : A.value_set_of_closures =
    { internal_value_set_of_closures with
      function_decls; unchanging_params;
    }
  in
  let set_of_closures : Flambda.set_of_closures =
    { function_decls;
      free_vars = Variable.Map.map fst free_vars;
      specialised_args;
    }
  in
  Set_of_closures (set_of_closures),
    ret r (A.value_set_of_closures value_set_of_closures)

and simplify_apply env r ~(apply : Flambda.apply) : Flambda.t * R.t =
  let { Flambda. func = lhs_of_application; args; kind = _; dbg } = apply in
  let lhs_of_application =
    freshen_and_simplify_variable env lhs_of_application
  in
  let args = List.map (freshen_and_simplify_variable env) args in
  let lhs_of_application_approx = E.find_exn env lhs_of_application in
  let args_approxs = List.map (fun arg -> E.find_exn env arg) args in
  (* By using the approximation of the left-hand side of the application,
     attempt to determine which function is being applied (even if the
     application is currently [Indirect]).  If successful---in which case we
     then have a direct application---consider inlining. *)
  match A.check_approx_for_closure lhs_of_application_approx with
  | Ok (value_closure, _set_of_closures_var, value_set_of_closures) ->
    let closure_id_being_applied = value_closure.closure_id in
    let function_decls = value_set_of_closures.function_decls in
    let function_decl =
      try
        Flambda_utils.find_declaration closure_id_being_applied function_decls
      with
      | Not_found ->
        Misc.fatal_errorf "When handling application expression, \
            approximation references non-existent closure %a@."
          Closure_id.print closure_id_being_applied
    in
    let nargs = List.length args in
    let arity = Flambda_utils.function_arity function_decl in
    if nargs = arity then
      simplify_full_application env r ~function_decls ~lhs_of_application
        ~closure_id_being_applied ~function_decl ~value_set_of_closures ~args
        ~args_approxs ~dbg
    else if nargs > arity then
      simplify_over_application env r ~args ~args_approxs ~function_decls
        ~lhs_of_application ~closure_id_being_applied ~function_decl
        ~value_set_of_closures ~dbg
    else if nargs > 0 && nargs < arity then
      simplify_partial_application env r ~lhs_of_application
        ~closure_id_being_applied ~function_decl ~args ~dbg
    else
      Misc.fatal_errorf "Function with arity %d when simplifying \
          application expression: %a"
        arity Flambda_printers.flambda (Flambda.Apply apply)
  | Wrong ->  (* Insufficient approximation information to simplify. *)
    Apply ({ func = lhs_of_application; args; kind = Indirect; dbg }),
      ret r A.value_unknown

and simplify_full_application env r ~function_decls ~lhs_of_application
      ~closure_id_being_applied ~function_decl ~value_set_of_closures ~args
      ~args_approxs ~dbg =
  Inlining_decision.for_call_site ~env ~r ~function_decls
    ~lhs_of_application ~closure_id_being_applied ~function_decl
    ~value_set_of_closures ~args ~args_approxs ~dbg ~simplify

and simplify_partial_application env r ~lhs_of_application
      ~closure_id_being_applied ~function_decl ~args ~dbg =
  let arity = Flambda_utils.function_arity function_decl in
  assert (arity > List.length args);
  let freshened_params =
    List.map (fun id -> Variable.freshen id) function_decl.Flambda.params
  in
  let applied_args, remaining_args =
    Misc.map2_head (fun arg id' -> id', arg) args freshened_params
  in
  let wrapper_accepting_remaining_args =
    let body : Flambda.t =
      Apply {
        func = lhs_of_application;
        args = freshened_params;
        kind = Direct closure_id_being_applied;
        dbg;
      }
    in
    Flambda_utils.make_closure_declaration ~id:(Variable.create "partial_fun")
      ~body
      ~params:remaining_args
  in
  let with_known_args =
    Flambda_utils.bind
      ~bindings:(List.map (fun (var, arg) ->
          var, Flambda.Expr (Var arg)) applied_args)
      ~body:wrapper_accepting_remaining_args
  in
  simplify env r with_known_args

and simplify_over_application env r ~args ~args_approxs ~function_decls
      ~lhs_of_application ~closure_id_being_applied ~function_decl
      ~value_set_of_closures ~dbg =
  let arity = Flambda_utils.function_arity function_decl in
  assert (arity < List.length args);
  assert (List.length args = List.length args_approxs);
  let full_app_args, remaining_args = Misc.split_at arity args in
  let full_app_approxs, _ = Misc.split_at arity args_approxs in
  let expr, r =
    simplify_full_application env r ~function_decls ~lhs_of_application
      ~closure_id_being_applied ~function_decl ~value_set_of_closures
      ~args:full_app_args ~args_approxs:full_app_approxs ~dbg
  in
  let func_var = Variable.create "full_apply" in
  let expr : Flambda.t =
    Let (Immutable, func_var, Expr expr,
      Apply { func = func_var; args = remaining_args; kind = Indirect; dbg })
  in
  simplify env r expr

and simplify_named env r (tree : Flambda.named) : Flambda.named * R.t =
  debug_free_variables_check env tree ~name:"simplify_named"
    ~calculate_free_variables:Free_variables.calculate_named
    ~printer:Flambda_printers.named;
  match tree with
  | Symbol sym ->
    let module Backend = (val (E.backend env) : Backend_intf.S) in
    (* CR mshinwell for pchambart: Is there a reason we cannot use
       [simplify_named_using_approx_and_env] here? *)
    simplify_named_using_approx r tree (Backend.import_symbol sym)
  | Const cst -> tree, ret r (A.const cst)
  | Set_of_closures set_of_closures ->
    simplify_set_of_closures env r set_of_closures
  | Project_closure project_closure ->
    simplify_project_closure env r ~project_closure
  | Project_var project_var -> simplify_project_var env r ~project_var
  | Move_within_set_of_closures move_within_set_of_closures ->
    simplify_move_within_set_of_closures env r ~move_within_set_of_closures
  | Prim (prim, args, dbg) ->
    let args = List.map (freshen_and_simplify_variable env) args in
    let tree = Flambda.Prim (prim, args, dbg) in
    begin match prim, args with
    | Pgetglobal id, [] ->
      let approx =
        if Ident.is_predef_exn id then A.value_unknown
        else
          let module Backend = (val (E.backend env) : Backend_intf.S) in
          Backend.import_global id
      in
      tree, ret r approx
    | Pgetglobalfield (id, i), [] ->
      let approx =
        if id = Compilation_unit.get_current_id_exn ()
        then R.find_global r ~field_index:i
        else
          let module Backend = (val (E.backend env) : Backend_intf.S) in
          A.get_field (Backend.import_global id) ~field_index:i
      in
      (* CR mshinwell for pchambart: Is there a reason we cannot use
         [simplify_named_using_approx_and_env] here? *)
      simplify_named_using_approx r tree approx
    | Psetglobalfield (_, i), [arg] ->
      let approx = E.find_exn env arg in
      let r = R.add_global r ~field_index:i ~approx in
      tree, ret r A.value_unknown
    | Pfield i, [arg] ->
      let approx = A.get_field (E.find_exn env arg) ~field_index:i in
      simplify_named_using_approx_and_env env r tree approx
    | (Psetfield _ | Parraysetu _ | Parraysets _), block::_ ->
      let block_approx = E.find_exn env block in
      if A.is_definitely_immutable block_approx then begin
        Location.prerr_warning (Debuginfo.to_location dbg)
          Warnings.Assignment_on_non_mutable_value
      end;
      tree, ret r A.value_unknown
    | (Psequand | Psequor), _ ->
      Misc.fatal_error "Psequand and Psequor must be expanded (see handling \
          in closure_conversion.ml)"
    | p, args ->
      let approxs = E.find_list_exn env args in
      let expr, approx, benefit =
        let module Backend = (val (E.backend env) : Backend_intf.S) in
        Simplify_primitives.primitive p (args, approxs) tree dbg
          ~size_int:Backend.size_int ~big_endian:Backend.big_endian
      in
      let r = R.map_benefit r (B.(+) benefit) in
      expr, ret r approx
    end
  | Expr expr ->
    let expr, r = simplify_direct env r expr in
    Expr expr, r

and simplify_direct env r (tree : Flambda.t) : Flambda.t * R.t =
  debug_free_variables_check env tree ~name:"loop"
    ~calculate_free_variables:Free_variables.calculate
    ~printer:Flambda_printers.flambda;
  match tree with
  | Var var ->
    let var = freshen_and_simplify_variable env var in
    simplify_using_approx_and_env env r (Var var) (E.find_exn env var)
  | Apply apply -> simplify_apply env r ~apply
  | Let (str, id, defining_expr, body) ->
    let defining_expr, r = simplify_named env r defining_expr in
    (* When [defining_expr] is really a [Flambda.named] rather than an
       [Flambda.t], squash any intermediate [let], or we will never eliminate
       certain cases (e.g. when a variable is simplified to a constant). *)
    let defining_expr =
      match defining_expr with
      | Expr (Let (Immutable, var1, defining_expr, Var var2))
          when Variable.equal var1 var2 -> defining_expr
      | _ -> defining_expr
    in
    let id, sb = Freshening.add_variable (E.freshening env) id in
    let env = E.set_freshening env sb in
    let body, r =
      let approx_of_bound_var =
        match str with
        | Immutable -> R.approx r
        | Mutable -> A.value_unknown
      in
      simplify (E.add env id approx_of_bound_var) r body
    in
    let free_variables_of_body = Free_variables.calculate body in
    let (expr : Flambda.t), r =
      if Variable.Set.mem id free_variables_of_body then
        Flambda.Let (str, id, defining_expr, body), r
      else if Effect_analysis.no_effects_named defining_expr then
        let r = R.map_benefit r (B.remove_code_named defining_expr) in
        body, r
      else
        (* CR mshinwell: check that Pignore is inserted correctly by a later
           pass. *)
        (* Generate a fresh name for increasing legibility of the
           intermediate language (in particular to make it more obvious that
           the variable is unused). *)
        let fresh_var = Variable.create "for_side_effect_only" in
        Flambda.Let (Immutable, fresh_var, defining_expr, body), r
    in
    expr, r
  | Let_rec (defs, body) ->
    let defs, sb = Freshening.add_variables (E.freshening env) defs in
    let env = E.set_freshening env sb in
    let def_env =
      List.fold_left (fun env_acc (id, _lam) ->
          E.add env_acc id A.value_unknown)
        env defs
    in
    let defs, body_env, r =
      List.fold_right (fun (id, lam) (defs, env_acc, r) ->
          let lam, r = simplify_named def_env r lam in
          let defs = (id, lam) :: defs in
          let env_acc = E.add env_acc id (R.approx r) in
          defs, env_acc, r)
        defs ([], env, r)
    in
    let body, r = simplify body_env r body in
    Let_rec (defs, body), r
  | Static_raise (i, args) ->
    let i = Freshening.apply_static_exception (E.freshening env) i in
    let args, _, r = simplify_list env r args in
    let r = R.use_staticfail r i in
    Static_raise (i, args), ret r A.value_bottom
  | Static_catch (i, vars, body, handler) ->
    let i, sb = Freshening.add_static_exception (E.freshening env) i in
    let env = E.set_freshening env sb in
    let body, r = simplify env r body in
    if not (Static_exception.Set.mem i (R.used_staticfail r)) then
      (* If the static exception is not used, we can drop the declaration *)
      body, r
    else begin
      match (body : Flambda.t) with
      | Static_raise (j, args) when
          Static_exception.equal i
            (Freshening.apply_static_exception (E.freshening env) j) ->
        (* This is usually true, since whe checked that the static
           exception was used.  The only case where it can be false
           is when an argument can raise.  This could be avoided if
           all arguments where guaranteed to be variables. *)
        let handler =
          List.fold_left2 (fun body var arg ->
              Flambda.Let (Immutable, var, Flambda.Expr arg, body))
            handler vars args
        in
        let r = R.exit_scope_catch r i in
        simplify env r handler
      | _ ->
        let vars, sb = Freshening.add_variables' (E.freshening env) vars in
        let env =
          List.fold_left (fun env id -> E.add env id A.value_unknown)
            (E.set_freshening env sb) vars
        in
        let env = E.inside_branch env in
        let handler, r = simplify env r handler in
        let r = R.exit_scope_catch r i in
        Static_catch (i, vars, body, handler), ret r A.value_unknown
    end
  | Try_with (body, id, handler) ->
    let body, r = simplify env r body in
    let id, sb = Freshening.add_variable (E.freshening env) id in
    let env = E.add (E.set_freshening env sb) id A.value_unknown in
    let env = E.inside_branch env in
    let handler, r = simplify env r handler in
    Try_with (body, id, handler), ret r A.value_unknown
  | If_then_else (arg, ifso, ifnot) ->
    (* When arg is the constant false or true (or something considered
       as true), we can drop the if and replace it by a sequence.
       if arg is not effectful we can also drop it. *)
    let arg = freshen_and_simplify_variable env arg in
    begin match (R.approx r).descr with
    | Value_constptr 0 ->  (* Constant [false]: keep [ifnot] *)
      let ifnot, r = simplify env r ifnot in
      ifnot, R.map_benefit r B.remove_branch
    | Value_constptr _ | Value_block _ ->  (* Constant [true]: keep [ifso] *)
      let ifso, r = simplify env r ifso in
      ifso, R.map_benefit r B.remove_branch
    | _ ->
      let env = E.inside_branch env in
      let ifso, r = simplify env r ifso in
      let ifso_approx = R.approx r in
      let ifnot, r = simplify env r ifnot in
      let ifnot_approx = R.approx r in
      If_then_else (arg, ifso, ifnot), ret r (A.meet ifso_approx ifnot_approx)
    end
  | While (cond, body) ->
    let cond, r = simplify env r cond in
    let env = E.inside_simplify env in
    let body, r = simplify env r body in
    While (cond, body), ret r A.value_unknown
  | Send { kind; meth; obj; args; dbg; } ->
    let meth = freshen_and_simplify_variable env meth in
    let obj = freshen_and_simplify_variable env obj in
    let args = List.map (freshen_and_simplify_variable env) args in
    Send { kind; meth; obj; args; dbg; }, ret r A.value_unknown
  | For { bound_var; from_value; to_value; direction; body; } ->
    let from_value = freshen_and_simplify_variable env from_value in
    let to_value = freshen_and_simplify_variable env to_value in
    let bound_var, sb = Freshening.add_variable (E.freshening env) bound_var in
    let env =
      E.inside_simplify
        (E.add (E.set_freshening env sb) bound_var A.value_unknown)
    in
    let env = E.inside_simplify env in
    let body, r = simplify env r body in
    For { bound_var; from_value; to_value; direction; body; },
      ret r A.value_unknown
  | Assign { being_assigned; new_value; } ->
    let being_assigned = freshen_and_simplify_variable env being_assigned in
    let new_value = freshen_and_simplify_variable env new_value in
    Assign { being_assigned; new_value; }, ret r A.value_unknown
  | Switch (arg, sw) ->
    (* When [arg] is known to be a variable whose approximation is that of a
       block with a fixed tag or a fixed integer, we can eliminate the
       [Switch].  (This should also make the [Let] that binds [arg] redundant,
       meaning that it too can be eliminated.) *)
    let arg = freshen_and_simplify_variable env arg in
    let get_failaction () : Flambda.t =
      (* If the switch is applied to a statically-known value that does not
         match any case:
         * if there is a default action take that case;
         * otherwise this is something that is guaranteed not to
           be reachable by the type checker.  For example:
           [type 'a t = Int : int -> int t | Float : float -> float t
            match Int 1 with
            | Int _ -> ...
            | Float f as v ->
              match v with   <-- This match is unreachable
              | Float f -> ...]
       *)
      match sw.failaction with
      | None -> Proved_unreachable
      | Some f -> f
    in
    begin match (R.approx r).descr with
    | Value_int i
    | Value_constptr i ->
      let lam =
        try List.assoc i sw.consts
        with Not_found -> get_failaction ()
      in
      let lam, r = simplify env r lam in
      lam, R.map_benefit r B.remove_branch
    | Value_block (tag, _) ->
      let tag = Tag.to_int tag in
      let lam =
        try List.assoc tag sw.blocks
        with Not_found -> get_failaction ()
      in
      let lam, r = simplify env r lam in
      lam, R.map_benefit r B.remove_branch
    | _ ->
      let env = E.inside_branch env in
      let f (i, v) (acc, r) =
        let approx = R.approx r in
        let lam, r = simplify env r v in
        ((i, lam)::acc, R.set_approx r (A.meet (R.approx r) approx))
      in
      let r = R.set_approx r A.value_bottom in
      let consts, r = List.fold_right f sw.consts ([], r) in
      let blocks, r = List.fold_right f sw.blocks ([], r) in
      let failaction, r =
        match sw.failaction with
        | None -> None, r
        | Some l ->
          let approx = R.approx r in
          let l, r = simplify env r l in
          Some l, R.set_approx r (A.meet (R.approx r) approx)
      in
      let sw = { sw with failaction; consts; blocks; } in
      Switch (arg, sw), r
    end
  | String_switch (arg, sw, def) ->
    let arg = freshen_and_simplify_variable env arg in
    let sw, r =
      List.fold_right (fun (str, lam) (sw, r) ->
          let lam, r = simplify env r lam in
          (str, lam)::sw, r)
        sw
        ([], r)
    in
    let def, r =
      match def with
      | None -> def, r
      | Some def ->
        let def, r = simplify env r def in
        Some def, r
    in
    String_switch (arg, sw, def), ret r A.value_unknown
  | Proved_unreachable -> tree, ret r A.value_bottom

and simplify_list env r l =
  match l with
  | [] -> [], [], r
  | h::t ->
    let t', approxs, r = simplify_list env r t in
    let h', r = simplify env r h in
    let approxs = (R.approx r) :: approxs in
    if t' == t && h' == h
    then l, approxs, r
    else h' :: t', approxs, r

and simplify env r tree =
  let f, r = simplify_direct env r tree in
  let module Backend = (val (E.backend env) : Backend_intf.S) in
  (* CR mshinwell for pchambart: This call to [really_import_approx] is
     kind of confusing; it seems like some kind of catch-all.  What
     exactly is happening here? *)
  f, ret r (Backend.really_import_approx (R.approx r))

(* CR mshinwell for pchambart: Change to a "-dinlining-benefit" option? *)
let debug_benefit =
  try ignore (Sys.getenv "BENEFIT"); true
  with _ -> false

let run ~never_inline ~backend tree =
  let r =
    if never_inline then
      R.set_inlining_threshold (R.create ()) Inlining_cost.Never_inline
    else
      R.create ()
  in
  let stats = !Clflags.inlining_stats in
  if never_inline then Clflags.inlining_stats := false;
  let env = E.create ~never_inline:false ~backend in
  let result, r = simplify env r tree in
  Clflags.inlining_stats := stats;
  if not (Static_exception.Set.is_empty (R.used_staticfail r))
  then begin
    Misc.fatal_error (Format.asprintf "remaining static exceptions: %a@.%a@."
      Static_exception.Set.print (R.used_staticfail r)
      Flambda_printers.flambda result)
  end;
  assert (Static_exception.Set.is_empty (R.used_staticfail r));
  if debug_benefit then
    Format.printf "benefit:@ %a@."
      B.print (R.benefit r);
  result
