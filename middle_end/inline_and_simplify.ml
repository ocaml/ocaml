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

type simplify_variable_result =
  | No_binding of Variable.t
  | Binding of Variable.t * Flambda.named

let simplify_free_variable_internal env original_var =
  let var = Freshening.apply_variable (E.freshening env) original_var in
  let original_var = var in
  (* In the case where an approximation is useful, we introduce a [let]
     to bind (e.g.) the constant or symbol replacing [var], unless this
     would introduce a useless [let] as a consequence of [var] already being
     in the current scope.

     Even when the approximation is not useful, this simplification helps.
     In particular, it squashes aliases of the form:
      let var1 = var2 in ... var2 ...
     by replacing [var2] in the body with [var1].  Simplification can then
     eliminate the [let].
  *)
  let var_opt =
    (* CR mshinwell: consider shuffling this logic around a bit *)
    A.simplify_var_to_var_using_env (E.find_exn env var)
      ~is_present_in_env:(fun var -> E.mem env var)
  in
  let var =
    match var_opt with
    | None -> var
    | Some var -> var
  in
  (* CR mshinwell: Should we update [r] when we *add* code?
     Aside from that, it looks like maybe we don't need [r] in this function,
     because the approximation within it wouldn't be used by any of the
     call sites. *)
  match E.find_with_scope_exn env var with
  | Current, _approx -> No_binding var  (* avoid useless [let] *)
  | Outer, approx ->
    match A.simplify_var approx with
    | None -> No_binding var
    | Some (named, _approx) -> Binding (original_var, named)

let simplify_free_variable env var ~f : Flambda.t * R.t =
  match simplify_free_variable_internal env var with
  | No_binding var -> f env var
  | Binding (var, named) ->
    let approx = E.find_exn env var in
    let var = Variable.rename var in
    let env = E.add env var approx in
    let body, r = f env var in
    (Flambda.create_let var named body), r

let simplify_free_variables env vars ~f : Flambda.t * R.t =
  let rec collect_bindings vars env bound_vars : Flambda.t * R.t =
    match vars with
    | [] -> f env (List.rev bound_vars)
    | var::vars ->
      match simplify_free_variable_internal env var with
      | No_binding var ->
        collect_bindings vars env (var::bound_vars)
      | Binding (var, named) ->
        let approx = E.find_exn env var in
        let var = Variable.rename var in
        let env = E.add env var approx in
        let body, r = collect_bindings vars env (var::bound_vars) in
        (Flambda.create_let var named body), r
  in
  collect_bindings vars env []

let simplify_free_variables_named env vars ~f : Flambda.named * R.t =
  let rec collect_bindings vars env bound_vars : Flambda.t * R.t =
    match vars with
    | [] ->
      let named, r = f env (List.rev bound_vars) in
      Flambda_utils.name_expr named, r
    | var::vars ->
      match simplify_free_variable_internal env var with
      | No_binding var -> collect_bindings vars env (var::bound_vars)
      | Binding (var, named) ->
        let approx = E.find_exn env var in
        let var = Variable.rename var in
        let env = E.add env var approx in
        let body, r = collect_bindings vars env (var::bound_vars) in
        (Flambda.create_let var named body), r
  in
  let expr, r = collect_bindings vars env [] in
  Expr expr, r

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
    (* CR mshinwell: Why is [r] not updated with the cost of adding the
       new code? *)
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
       then E.add_outer_scope env id desc
       else env) free_vars set_of_closures_env in
  (* Add known approximations of function parameters *)
  let env =
    List.fold_left (fun env id ->
       let approx = try Variable.Map.find id parameter_approximations
                    with Not_found -> A.value_unknown in
       E.add env id approx)
      env function_decl.params in
  env

let simplify_const (const : Flambda.const) =
  match const with
  | Int i -> A.value_int i
  | Char c -> A.value_char c
  | Const_pointer i -> A.value_constptr i

(* CR mshinwell: function name is misleading, it only computes an approx *)
let approx_for_allocated_const (const : Allocated_const.t) =
  match const with
  | String s -> A.value_string (String.length s) None
  | Int32 i -> A.value_boxed_int Int32 i
  | Int64 i -> A.value_boxed_int Int64 i
  | Nativeint i -> A.value_boxed_int Nativeint i
  | Float f -> A.value_float f
  | Float_array a -> A.value_float_array (List.length a)
  | Immstring s -> A.value_string (String.length s) (Some s)

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
      Flambda.print_project_closure project_closure
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
      Flambda.print_move_within_set_of_closures move_within_set_of_closures
  | Ok (_value_closure, set_of_closures_var, set_of_closures_symbol, value_set_of_closures) ->
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
        | Some set_of_closures_var when E.mem env set_of_closures_var ->
          (* A variable bound to the set of closures is in scope, meaning we
             can rewrite the [Move_within_set_of_closures] to a
             [Project_closure]. *)
          (* CR mshinwell: does [set_of_closures_var] need freshening?
             before the [E.mem] test above? *)
          let project_closure : Flambda.project_closure =
            { set_of_closures = set_of_closures_var;
              closure_id = move_to;
            }
          in
          let approx =
            A.value_closure ~set_of_closures_var value_set_of_closures move_to
          in
          Project_closure project_closure, ret r approx
        | Some _ | None ->
          match set_of_closures_symbol with
          | Some set_of_closures_symbol ->
            let set_of_closures_var = Variable.create "symbol" in
            let project_closure : Flambda.project_closure =
              { set_of_closures = set_of_closures_var;
                closure_id = move_to;
              }
            in
            let project_closure_var = Variable.create "project_closure" in
            let let1 =
              Flambda.create_let project_closure_var
                (Project_closure project_closure)
                (Var project_closure_var)
            in
            let expr =
              Flambda.create_let set_of_closures_var
                (Symbol set_of_closures_symbol)
                let1
            in
            let approx =
              A.value_closure ~set_of_closures_var ~set_of_closures_symbol
                value_set_of_closures move_to
            in
            Expr expr, ret r approx
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
  | Ok (value_closure, _set_of_closures_var, _set_of_closures_symbol,
        value_set_of_closures) ->
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
        approximation: %a@.closure=%a@.approx of closure=%a@."
      Flambda.print_project_var project_var
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
      (set_of_closures : Flambda.set_of_closures)
      : Flambda.set_of_closures * R.t =
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
        let external_var =
          Freshening.apply_variable (E.freshening env) external_var
        in
        external_var, E.find_exn env external_var)
      set_of_closures.free_vars
  in
  let specialised_args =
    Variable.Map.map (Freshening.apply_variable (E.freshening env))
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
    Variable.Map.map_keys (Freshening.apply_variable (E.freshening env))
      specialised_args
  in
  let parameter_approximations =
    (* Approximations of parameters that are known to always hold the same
       argument throughout the body of the function. *)
    Variable.Map.map_keys (Freshening.apply_variable (E.freshening env))
      (Variable.Map.mapi (fun _id' id ->
          E.find_exn environment_before_cleaning id)
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
        E.add env closure approx
      )
      function_decls.funs env
  in
  let simplify_function fid (function_decl : Flambda.function_declaration)
        (funs, used_params, r)
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
        ~f:(fun body_env -> simplify body_env r function_decl.body)
    in
    let function_decl =
      Flambda.create_function_declaration ~params:function_decl.params
        ~body ~stub:function_decl.stub ~dbg:function_decl.dbg
    in
    let used_params' = Flambda.used_params function_decl in
    Variable.Map.add fid function_decl funs,
      Variable.Set.union used_params used_params', r
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
  let function_decls =
    Flambda.update_function_declarations function_decls ~funs
  in
  let unchanging_params =
    Invariant_params.unchanging_params_in_recursion function_decls
      ~backend:(E.backend env)
  in
  let value_set_of_closures : A.value_set_of_closures =
    { internal_value_set_of_closures with
      function_decls; unchanging_params;
    }
  in
  let set_of_closures =
    Flambda.create_set_of_closures ~function_decls
      ~free_vars:(Variable.Map.map fst free_vars)
      ~specialised_args
  in
  set_of_closures, ret r (A.value_set_of_closures value_set_of_closures)

and simplify_apply env r ~(apply : Flambda.apply) : Flambda.t * R.t =
  let { Flambda. func = lhs_of_application; args; kind = _; dbg } = apply in
  simplify_free_variable env lhs_of_application ~f:(fun env lhs_of_application ->
    simplify_free_variables env args ~f:(fun env args ->
      let lhs_of_application_approx = E.find_exn env lhs_of_application in
      let args_approxs = List.map (fun arg -> E.find_exn env arg) args in
      (* By using the approximation of the left-hand side of the application,
         attempt to determine which function is being applied (even if the
         application is currently [Indirect]).  If successful---in which case we
         then have a direct application---consider inlining. *)
      match A.check_approx_for_closure lhs_of_application_approx with
      | Ok (value_closure, _set_of_closures_var,
            _set_of_closures_symbol, value_set_of_closures) ->
        let closure_id_being_applied = value_closure.closure_id in
        let function_decls = value_set_of_closures.function_decls in
        let function_decl =
          try
            Flambda_utils.find_declaration closure_id_being_applied
              function_decls
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
            ~closure_id_being_applied ~function_decl ~value_set_of_closures
            ~args ~args_approxs ~dbg
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
            arity Flambda.print (Flambda.Apply apply)
      | Wrong ->  (* Insufficient approximation information to simplify. *)
        Apply ({ func = lhs_of_application; args; kind = Indirect; dbg }),
          ret r A.value_unknown))

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
    Flambda.create_let func_var (Expr expr)
      (Apply { func = func_var; args = remaining_args; kind = Indirect; dbg })
  in
  simplify env r expr

and simplify_named env r (tree : Flambda.named) : Flambda.named * R.t =
  match tree with
  | Symbol sym ->
    (* New Symbol construction could have been introduced during
       transformation (by simplify_named_using_approx_and_env).
       When this comes from another compilation unit, we must load it. *)
    let approx = E.find_or_load_symbol env sym in
    (* CR mshinwell for mshinwell: Check this is the correct simplification
       function to use *)
    simplify_named_using_approx r tree approx
  | Const cst -> tree, ret r (simplify_const cst)
  | Allocated_const cst -> tree, ret r (approx_for_allocated_const cst)
  | Read_mutable mut_var ->
    (* See comment on the [Assign] case. *)
    let mut_var =
      Freshening.apply_mutable_variable (E.freshening env) mut_var
    in
    Read_mutable mut_var, ret r A.value_unknown
  | Read_symbol_field (symbol, field_index) ->
    let approx =
      A.augment_with_symbol_field
        (A.get_field (E.find_or_load_symbol env symbol) ~field_index)
        symbol field_index
    in
    simplify_named_using_approx_and_env env r tree approx
  | Set_of_closures set_of_closures ->
    let set_of_closures, r =
      simplify_set_of_closures env r set_of_closures
    in
    Set_of_closures set_of_closures, r
  | Project_closure project_closure ->
    simplify_project_closure env r ~project_closure
  | Project_var project_var -> simplify_project_var env r ~project_var
  | Move_within_set_of_closures move_within_set_of_closures ->
    simplify_move_within_set_of_closures env r ~move_within_set_of_closures
  | Prim (prim, args, dbg) ->
    simplify_free_variables_named env args ~f:(fun env args ->
      let tree = Flambda.Prim (prim, args, dbg) in
      begin match prim, args with
      | Pgetglobal _, _ ->
        Misc.fatal_error "Pgetglobal is forbidden in Inline_and_simplify"
      | Pfield field_index, [arg] ->
        let tree, approx =
          let approx = E.find_exn env arg in
          begin match approx.symbol with
          (* If the [Pfield] is projecting directly from a symbol, rewrite the
             expression to [Read_symbol_field]. *)
          | Some (symbol, None) ->
            let approx =
              A.augment_with_symbol_field
                (A.get_field approx ~field_index)
                symbol field_index
            in
            Flambda.Read_symbol_field (symbol, field_index), approx
          | None | Some (_, Some _ ) ->
            (* This [Pfield] is either not projecting from a symbol at all, or
               it is the projection of a projection from a symbol. *)
            let approx = A.get_field approx ~field_index in
            tree, approx
          end
        in
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
      end)
  | Expr expr ->
    let expr, r = simplify_direct env r expr in
    Expr expr, r

and simplify_direct env r (tree : Flambda.t) : Flambda.t * R.t =
  match tree with
  | Var var ->
    let var = Freshening.apply_variable (E.freshening env) var in
    (* If from the approximations we can simplify [var], then we will be
       forced to insert [let]-expressions (done using [name_expr], in
       [Simple_value_approx]) to bind a [named].  This has an important
       consequence: it brings bindings of constants closer to their use
       points. *)
    simplify_using_approx_and_env env r (Var var) (E.find_exn env var)
  | Apply apply ->
    simplify_apply env r ~apply
  | Let _ ->
    let for_defining_expr (env, r) var defining_expr =
      let defining_expr, r = simplify_named env r defining_expr in
      let var, sb = Freshening.add_variable (E.freshening env) var in
      let env = E.set_freshening env sb in
      let env = E.add env var (R.approx r) in
      (env, r), var, defining_expr
    in
    let for_last_body (env, r) body =
      simplify env r body
    in
    let filter_defining_expr r var defining_expr free_vars_of_body =
      if Variable.Set.mem var free_vars_of_body then
        r, var, Some defining_expr
      else if Effect_analysis.no_effects_named defining_expr then
        let r = R.map_benefit r (B.remove_code_named defining_expr) in
        r, var, None
      else
        (* CR mshinwell: check that Pignore is inserted correctly by a later
           pass.
           pchambart: Is it still relevant ? Why should we need ignore anymore ? *)
        (* Generate a fresh name for increasing legibility of the
           intermediate language (in particular to make it more obvious that
           the variable is unused). *)
        let fresh_var = Variable.create "for_side_effect_only" in
        r, fresh_var, Some defining_expr
    in
    Flambda.fold_lets_option tree
      ~init:(env, r)
      ~for_defining_expr
      ~for_last_body
      ~filter_defining_expr
  | Let_mutable (mut_var, var, body) ->
    (* CR mshinwell: add the dead let elimination, as above. *)
    simplify_free_variable env var ~f:(fun env var ->
      let mut_var, sb =
        Freshening.add_mutable_variable (E.freshening env) mut_var
      in
      let env = E.set_freshening env sb in
      let body, r =
        simplify (E.add_mutable env mut_var A.value_unknown) r body
      in
      Flambda.Let_mutable (mut_var, var, body), r)
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
    begin
      match body with
      | Let { var; defining_expr = def; body; _ }
          when not (Flambda_utils.might_raise_static_exn def i) ->
        simplify_direct env r
          (Flambda.create_let var def (Static_catch (i, vars, body, handler)))
      | _ ->
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
                  Flambda.create_let var (Expr arg) body)
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
    simplify_free_variable env arg ~f:(fun env arg ->
      begin match (E.find_exn env arg).descr with
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
        If_then_else (arg, ifso, ifnot),
          ret r (A.meet ifso_approx ifnot_approx)
      end)
  | While (cond, body) ->
    let cond, r = simplify env r cond in
    let env = E.inside_simplify env in
    let body, r = simplify env r body in
    While (cond, body), ret r A.value_unknown
  | Send { kind; meth; obj; args; dbg; } ->
    simplify_free_variable env meth ~f:(fun env meth ->
      simplify_free_variable env obj ~f:(fun env obj ->
        simplify_free_variables env args ~f:(fun _env args ->
          Send { kind; meth; obj; args; dbg; }, ret r A.value_unknown)))
  | For { bound_var; from_value; to_value; direction; body; } ->
    simplify_free_variable env from_value ~f:(fun env from_value ->
      simplify_free_variable env to_value ~f:(fun env to_value ->
        let bound_var, sb =
          Freshening.add_variable (E.freshening env) bound_var
        in
        let env =
          E.inside_simplify
            (E.add (E.set_freshening env sb) bound_var A.value_unknown)
        in
        let env = E.inside_simplify env in
        let body, r = simplify env r body in
        For { bound_var; from_value; to_value; direction; body; },
          ret r A.value_unknown))
  | Assign { being_assigned; new_value; } ->
    (* No need to use something like [simplify_free_variable]: the
       approximation of [being_assigned] is always unknown. *)
    let being_assigned =
      Freshening.apply_mutable_variable (E.freshening env) being_assigned
    in
    simplify_free_variable env new_value ~f:(fun _env new_value ->
      Assign { being_assigned; new_value; }, ret r A.value_unknown)
  | Switch (arg, sw) ->
    (* When [arg] is known to be a variable whose approximation is that of a
       block with a fixed tag or a fixed integer, we can eliminate the
       [Switch].  (This should also make the [Let] that binds [arg] redundant,
       meaning that it too can be eliminated.) *)
    simplify_free_variable env arg ~f:(fun env arg ->
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
      begin match (E.find_exn env arg).descr with
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
      end)
  | String_switch (arg, sw, def) ->
    simplify_free_variable env arg ~f:(fun env arg ->
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
      String_switch (arg, sw, def), ret r A.value_unknown)
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
     exactly is happening here?
     It looks like this might only be needed in the "field" case.
  *)
  f, ret r (Backend.really_import_approx (R.approx r))

let constant_defining_value_approx
    env
    (constant_defining_value:Flambda.constant_defining_value) =
  match constant_defining_value with
  | Allocated_const const ->
    approx_for_allocated_const const
  | Block (tag, fields) ->
    let fields =
      List.map
        (function
          | Flambda.Symbol sym -> begin
              match E.find_symbol_opt env sym with
              | Some approx -> approx
              | None -> A.value_unknown
            end
          | Flambda.Const cst -> simplify_const cst)
        fields
    in
    A.value_block (tag, Array.of_list fields)
  | Set_of_closures { function_decls; free_vars; specialised_args } ->
    (* At toplevel, there is no freshening currently happening (this
       cannot be the body of a currently inlined function), so we can
       keep the original set_of_closures in the approximation. *)
    assert(E.freshening env = Freshening.empty);
    assert(Variable.Map.is_empty free_vars);
    assert(Variable.Map.is_empty specialised_args);
    let unchanging_params =
      Invariant_params.unchanging_params_in_recursion function_decls
        ~backend:(E.backend env)
    in
    let value_set_of_closures : A.value_set_of_closures =
      { function_decls;
        bound_vars = Var_within_closure.Map.empty;
        unchanging_params;
        specialised_args = Variable.Set.empty;
        freshening = Freshening.Project_var.empty;
      }
    in
    A.value_set_of_closures value_set_of_closures
  | Project_closure (set_of_closures_symbol, closure_id) -> begin
      match E.find_symbol_opt env set_of_closures_symbol with
      | None ->
        A.value_unknown
      | Some set_of_closures_approx ->
        let checked_approx =
          A.checked_approx_for_set_of_closures_allowing_unknown_and_unresolved
            set_of_closures_approx
        in
        match checked_approx with
        | Ok value_set_of_closures ->
          let closure_id =
            A.freshen_and_check_closure_id value_set_of_closures closure_id
          in
          A.value_closure value_set_of_closures closure_id
        | Unknown_or_unresolved ->
          A.value_unknown
        | Wrong ->
          Misc.fatal_errorf "Wrong approximation for [Project_closure] \
                             when being used as a [constant_defining_value]: %a"
            Flambda.print_constant_defining_value constant_defining_value
    end

let define_let_rec_symbol_approx env defs =
  (* First declare an empty version of the symbols *)
  let env =
    List.fold_left (fun env (symbol, _) ->
        E.add_symbol env symbol A.value_unknown)
      env defs
  in
  let rec loop times env =
    if times <= 0 then
      env
    else
      let env =
        List.fold_left (fun env (symbol, constant_defining_value) ->
            let approx =
              constant_defining_value_approx env constant_defining_value
            in
            E.redefine_symbol env symbol approx)
          env defs
      in
      loop (times-1) env
  in
  loop 2 env

let simplify_constant_defining_value
    env r symbol
    (constant_defining_value:Flambda.constant_defining_value) =
  let r, constant_defining_value, approx =
    match constant_defining_value with
    (* No simplifications are possible for [Allocated_const] or [Block]. *)
    | Allocated_const const ->
      r, constant_defining_value, approx_for_allocated_const const
    | Block (tag, fields) ->
      let fields = List.map
          (function
            | Flambda.Symbol sym -> E.find_symbol_exn env sym
            | Flambda.Const cst -> simplify_const cst)
          fields
      in
      r, constant_defining_value, A.value_block (tag, Array.of_list fields)
    | Set_of_closures set_of_closures ->
      if Variable.Map.cardinal set_of_closures.free_vars <> 0 then begin
        Misc.fatal_errorf "Set of closures bound by [Let_symbol] is not \
                           closed: %a"
          Flambda.print_set_of_closures set_of_closures
      end;
      let set_of_closures, r =
        simplify_set_of_closures env r set_of_closures
      in
      r, ((Set_of_closures set_of_closures) : Flambda.constant_defining_value),
      R.approx r
    | Project_closure (set_of_closures_symbol, closure_id) ->
      (* No simplifications are necessary here. *)
      let set_of_closures_approx =
        E.find_symbol_exn env set_of_closures_symbol
      in
      let closure_approx =
        match A.check_approx_for_set_of_closures set_of_closures_approx with
        | Ok (_, value_set_of_closures) ->
          let closure_id =
            A.freshen_and_check_closure_id value_set_of_closures closure_id
          in
          A.value_closure value_set_of_closures closure_id
        | Unresolved _symbol ->
          A.value_unknown  (* CR mshinwell: is this correct? *)
        | Wrong ->
          Misc.fatal_errorf "Wrong approximation for [Project_closure] \
                             when being used as a [constant_defining_value]: %a"
            Flambda.print_constant_defining_value constant_defining_value
      in
      r, constant_defining_value, closure_approx
  in
  let approx = A.augment_with_symbol approx symbol in
  let r = ret r approx in
  r, constant_defining_value, approx

let rec simplify_program env r (program : Flambda.program)
  : Flambda.program * R.t =
  match program with
  | Let_rec_symbol (defs, program) ->
    let env = define_let_rec_symbol_approx env defs in
    let env, r, defs =
      List.fold_left (fun (env, r, defs) (symbol, def) ->
          let r, def, approx =
            simplify_constant_defining_value env r symbol def
          in
          let approx = A.augment_with_symbol approx symbol in
          let env = E.redefine_symbol env symbol approx in
          (env, r, (symbol, def) :: defs))
        (env, r, []) defs
    in
    let program, r = simplify_program env r program in
    Let_rec_symbol (defs, program), r
  | Let_symbol (symbol, constant_defining_value, program) ->
    let r, constant_defining_value, approx =
      simplify_constant_defining_value env r symbol constant_defining_value
    in
    let approx = A.augment_with_symbol approx symbol in
    let env = E.add_symbol env symbol approx in
    let program, r = simplify_program env r program in
    Let_symbol (symbol, constant_defining_value, program), r
  | Import_symbol (symbol, program) ->
    let env, approx =
      match E.find_symbol_exn env symbol with
      | exception Not_found ->
        let module Backend = (val (E.backend env) : Backend_intf.S) in
        (* CR mshinwell for mshinwell: Is there a reason we cannot use
           [simplify_named_using_approx_and_env] here? *)
        let approx = Backend.import_symbol symbol in
        E.add_symbol env symbol approx, approx
      | approx -> env, approx
    in
    let r = ret r approx in
    let program, r = simplify_program env r program in
    Import_symbol (symbol, program), r
  | Initialize_symbol (symbol, tag, fields, program) ->
    let fields, approxs, r = simplify_list env r fields in
    let approx =
      A.augment_with_symbol (A.value_block (tag, Array.of_list approxs))
        symbol
    in
    let module Backend = (val (E.backend env) : Backend_intf.S) in
    let env = E.add_symbol env symbol approx in
    let program, r = simplify_program env r program in
    Initialize_symbol (symbol, tag, fields, program), r
  | Effect (expr, program) ->
    let expr, r = simplify env r expr in
    let program, r = simplify_program env r program in
    Effect (expr, program), r
  | End root -> End root, r

(* CR mshinwell for pchambart: Change to a "-dinlining-benefit" option? *)
let debug_benefit =
  try ignore (Sys.getenv "BENEFIT"); true
  with _ -> false

let run ~never_inline ~backend program =
  let r =
    if never_inline then
      R.set_inlining_threshold (R.create ()) Inlining_cost.Never_inline
    else
      R.create ()
  in
  let stats = !Clflags.inlining_stats in
  if never_inline then Clflags.inlining_stats := false;
  (* CR mshinwell: Why does this always set [never_inline:false]? *)
  let initial_env = E.create ~never_inline:false ~backend in
  let result, r = simplify_program initial_env r program in
  let result = Flambda_utils.introduce_needed_import_symbols result in
  Clflags.inlining_stats := stats;
  if not (Static_exception.Set.is_empty (R.used_staticfail r))
  then begin
    Misc.fatal_error (Format.asprintf "remaining static exceptions: %a@.%a@."
      Static_exception.Set.print (R.used_staticfail r)
      Flambda.print_program result)
  end;
  assert (Static_exception.Set.is_empty (R.used_staticfail r));
  if debug_benefit then
    Format.printf "benefit:@ %a@."
      B.print (R.benefit r);
  result
