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

let new_var name =
  Variable.create name
    ~current_compilation_unit:(Compilation_unit.get_current_exn ())

let which_function_parameters_can_we_specialize ~params ~args
      ~args_approxs ~unchanging_params =
  assert (List.length params = List.length args);
  assert (List.length args = List.length args_approxs);
  List.fold_right2 (fun (var, arg) approx (spec_args, args, args_decl) ->
      let spec_args =
        if Simple_value_approx.useful approx
          && Variable.Set.mem var unchanging_params
        then
          Variable.Map.add var arg spec_args
        else
          spec_args
      in
      spec_args, arg :: args, args_decl)
    (List.combine params args) args_approxs
    (Variable.Map.empty, [], [])

let fold_over_projections_of_vars_bound_by_closure ~closure_id_being_applied
      ~lhs_of_application ~function_decls ~init ~f =
  Variable.Set.fold (fun var acc ->
      let expr : Flambda.named =
        Project_var {
          closure = lhs_of_application;
          closure_id = closure_id_being_applied;
          var = Var_within_closure.wrap var;
        }
      in
      f ~acc ~var ~expr)
    (Flambda_utils.variables_bound_by_the_closure closure_id_being_applied
      function_decls)
    init

(** Assign fresh names for a function's parameters and rewrite the body to
    use these new names. *)
let copy_of_function's_body_with_freshened_params
      ~(function_decl : Flambda.function_declaration) =
  let params = function_decl.params in
  let freshened_params = List.map Variable.freshen params in
  let subst = Variable.Map.of_list (List.combine params freshened_params) in
  let body = Flambda_utils.toplevel_substitution subst function_decl.body in
  freshened_params, body

let inline_by_copying_function_body ~env ~r
      ~(function_decls : Flambda.function_declarations) ~lhs_of_application
      ~closure_id_being_applied ~(function_decl : Flambda.function_declaration)
      ~args ~simplify =
  let r = R.map_benefit r B.remove_call in
  let env = E.inlining_level_up env in
  let freshened_params, body =
    copy_of_function's_body_with_freshened_params ~function_decl
  in
  let bindings_for_params_to_args =
    (* Bind the function's parameters to the arguments from the call site. *)
    let args = List.map (fun arg -> Flambda.Expr (Var arg)) args in
    Flambda_utils.bind ~body ~bindings:(List.combine freshened_params args)
  in
  (* Add bindings for variables bound by the closure. *)
  let bindings_for_vars_bound_by_closure_and_params_to_args =
    fold_over_projections_of_vars_bound_by_closure ~closure_id_being_applied
      ~lhs_of_application ~function_decls ~init:bindings_for_params_to_args
      ~f:(fun ~acc:body ~var ~expr -> Flambda.Let (Immutable, var, expr, body))
  in
  (* CR mshinwell: How does this not add a variable that points to the
     function being applied itself?  Presumably it shouldn't do that. *)
  (* Add bindings for variables corresponding to the functions introduced by
     the whole set of closures.  Each such variable will be bound to a closure;
     each such closure is in turn produced by moving from the closure being
     applied to another closure in the same set.
  *)
  let expr =
    Variable.Map.fold (fun another_closure_in_the_same_set _ expr ->
        Flambda.Let (Immutable, another_closure_in_the_same_set,
          Move_within_set_of_closures {
            closure = lhs_of_application;
            start_from = closure_id_being_applied;
            move_to = Closure_id.wrap another_closure_in_the_same_set;
          },
          expr))
      function_decls.funs
      bindings_for_vars_bound_by_closure_and_params_to_args
  in
  let env =
    E.note_entering_closure env ~closure_id:closure_id_being_applied
      ~where:Inline_by_copying_function_body
  in
  simplify (E.activate_freshening env) r expr

let inline_by_copying_function_declaration ~env ~r
    ~(function_decls : Flambda.function_declarations)
    ~lhs_of_application ~closure_id_being_applied
    ~(function_decl : Flambda.function_declaration)
    ~args ~args_approxs ~unchanging_params ~specialised_args ~dbg ~simplify =
  let more_specialised_args, args, args_decl =
    which_function_parameters_can_we_specialize
      ~params:function_decl.params ~args ~args_approxs ~unchanging_params
  in
  if Variable.Set.equal specialised_args
      (Variable.Map.keys more_specialised_args)
  then
    (* If the function already has the right set of specialised arguments,
       then there is nothing to do to improve it here. *)
    None
  else
    let env = E.inlining_level_up env in
    let set_of_closures_var = new_var "dup_set_of_closures" in
    (* The free variable map for the duplicated declaration(s) maps the
       "internal" names used within the function bodies to fresh names,
       which in turn are bound to projections from the set of closures being
       copied.  We add these bindings using [Let] around the new
       set-of-closures declaration. *)
    let free_vars, free_vars_for_lets =
      fold_over_projections_of_vars_bound_by_closure ~closure_id_being_applied
        ~lhs_of_application ~function_decls ~init:(Variable.Map.empty, [])
        ~f:(fun ~acc:(map, for_lets) ~var:internal_var ~expr ->
          let from_closure = new_var "from_closure" in
          Variable.Map.add internal_var from_closure map,
            (from_closure, expr)::for_lets)
    in
    let set_of_closures : Flambda.set_of_closures =
      (* This is the new set of closures, with more precise specialisation
         information than the one being copied. *)
      { function_decls;
        free_vars;
        specialised_args = more_specialised_args;
      }
    in
    (* Generate a copy of the function application, including the function
       declaration(s), but with variables (not yet bound) in place of the
       arguments.  The new set of closures is bound to a fresh variable. *)
    let duplicated_application : Flambda.t =
      let project_closure : Flambda.project_closure =
        { set_of_closures = set_of_closures_var;
          closure_id = closure_id_being_applied;
        }
      in
      let func = new_var "dup_func" in
      let body : Flambda.t =
        Let (Immutable, set_of_closures_var, Set_of_closures set_of_closures,
          Let (Immutable, func, Project_closure project_closure,
            Apply {
              func;
              args;
              kind = Direct closure_id_being_applied;
              dbg;
            }))
      in
      Flambda_utils.bind ~bindings:free_vars_for_lets ~body
    in
    (* Now bind the variables that will hold the arguments from the original
       application. *)
    let expr : Flambda.t =
      Flambda_utils.bind ~body:duplicated_application ~bindings:args_decl
    in
    let env =
      E.note_entering_closure env ~closure_id:closure_id_being_applied
        ~where:Inline_by_copying_function_declaration
    in
    Some (simplify (E.activate_freshening env) r expr)
