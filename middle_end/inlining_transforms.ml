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

let which_function_parameters_can_we_specialise ~params ~args
      ~args_approxs ~invariant_params ~specialised_args =
  assert (List.length params = List.length args);
  assert (List.length args = List.length args_approxs);
  List.fold_right2 (fun (var, arg) approx
    (worth_specialising_args, spec_args, args, args_decl) ->
      let spec_args =
        if Variable.Set.mem var invariant_params ||
           Variable.Set.mem var specialised_args
        then
          Variable.Map.add var arg spec_args
        else
          spec_args
      in
      let worth_specialising_args =
        if Simple_value_approx.useful approx
          && Variable.Set.mem var invariant_params
        then
          Variable.Set.add var worth_specialising_args
        else
          worth_specialising_args
      in
      worth_specialising_args, spec_args, arg :: args, args_decl)
    (List.combine params args) args_approxs
    (Variable.Set.empty, Variable.Map.empty, [], [])

(** Fold over all variables bound by the given closure, which is bound to the
    variable [lhs_of_application], and corresponds to the given
    [function_decls].  Each variable bound by the closure is passed to the
    user-specified function as an [Flambda.named] value that projects the
    variable from its closure. *)
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

(* CR mshinwell: Add a note somewhere to explain why "bound by the closure"
   does not include the function identifiers for other functions in the same
   set of closures. *)

(** Inline a function by copying its body into a context where it becomes
    closed.  That is to say, we bind the free variables of the body
    (= "variables bound by the closure"), and any function identifiers
    introduced by the corresponding set of closures. *)
let inline_by_copying_function_body ~env ~r ~function_decls ~lhs_of_application
      ~closure_id_being_applied ~function_decl ~args ~simplify =
  assert (E.mem env lhs_of_application);
  assert (List.for_all (E.mem env) args);
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
  (* Add bindings for the variables bound by the closure. *)
  let bindings_for_vars_bound_by_closure_and_params_to_args =
    fold_over_projections_of_vars_bound_by_closure ~closure_id_being_applied
      ~lhs_of_application ~function_decls ~init:bindings_for_params_to_args
      ~f:(fun ~acc:body ~var ~expr -> Flambda.create_let var expr body)
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
        Flambda.create_let another_closure_in_the_same_set
          (Move_within_set_of_closures {
            closure = lhs_of_application;
            start_from = closure_id_being_applied;
            move_to = Closure_id.wrap another_closure_in_the_same_set;
          })
          expr)
      function_decls.Flambda.funs
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
    ~args ~args_approxs ~invariant_params ~specialised_args ~dbg ~simplify =
  let worth_specialising_args, specialisable_args, args, args_decl =
    which_function_parameters_can_we_specialise
      ~params:function_decl.params ~args ~args_approxs
      ~invariant_params
      ~specialised_args
  in
  if Variable.Set.subset worth_specialising_args specialised_args
  then
    (* Don't duplicate the function definition if we would make its
       specialisation information worse.  (Note that this judgement is made
       based only on those arguments found to be invariant with known-useful
       approximations, rather than on all invariant arguments.) *)
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
    let set_of_closures =
      (* This is the new set of closures, with more precise specialisation
         information than the one being copied. *)
      Flambda.create_set_of_closures ~function_decls ~free_vars
        ~specialised_args:specialisable_args
    in
    (* Generate a copy of the function application, including the function
       declaration(s), but with variables (not yet bound) in place of the
       arguments. *)
    let duplicated_application : Flambda.t =
      let project_closure : Flambda.project_closure =
        { set_of_closures = set_of_closures_var;
          closure_id = closure_id_being_applied;
        }
      in
      let func = new_var "dup_func" in
      let body : Flambda.t =
        Flambda.create_let set_of_closures_var
          (Set_of_closures set_of_closures)
          (Flambda.create_let func (Project_closure project_closure)
            (Apply {
              func;
              args;
              kind = Direct closure_id_being_applied;
              dbg;
              inline = function_decl.inline;
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
