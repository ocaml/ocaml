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
module E = Inlining_env
module R = Inlining_result

let new_var name =
  Variable.create name
    ~current_compilation_unit:(Compilation_unit.get_current_exn ())

let which_function_parameters_can_we_specialize ~params ~args
      ~approximations_of_args ~unchanging_params =
  assert (List.length params = List.length args);
  assert (List.length args = List.length approximations_of_args);
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
    (List.combine params args) approximations_of_args
    (Variable.Map.empty, [], [])

let fold_over_exprs_for_variables_bound_by_closure ~fun_id ~clos_id ~clos
      ~init ~f =
  Variable.Set.fold (fun var acc ->
      let expr : _ Flambda.t =
        Project_var
          ({ closure = clos_id;
             closure_id = fun_id;
             var = Var_within_closure.wrap var;
           },
           Expr_id.create ())
      in
      f ~acc ~var ~expr)
    (Flambdautils.variables_bound_by_the_closure fun_id clos) init

let inline_by_copying_function_body ~env ~r
      ~(clos : _ Flambda.function_declarations) ~lfunc ~fun_id
      ~(func : _ Flambda.function_declaration) ~args
      ~simplify =
  let r = R.map_benefit r B.remove_call in
  let env = E.inlining_level_up env in
  let closure = new_var "inline_by_copying_function_body" in
  (* Assign fresh names for the function's parameters and rewrite the body to
     use these new names. *)
  let subst_params = List.map Variable.freshen func.params in
  let subst_map =
    Variable.Map.of_list (List.combine func.params subst_params)
  in
  let body = Flambdautils.toplevel_substitution subst_map func.body in
  (* Around the function's body, bind the parameters to the arguments
     that we saw at the call site. *)
  let bindings_for_params_around_body =
    let args =
      List.map (fun arg -> Flambda.Var (arg, Expr_id.create ())) args
    in
    Flambdautils.bind ~body
      ~bindings:(List.combine subst_params args)
      ~name:"inline_arg"
  in
  (* 2. Now add bindings for variables bound by the closure. *)
  let bindings_for_vars_bound_by_closure_and_params_around_body =
    fold_over_exprs_for_variables_bound_by_closure ~fun_id
      ~clos_id:closure ~clos ~init:bindings_for_params_around_body
      ~f:(fun ~acc:body ~var ~expr ->
        Flambda.Let (Immutable, var, expr, body, Expr_id.create ()))
  in
  (* 3. Finally add bindings for the function declaration identifiers being
     introduced by the whole set of closures. *)
  let expr =
    Variable.Map.fold (fun id _ expr ->
        Flambda.Let (Immutable, id,
          Move_within_set_of_closures ({
              closure;
              start_from = fun_id;
              move_to = Closure_id.wrap id;
            }, Expr_id.create ()),
          expr, Expr_id.create ()))
      clos.funs bindings_for_vars_bound_by_closure_and_params_around_body
  in
  let env =
    E.note_entering_closure env ~closure_id:fun_id
      ~where:Inline_by_copying_function_body
  in
  simplify (E.activate_freshening env) r
    (Flambda.Let (Immutable, closure, lfunc, expr, Expr_id.create ()))

let inline_by_copying_function_declaration ~env ~r ~funct
    ~(function_decls : _ Flambda.function_declarations)
    ~closure_id
    ~(function_decl : _ Flambda.function_declaration)
    ~args_with_approxs ~unchanging_params ~specialised_args ~dbg
    ~simplify =
  let args, approxs = args_with_approxs in
  let more_specialised_args, args, args_decl =
    which_function_parameters_can_we_specialize
      ~params:function_decl.params ~args ~approximations_of_args:approxs
      ~unchanging_params
  in
  if Variable.Set.equal specialised_args
      (Variable.Map.keys more_specialised_args)
  then
    (* If the function already has the right set of specialised arguments,
       then there is nothing to do to improve it here. *)
    None
  else
    let env = E.inlining_level_up env in
    let closure = new_var "inline_by_copying_function_declaration" in
    let set_of_closures =
      (* The free variable map for the duplicated declaration(s) maps the
         "internal" names used within the function bodies to fresh names,
         which in turn are bound to projections from the set of closures being
         copied.  We add these bindings using [Let] around the new
         set-of-closures declaration. *)
      let free_vars, free_vars_for_lets =
        fold_over_exprs_for_variables_bound_by_closure ~fun_id:closure_id
          ~clos_id:closure ~clos:function_decls ~init:(Variable.Map.empty, [])
          ~f:(fun ~acc:(map, for_lets) ~var:internal_var ~expr ->
            let from_closure = new_var "from_closure" in
            Variable.Map.add internal_var from_closure map,
              (from_closure, expr)::for_lets)
      in
      let set_of_closures : _ Flambda.set_of_closures =
        (* This is the new set of closures, with more precise specialisation
           information than the one being copied. *)
        { function_decls;
          free_vars;
          specialised_args = more_specialised_args;
        }
      in
      Flambdautils.bind ~bindings:free_vars_for_lets
        ~body:(Set_of_closures (set_of_closures, Expr_id.create ()))
        ~name:"free_var"
    in
    (* Generate a copy of the function application, including the function
       declaration(s), but with variables (not yet bound) in place of the
       arguments.  The new set of closures is bound to a fresh variable. *)
    let duplicated_application : _ Flambda.t =
      let set_of_closures_var = new_var "dup_set_of_closures" in
      let project_closure : Flambda.project_closure =
        { set_of_closures = set_of_closures_var;
          closure_id;
        }
      in
      let func : _ Flambda.t =
        Project_closure (project_closure, Expr_id.create ())
      in
      Let (Immutable, set_of_closures_var, set_of_closures,
        Apply ({ func; args; kind = Direct closure_id; dbg; },
          Expr_id.create ()),
        Expr_id.create ())
    in
    (* Now bind the variables that will hold the arguments from the original
       application. *)
    let expr : _ Flambda.t =
      Let (Immutable, closure, funct,
        Flambdautils.bind ~body:duplicated_application ~bindings:args_decl
          ~name:"dup_apply_arg",
        Expr_id.create ())
    in
    let env =
      E.note_entering_closure env ~closure_id
        ~where:Inline_by_copying_function_declaration
    in
    Some (simplify (E.activate_freshening env) r expr)
