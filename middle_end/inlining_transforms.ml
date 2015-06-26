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

let inline_by_copying_function_body ~env ~r
      ~(clos : _ Flambda.function_declarations) ~lfunc ~fun_id
      ~(func : _ Flambda.function_declaration) ~args =
  let r = R.map_benefit r B.remove_call in
  let env = E.inlining_level_up env in
  let clos_id = new_var "inline_by_copying_function_body" in
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
    List.fold_left2 (fun body id arg ->
        Flambda.Flet (Immutable, id, arg, body,
          Expr_id.create ~name:"inline arg" ()))
      body subst_params args
  in
  (* 2. Now add bindings for variables bound by the closure. *)
  let bindings_for_vars_bound_by_closure_and_params_around_body =
    Flambdautils.fold_over_exprs_for_variables_bound_by_closure ~fun_id
      ~clos_id ~clos ~init:bindings_for_params_around_body
      ~f:(fun ~acc:body ~var ~expr ->
        Flambda.Flet (Immutable, var, expr, body, Expr_id.create ()))
  in
  (* 3. Finally add bindings for the function declaration identifiers being
     introduced by the whole set of closures. *)
  let expr =
    Variable.Map.fold (fun id _ expr ->
        Flambda.Flet (Immutable, id,
          Fmove_within_set_of_closures ({
              closure = clos_id;
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
  loop (E.activate_freshening env) r
    (Flambda.Flet (Immutable, clos_id, lfunc, expr, Expr_id.create ()))

let inline_by_copying_function_declaration ~env ~r ~funct
    ~(clos : _ Flambda.function_declarations)
    ~closure_id
    ~(func : _ Flambda.function_declaration)
    ~args_with_approxs ~unchanging_params ~specialised_args ~dbg =
  let args, approxs = args_with_approxs in
  let env = E.inlining_level_up env in
  let clos_id = new_var "inline_by_copying_function_declaration" in
  let fv =
    Flambdautils.fold_over_exprs_for_variables_bound_by_closure
      ~fun_id:closure_id
      ~clos_id ~clos ~init:Variable.Map.empty
      ~f:(fun ~acc ~var ~expr -> Variable.Map.add var expr acc)
  in
  let spec_args, args, args_decl =
    which_function_parameters_can_we_specialize
      ~params:func.params ~args ~approximations_of_args:approxs
      ~unchanging_params
  in
  if Variable.Set.equal specialised_args (Variable.Map.keys spec_args)
  then
    (* If the function already has the right set of specialised arguments,
       then there is nothing to do to improve it here. *)
    None
  else
  (* First we generate a copy of the function application, including the
     function declaration (s), but with variables (not yet bound) in place of
     the arguments. *)
  let duplicated_application : _ Flambda.t =
    let set_of_closures : _ Flambda.set_of_closures =
      { function_decls = clos;
        free_vars = fv;
        specialised_args = spec_args;
      };
    in
    let set_of_closures_var = fresh_variable t ~name:"dup_set_of_closures" in
    let select_closure : Flambda.select_closure =
      { from = Set_of_closures_same_unit set_of_closures_var;
        closure_id;
      }
    in
    Fapply (

        args = List.map (fun id -> Flambda.Fvar (id, Expr_id.create ())) args;
        kind = Direct closure_id;
        dbg;
      }, Expr_id.create ())
  in
  (* Now we bind the variables that will hold the arguments from the original
     application, together with the set-of-closures identifier. *)
  let expr : _ Flambda.t =
    Flet (Immutable, clos_id, funct,
      List.fold_left (fun expr (id, arg) ->
          Flambda.Flet (Immutable, id, arg, expr, Expr_id.create ()))
        duplicated_application args_decl,
      Expr_id.create ())
  in
  let env =
    E.note_entering_closure env ~closure_id
      ~where:Inline_by_copying_function_declaration
  in
  Some (loop (E.activate_freshening env) r expr)
