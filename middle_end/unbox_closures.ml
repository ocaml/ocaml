(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module A = Simple_value_approx

(* CR mshinwell: should be backend-dependent *)
let max_arguments = 10

let params_only_used_for_project_var function_decl =
  let free_variables_including_project_var =
    Free_variables.calculate function_decl.body
  in
  let free_variables_excluding_project_var =
    Free_variables.calculate function_decl.body
      ~ignore_uses_in_project_var:()
  in
  let free_variables_only_used_for_project_var =
    Variable.Set.diff free_variables_including_project_var
      free_variables_excluding_project_var
  in
  Variable.Set.inter free_variables_only_used_for_project_var
    (Variable.Set.of_list function_decl.params)

let rewrite_function_decl ~env ~function_decls ~specialised_args
      fun_var function_decl funs =
  let closure_id = Closure_id.wrap fun_var in
  let closure_element_params_for_specialised_args =
    Variable.Map.fold (fun specialised_param specialised_to map ->
        let approx = E.find_exn env specialised_to in
        match A.check_approx_for_closure approx with
        | Wrong ->
          Misc.fatal_error "Unbox_closures.rewrite_function_decl: \
              wrong approximation for specialised arg %a -> %a: %a"
            Variable.print specialised_param
            Variable.print specialised_to
            Simple_value_approx.print approx
        | Ok (_value_closure, _approx_var, value_set_of_closures) ->
          Var_within_closure.Map.fold (fun var _approx map ->
              let param = Variable.rename ~append:"_unboxed_param" var in
              Variable.Pair.Map.add (specialised_param, var) param map))
      specialised_args
      Variable.Pair.Map.empty
  in
  let closure_element_params_for_vars_bound_by_closure =
    Variable.Set.fold (fun var map ->
        let param = Variable.rename ~append:"_unboxed_bound"
      (Flambda_utils.variables_bound_by_the_closure closure_id function_decls)
      Variable.Pair.Map.empty
  in
  let closure_element_params_mapping =
    Variable.Pair.Map.union closure_element_params_for_specialised_args
      closure_element_params_for_vars_bound_by_closure
  in
  let closure_element_params =
    List.map snd (Variable.Pair.Map.bindings closure_element_params_mapping)
  in
  let all_params = function_decl.params @ closure_element_params in
  if List.length all_params > max_arguments then begin
    Variable.Map.add fun_var function_decl funs
  end else begin
    let body_using_params_not_closures =
      Flambda_iterators.map_project_var_to_expr_opt function_decl.body
        ~f:(fun project_var ->
          match
            Variable.Pair.Map.find closure_element_params_mapping
              (project_var.closure, Var_within_closure.unwrap project_var.var)
          with
          | None -> None
          | Some param -> Some (Flambda.Var param))
    in
    let free_variables =
      Free_variables.calculate body_using_params_not_closures
    in
    let rewritten_function_decl : Flambda.function_declaration =
      { params = all_params;
        body = body_using_params_not_closures;
        free_variables;
        stub = function_decl.stub;
        dbg = function_decl.dbg;
      }
    in
    let wrapper_params =
      List.map (fun param -> Variable.rename ~append:"_unbox_closures" param)
        function_decl.params
    in
    let wrapper_body : Flambda.t =
      let bindings_for_closure_element_params =
        Variable.Pair.fold (fun (projected_from, var) param ->
            let binding_var = Variable.rename var in
            let defining_expr : Flambda.named =
              Project_var {
                closure = fun_var;
                closure_id;
                var = Var_within_closure.wrap var;
              }
            in
            binding_var, defining_expr)
          closure_element_
      in
      let args_for_vars_bound_by_closure =
        List.map fst bindings_for_vars_bound_by_closure
      in
      let body : Flambda.t =
        Apply {
          func = new_fun_var;
          args = wrapper_params @ args_for_vars_bound_by_closure;
          kind = Direct closure_id;
          dbg = Debuginfo.none;
        }
      in
      Flambda_utils.bind ~bindings:bindings_for_vars_bound_by_closure ~body
    in
    let wrapper : Flambda.function_declaration =
      { params = wrapper_params;
        body = wrapper_body;
        free_variables = Free_variables.calculate wrapper_body;
        stub = true;
        dbg = Debuginfo.none;
      }
    in
    Variable.Map.add new_fun_var rewritten_function_decl
      (Variable.Map.add fun_var wrapper funs)
  end

let run ~env ~function_decls ~specialised_args =
  let funs =
    Variable.Map.fold (rewrite_function_decl ~env ~function_decls
        ~specialised_args)
      function_decls.funs function_decls.funs
  in
  let function_decls = { function_decls with funs; } in
  { function_decls; free_vars; specialised_args; }
