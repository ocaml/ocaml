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

(* CR mshinwell: should be backend-dependent *)
let max_arguments = 10

let run expr =
  let rewrite { Flambda. function_decls; free_vars; specialised_args; }
        : Flambda.set_of_closures =
    let funs =
      Variable.Map.fold (fun fun_var
            (function_decl : Flambda.function_declaration) funs ->
          let closure_id = Closure_id.wrap fun_var in
          let vars_bound_by_closure =
            Flambda_utils.variables_bound_by_the_closure closure_id
              function_decls
          in
          let num_params = List.length function_decl.params in
          let num_vars_bound_by_closure =
            Variable.Set.cardinal vars_bound_by_closure
          in
          if num_vars_bound_by_closure > 0
              && num_params + num_vars_bound_by_closure <= max_arguments
          then begin
            let param_map_for_vars_bound_by_closure =
              List.map (fun var ->
                  var, Variable.rename ~append:"unbox_closures" var)
                (Variable.Set.elements vars_bound_by_closure)
              |> Variable.Map.of_list
            in
            let param_for_var_bound_by_closure var =
              Variable.Map.find (Var_within_closure.unwrap var)
                param_map_for_vars_bound_by_closure
            in
            let params_for_vars_bound_by_closure =
              List.map snd
                (Variable.Map.bindings param_map_for_vars_bound_by_closure)
            in
            let body_using_args_not_closure =
              Flambda_iterators.map_project_var_to_expr_opt function_decl.body
                ~f:(fun project_var ->
                  if not (Closure_id.equal closure_id project_var.closure_id)
                  then begin
                    None
                  end else begin
                    Some (Flambda.Var
                      (param_for_var_bound_by_closure project_var.var))
                  end)
            in
            let new_fun_var =
              Variable.rename ~append:"unbox_closures" fun_var
            in
            assert (Variable.Set.equal function_decl.free_variables
                (Free_variables.calculate body_using_args_not_closure));
            let rewritten_function_decl_params =
              function_decl.params @ params_for_vars_bound_by_closure
            in
            let rewritten_function_decl : Flambda.function_declaration =
              { params = rewritten_function_decl_params;
                body = body_using_args_not_closure;
                free_variables = function_decl.free_variables;
                stub = function_decl.stub;
                dbg = function_decl.dbg;
              }
            in
            let wrapper_params =
              List.map (fun param ->
                  Variable.rename ~append:"unbox_closures" param)
                function_decl.params
            in
            let wrapper_body : Flambda.t =
              let bindings_for_vars_bound_by_closure =
                List.map (fun var ->
                    let binding_var = Variable.rename var in
                    let defining_expr : Flambda.named =
                      Project_var {
                        closure = fun_var;
                        closure_id;
                        var = Var_within_closure.wrap var;
                      }
                    in
                    binding_var, defining_expr)
                  (Variable.Set.elements vars_bound_by_closure)
              in
              let args_for_vars_bound_by_closure =
                List.map fst bindings_for_vars_bound_by_closure
              in
              let body : Flambda.t =
                Apply {
                  func = fun_var;
                  args = wrapper_params @ args_for_vars_bound_by_closure;
                  kind = Direct closure_id;
                  dbg = Debuginfo.none;
                }
              in
              Flambda_utils.bind ~bindings:bindings_for_vars_bound_by_closure
                ~body
            in
            let free_variables = Variable.Set.singleton fun_var in
            assert (Variable.Set.equal (Free_variables.calculate wrapper_body)
                free_variables);
            let wrapper : Flambda.function_declaration =
              { params = wrapper_params;
                body = wrapper_body;
                free_variables;
                stub = true;
                dbg = Debuginfo.none;
              }
            in
            Variable.Map.add new_fun_var rewritten_function_decl
              (Variable.Map.add fun_var wrapper funs)
          end else begin
            funs
          end)
      function_decls.funs function_decls.funs
    in
    let function_decls = { function_decls with funs; } in
    { function_decls; free_vars; specialised_args; }
  in
  Flambda_iterators.map_sets_of_closures expr ~f:rewrite
