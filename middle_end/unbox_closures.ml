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
module E = Inline_and_simplify_aux.Env

let rewrite_function_decl ~env
      ~(function_decls : Flambda.function_declarations) ~fun_var
      ~(function_decl : Flambda.function_declaration) ~specialised_args
      ~funs ~additional_specialised_args =
  let closure_id = Closure_id.wrap fun_var in
  let closure_element_params_for_specialised_args =
    Variable.Map.fold (fun specialised_param specialised_to map ->
        let approx = E.find_exn env specialised_to in
        match A.check_approx_for_closure approx with
        | Wrong -> map  (* Ignore specialised args that aren't closures. *)
        | Ok (_value_closure, _approx_var, value_set_of_closures) ->
          Var_within_closure.Map.fold (fun var _approx map ->
              let var = Var_within_closure.unwrap var in
              let param = Variable.rename ~append:"_unboxed_param" var in
              Variable.Pair.Map.add (specialised_param, var) param map)
            value_set_of_closures.bound_vars
            map)
      specialised_args
      Variable.Pair.Map.empty
  in
  let closure_element_params_for_vars_bound_by_closure =
    Variable.Set.fold (fun var map ->
        let param = Variable.rename ~append:"_unboxed_bound" var in
        Variable.Pair.Map.add (fun_var, var) param map)
      (Flambda_utils.variables_bound_by_the_closure closure_id function_decls)
      Variable.Pair.Map.empty
  in
  let closure_element_params_mapping =
    Variable.Pair.Map.disjoint_union
      closure_element_params_for_specialised_args
      closure_element_params_for_vars_bound_by_closure
      ~eq:Variable.equal
  in
  let closure_element_params =
    List.map snd (Variable.Pair.Map.bindings closure_element_params_mapping)
  in
  let all_params = function_decl.params @ closure_element_params in
  let new_fun_var = Variable.rename ~append:"_unbox_closures" fun_var in
  let body_using_params_not_closures =
    function_decl.body
    |> Flambda_iterators.map_project_var_to_expr_opt ~f:(fun project_var ->
        match
          Variable.Pair.Map.find
            (project_var.closure, Var_within_closure.unwrap project_var.var)
            closure_element_params_mapping
        with
        | exception Not_found -> None
        | param -> Some (Flambda.Var param))
    |> Flambda_iterators.map_apply ~f:(fun apply ->
        if not ( Variable.equal apply.func fun_var) then apply
        else
          { func = new_fun_var;
            args = apply.args @ closure_element_params;
            kind = Direct (Closure_id.wrap new_fun_var);
            dbg = apply.dbg;
          })
  in
  let rewritten_function_decl =
    Flambda.create_function_declaration ~params:all_params
      ~body:body_using_params_not_closures ~stub:function_decl.stub
      ~dbg:function_decl.dbg
  in
  let wrapper_params =
    List.map (fun param -> Variable.rename ~append:"_unbox_closures" param)
      function_decl.params
  in
  let function_decl_params_to_wrapper_params =
    Variable.Map.of_list (List.combine function_decl.params wrapper_params)
  in
  let wrapper_params_to_function_decl_params =
    Variable.Map.of_list (List.combine wrapper_params function_decl.params)
  in
  let additional_specialised_args =
    Variable.Map.fold (fun wrapper_param original_param
          additional_specialised_args ->
        match Variable.Map.find original_param specialised_args with
        | exception Not_found -> additional_specialised_args
        | specialised_to ->
          Variable.Map.add wrapper_param specialised_to
            additional_specialised_args)
      wrapper_params_to_function_decl_params
      additional_specialised_args
  in
  let wrapper_body : Flambda.t =
    let bindings_for_closure_element_params =
      Variable.Pair.Map.fold (fun (projected_from, var) _param bindings ->
          let closure, closure_id =
            if Variable.equal projected_from fun_var then
              fun_var, closure_id
            else
              match
                Variable.Map.find projected_from
                  function_decl_params_to_wrapper_params
              with
              | exception Not_found -> assert false
              | wrapper_param ->
                match
                  Variable.Map.find wrapper_param
                    additional_specialised_args
                with
                | exception Not_found -> assert false
                | specialised_to ->
                  let approx = E.find_exn env specialised_to in
                  match A.check_approx_for_closure approx with
                  | Wrong -> assert false
                  | Ok (value_closure, _, _) ->
                    wrapper_param, value_closure.closure_id
          in
          let binding_var = Variable.rename var in
          let defining_expr : Flambda.named =
            Project_var {
              closure;
              closure_id;
              var = Var_within_closure.wrap var;
            }
          in
          (binding_var, defining_expr)::bindings)
        closure_element_params_mapping
        []
    in
    let extra_args = List.map fst bindings_for_closure_element_params in
    let body : Flambda.t =
      Apply {
        func = new_fun_var;
        args = wrapper_params @ extra_args;
        kind = Direct (Closure_id.wrap new_fun_var);
        dbg = Debuginfo.none;
      }
    in
    Flambda_utils.bind ~bindings:bindings_for_closure_element_params ~body
  in
  let wrapper : Flambda.function_declaration =
    Flambda.create_function_declaration ~params:wrapper_params
      ~body:wrapper_body ~stub:true ~dbg:Debuginfo.none
  in
  let funs =
    Variable.Map.add new_fun_var rewritten_function_decl
        (Variable.Map.add fun_var wrapper funs)
  in
  funs, additional_specialised_args

let run env expr =
(*
  Format.eprintf "Before Unbox_closures: %a\n" Flambda.print expr;
*)
  let expr =
    Flambda_iterators.map_toplevel_sets_of_closures expr
      ~f:(fun (set_of_closures : Flambda.set_of_closures) ->
        let function_decls = set_of_closures.function_decls in
        let specialised_args = set_of_closures.specialised_args in
        let funs, additional_specialised_args =
          Variable.Map.fold (fun fun_var function_decl
                (funs, additional_specialised_args) ->
              rewrite_function_decl ~env ~function_decls ~fun_var
                ~function_decl ~specialised_args ~funs
                ~additional_specialised_args)
            function_decls.funs
            (Variable.Map.empty, Variable.Map.empty)
        in
        let specialised_args =
          Variable.Map.disjoint_union specialised_args
            additional_specialised_args
            ~eq:Variable.equal
        in
        let function_decls =
          Flambda.update_function_declarations function_decls ~funs
        in
        Flambda.create_set_of_closures
          ~function_decls
          ~free_vars:set_of_closures.free_vars
          ~specialised_args)
  in
(*
  Format.eprintf "After Unbox_closures: %a\n" Flambda.print expr;
*)
  expr
