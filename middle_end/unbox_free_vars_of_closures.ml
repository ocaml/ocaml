(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

let pass_name = "unbox-free-vars-of-closures"
let () = Pass_manager.register ~pass_name

type result = {
  projection_defns : projection_defns;
  new_function_body : Flambda.expr;
  new_inner_to_new_outer_vars : Variable.t Variable.Map.t;
  benefit : Inlining_cost.Benefit.t;
}

let run ~env ~set_of_closures =
  if !Clflags.classic_inlining then
    None
  else
    let funs, projection_defns, additional_free_vars, _total_benefit =
      Variable.Map.fold (fun fun_var function_decl
            (funs, projection_defns, additional_free_vars) ->
          let extracted =
            Extract_projections.from_function_decl ~env ~function_decl
              ~which_variables:set_of_closures.function_decls.free_vars
              ~set_of_closures
          in
          let function_decl =
            match extracted with
            | None -> function_decl
            | Some result ->
              Flambda.create_function_declaration ~params:function_decl.params
                ~body:result.new_function_body
                ~stub:function_decl.stub
                ~dbg:function_decl.dbg
                ~inline:function_decl.inline
                ~is_a_functor:function_decl.is_a_functor
          in
          let funs = Variable.Map.add fun_var function_decl funs in
          let projection_defns = projection_defns @ result.projection_defns in
          let additional_free_vars =
            try
              Variable.Map.disjoint_union additional_free_vars
                result.new_inner_to_new_outer_vars
                ~eq:Variable.equal
            with _exn ->
              Misc.fatal_errorf "Unbox_free_vars_of_closures: non-disjoint \
                  [free_vars] sets: %a vs. %a"
                Variable.Set.print additional_free_vars
                Variable.Set.print set_of_closures.free_vars
          in
          funs, projection_defns, additional_free_vars)
        set_of_closures.function_decls.funs
        (Variable.Map.empty, [], set_of_closures.free_vars)
    in
    let function_decls =
      Flambda.update_function_declarations function_decls ~funs
    in
    let set_of_closures =
      Flambda.create_set_of_closures ~function_decls ~free_vars
        ~specialised_args:set_of_closures.specialised_args
    in
    let expr =
      Variable.Map.fold Flambda.create_let extracted_bindings
        (Flambda_utils.name_expr (Set_of_closures set_of_closures)
          ~name:"unbox_free_vars_of_closures")
    in
    Some expr

let run ~env ~set_of_closures =
  Pass_manager.with_dump ~pass_name ~input:set_of_closures
    ~print_input:Flambda.print_set_of_closures
    ~print_output:Flambda.print
    ~f:(fun () -> run ~env ~set_of_closures)
