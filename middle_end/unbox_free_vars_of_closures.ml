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

let run ~env ~set_of_closures =
  if !Clflags.classic_inlining then
    None
  else
    let funs, extracted_bindings, additional_free_vars, _total_benefit =
      Variable.Map.fold (fun fun_var function_decl
            (funs, extracted_bindings) ->
          let extracted =
            Extract_projections.from_function_decl ~env ~function_decl
              ~which_variables:set_of_closures.function_decls.free_vars
              ~set_of_closures
          in
          let function_decl =
            match extracted with
            | None -> function_decl
            | Some (new_function_body, extracted_bindings') ->
              Flambda.create_function_declaration ~params:function_decl.params
                ~body:new_function_body
                ~stub:function_decl.stub
                ~dbg:function_decl.dbg
                ~inline:function_decl.inline
                ~is_a_functor:function_decl.is_a_functor
          in
          let funs =
            Variable.Map.add fun_var function_decl funs
          in
          let extracted_bindings =
            Variable.Map.disjoint_union extracted_bindings extracted_bindings'
          in
          funs, extracted_bindings)
        set_of_closures.function_decls.funs
        (Variable.Map.empty, Variable.Map.empty)
    in
    let function_decls =
      Flambda.update_function_declarations function_decls ~funs
    in
    let free_vars =
      try
        Variable.Map.disjoint_union additional_free_vars
          set_of_closures.free_vars
          ~eq:Variable.equal
      with exn ->
        ...
    in
    let set_of_closures =
      Flambda.create_set_of_closures
        ~function_decls
        ~free_vars
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
