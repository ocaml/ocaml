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
let () = Pass_wrapper.register ~pass_name

let run ~env ~(set_of_closures : Flambda.set_of_closures) =
  if !Clflags.classic_inlining then
    None
  else
    let funs, projection_defns, free_vars, done_something =
      Variable.Map.fold (fun fun_var
            (function_decl : Flambda.function_declaration)
            (funs, projection_defns, additional_free_vars, done_something) ->
          if function_decl.stub then
            let funs = Variable.Map.add fun_var function_decl funs in
            funs, projection_defns, additional_free_vars, done_something
          else
            let extracted =
              Extract_projections.from_function_decl ~env ~function_decl
                ~which_variables:set_of_closures.free_vars
            in
            match extracted with
            | None ->
              let funs = Variable.Map.add fun_var function_decl funs in
              funs, projection_defns, additional_free_vars, done_something
            | Some extracted ->
              let function_decl =
                Flambda.create_function_declaration
                  ~params:function_decl.params
                  ~body:function_decl.body
                  ~stub:function_decl.stub
                  ~dbg:function_decl.dbg
                  ~inline:function_decl.inline
                  ~is_a_functor:function_decl.is_a_functor
              in
              let funs = Variable.Map.add fun_var function_decl funs in
              let projection_defns =
                Variable.Map.union (fun _var defns1 defns2 ->
                    Some (Variable.Map.disjoint_union defns1 defns2))
                  projection_defns
                  extracted.projection_defns_indexed_by_outer_vars
              in
              (* CR mshinwell: share code with
                 Unbox_specialised_args to deal with the deduping *)
              let filter_outer_vars = ref Variable.Set.empty in
              let new_inner_to_new_outer_vars =
                Variable.Map.filter (fun _var
                          (spec_to : Flambda.specialised_to) ->
                    let already_present =
                      Variable.Map.exists (fun _var
                                (spec_to' : Flambda.specialised_to) ->
                          match spec_to.projectee, spec_to'.projectee with
                          | Some (_, projectee), Some (_, projectee') ->
                            let equal = Projectee.equal projectee projectee' in
                            if equal then begin
                              filter_outer_vars :=
                                Variable.Set.add spec_to.var
                                  !filter_outer_vars;
                              true
                            end else begin
                              false
                            end
                          | Some _, None
                          | None, Some _
                          | None, None -> false)
                        set_of_closures.free_vars
                    in
                    not already_present)
                  extracted.new_inner_to_new_outer_vars
              in
              let projection_defns =
                Variable.Map.filter_map
                  projection_defns
                  ~f:(fun _projecting_from indexed_by_outer_vars ->
                    let indexed_by_outer_vars =
                      Variable.Map.filter (fun outer_var _defn ->
                          not (Variable.Set.mem outer_var !filter_outer_vars))
                        indexed_by_outer_vars
                    in
                    if Variable.Map.cardinal indexed_by_outer_vars < 1 then
                      None
                    else
                      Some indexed_by_outer_vars)
              in
              let additional_free_vars =
                try
                  Variable.Map.disjoint_union additional_free_vars
                    new_inner_to_new_outer_vars
                    ~eq:Flambda.equal_specialised_to
                with _exn ->
                  Misc.fatal_errorf "Unbox_free_vars_of_closures: non-disjoint \
                      [free_vars] sets: %a vs. %a"
                    (Variable.Map.print Flambda.print_specialised_to)
                      additional_free_vars
                    (Variable.Map.print Flambda.print_specialised_to)
                      set_of_closures.free_vars
              in
              funs, projection_defns, additional_free_vars, true)
        set_of_closures.function_decls.funs
        (Variable.Map.empty, Variable.Map.empty,
          set_of_closures.free_vars, false)
    in
    if not done_something then
      None
    else
      (* CR-someday mshinwell: could consider doing the grouping thing
         similar to Augment_specialised_args *)
      let num_free_vars_before =
        Variable.Map.cardinal set_of_closures.free_vars
      in
      let num_free_vars_after =
        Variable.Map.cardinal free_vars
      in
      (* Don't let the closure grow too large. *)
      if num_free_vars_after > 2 * num_free_vars_before then
        None
      else
        let function_decls =
          Flambda.update_function_declarations set_of_closures.function_decls
            ~funs
        in
        let set_of_closures =
          Flambda.create_set_of_closures ~function_decls ~free_vars
            ~specialised_args:set_of_closures.specialised_args
        in
        let expr =
          Variable.Map.fold (fun _projected_from projection_defns expr ->
              Variable.Map.fold Flambda.create_let projection_defns expr)
            projection_defns
            (Flambda_utils.name_expr (Set_of_closures set_of_closures)
              ~name:"unbox_free_vars_of_closures")
        in
        Some expr

let run ~env ~set_of_closures =
  Pass_wrapper.with_dump ~pass_name ~input:set_of_closures
    ~print_input:Flambda.print_set_of_closures
    ~print_output:Flambda.print
    ~f:(fun () -> run ~env ~set_of_closures)
