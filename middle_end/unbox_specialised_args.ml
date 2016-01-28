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

module ASA = Augment_specialised_args

module Transform = struct
  let pass_name = "unbox-specialised-args"
  let variable_suffix = "_unbox_spec_args"

  type mini_extracted = {  (* Subset of [Extract_projections.result] *)
    projection_defns_indexed_by_outer_vars :
      Extract_projections.projection_defns;
    new_inner_to_new_outer_vars : Flambda.specialised_to Variable.Map.t;
  }

  type user_data = mini_extracted option Variable.Map.t

  let collect_projections ~backend ~env
        ~(set_of_closures : Flambda.set_of_closures) =
    let projections_by_function =
      Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
          let extracted =
            Extract_projections.from_function_decl ~env ~function_decl
              ~which_variables:set_of_closures.specialised_args
          in
          function_decl, extracted)
        set_of_closures.function_decls.funs
    in
    (* CR-soon mshinwell: consider caching the Invariant_params *relation*
       as well as the "_in_recursion" map *)
    let invariant_params_flow =
      Invariant_params.invariant_param_sources set_of_closures.function_decls
        ~backend
    in
    (* If for function [f] we would extract a projection expression [e]
       from some specialised argument [x] of [f], and we know from
       [Invariant_params] that [x] flows to the specialised argument [y]
       of another function [g], then add [e] with [y] substituted for [x]
       throughout as a newly-specialised argument for [g].  This should help
       reduce the number of simplification rounds required for
       mutually-recursive functions.  (If you don't like "fold", stop here.) *)
    let projections_by_function =
      let specialised_args = set_of_closures.specialised_args in
      Variable.Map.fold (fun fun_var (function_decl, extracted) result ->
          Variable.Map.fold (fun projected_from
                    projection_defns_indexed_by_outer_vars result ->
              assert (Variable.Map.mem projected_from
                set_of_closures.specialised_args);
              match Variable.Map.find projected_from invariant_params_flow with
              | exception Not_found ->
                Misc.fatal_errorf "No invariant params flow for variable %a \
                    despite it being a specialised arg"
                  Variable.print projected_from
              | flow ->
                (* For each [target_arg] equal to [projected_from] in
                   another function known as [target_fun_var], add all of
                   the projections found in
                   [projection_defns_indexed_by_outer_vars] having freshened
                   those (new) outer vars.  We also need to freshen the new
                   inner vars. *)
                Variable.Pair.Set.fold (fun (target_fun_var, target_arg)
                      result ->
                    assert (Variable.Map.mem target_arg specialised_args);
                    let new_outer_vars_freshening =
                      List.fold_left (fun freshening
                                (spec_to : Flambda.specialised_to) ->
                          let outer_var = spec_to.var in
                          let new_outer_var =
                            Variable.rename outer_var ~append:variable_suffix
                          in
                          Variable.Map.add outer_var new_outer_var freshening)
                        Variable.Map.empty
                        (Variable.Map.data
                          extracted.new_inner_to_new_outer_vars)
                    in
                    let freshen_outer_var outer_var =
                      match
                        Variable.Map.find outer_var new_outer_vars_freshening
                      with
                      | exception Not_found -> assert false
                      | new_outer_var -> new_outer_var
                    in
                    let new_inner_to_new_outer_vars =
                      Variable.Map.fold (fun inner_var
                                (spec_to : Flambda.specialised_to)
                                new_inner_to_new_outer_vars ->
                          let new_inner_var =
                            Variable.rename inner_var ~append:variable_suffix
                          in
                          let outer_var = spec_to.var in
                          let new_outer_var = freshen_outer_var outer_var in
                          Variable.Map.add outer_var new_outer_var
                            new_inner_to_new_outer_vars)
                        extracted.new_inner_to_new_outer_vars
                        Variable.Map.empty
                    in
                    let projection_defns_indexed_by_outer_vars =
                      (* The defining expressions of these new projections for
                         [target_fun_var] are the same as for [fun_var] save
                         that we must rewrite occurrences of [projected_from]
                         to [target_arg]. *)
                      let fun_var_substitution =
                        Variable.Map.add projected_from target_arg
                          Variable.Map.empty
                      in
                      Variable.Map.fold (fun outer_var defining_expr
                                projection_defns_indexed_by_outer_vars ->
                          let new_outer_var =
                            match
                              Variable.Map.find outer_var
                                new_outer_vars_freshening
                            with
                            | exception Not_found -> assert false
                            | new_outer_var -> new_outer_var
                          in
                          let defining_expr =
                            Flambda_utils.toplevel_substitution_named
                              fun_var_substitution defining_expr
                          in
                          Variable.Map.add new_outer_var defining_expr
                            projection_defns_indexed_by_outer_vars)
                        projection_defns_indexed_by_outer_vars
                        Variable.Map.empty
                    in
                    let existing : mini_extracted =
                      match Variable.Map.find target_fun_var result with
                      | exception Not_found -> 
                        { projection_defns_indexed_by_outer_vars =
                            Variable.Map.empty;
                          new_inner_to_new_outer_vars = Variable.Map.empty;
                        }
                      | extracted -> extracted
                    in
                    let extracted =
                      { projection_defns_indexed_by_outer_vars =
                          Variable.Map.disjoint_union
                            projection_defns_indexed_by_outer_vars
                            existing.projection_defns_indexed_by_outer_vars;
                        new_inner_to_new_outer_vars =
                          Variable.Map.disjoint_union
                            new_inner_to_new_outer_vars
                            existing.new_inner_to_new_outer_vars;
                      }
                    in
                    Variable.Map.add target_fun_var extracted result)
                  flow)
            extracted.projection_defns_indexed_by_outer_vars
            result)
        result
        projections_by_function
    in
    Some projections_by_function

  let precondition ~backend ~env ~(set_of_closures : Flambda.set_of_closures) =
    let is_ok =
      (* !Clflags.unbox_specialised_args *) true
        && not (Variable.Map.is_empty set_of_closures.specialised_args)
    in
    if not is_ok then None
    else Some (collect_projections ~backend ~env ~set_of_closures)

  let what_to_specialise ~env ~closure_id ~function_decl
        ~(set_of_closures : Flambda.set_of_closures)
        ~user_data:projections_by_function
        : ASA.what_to_specialise option =
    let fun_var = Closure_id.unwrap closure_id in
    match Variable.Map.find fun_var projections_by_function with
    | exception Not_found -> assert false
    | None -> None
    | Some (extracted : mini_extracted) ->
      let what_to_specialise : ASA.what_to_specialise = {
        (* All of the rewrites in the body will be taken care of by
           [Inline_and_simplify] upon detection of projection expressions
           and examination of the specialised argument map. *)
        new_function_body = function_decl.body;
        removed_free_vars = Variable.Set.empty;
        new_specialised_args_indexed_by_new_outer_vars =
          Variable.Map.data extracted.projection_defns_indexed_by_outer_vars;
        new_inner_to_new_outer_vars = extracted.new_inner_to_new_outer_vars;
      }
      in
      Some what_to_specialise
end

include ASA.Make (Transform)
