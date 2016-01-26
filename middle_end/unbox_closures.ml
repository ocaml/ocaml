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
  let pass_name = "unbox-closures"
  let variable_suffix = "_unbox_closures"

  let precondition ~(set_of_closures : Flambda.set_of_closures) =
    !Clflags.unbox_closures
      && not (Variable.Map.is_empty set_of_closures.free_vars)
      && (Variable.Map.is_empty set_of_closures.specialised_args
        || Flambda_utils.contains_stub set_of_closures.function_decls)

  let what_to_specialise ~closure_id ~function_decl
        ~(set_of_closures : Flambda.set_of_closures)
        : ASA.what_to_specialise option =
    let free_vars =
      let bound_by_this_closure =
        Flambda_utils.variables_bound_by_the_closure closure_id
          set_of_closures.function_decls
      in
      Variable.Map.filter set_of_closures
        ~f:(fun inner_free_var _outer_free_var ->
          Variable.Set.mem inner_free_var bound_by_this_closure)
    in
    if Variable.Map.cardinal free_vars < 1 then
      None
    else
      (* Just in case more than one inner free variable maps to the same
         outer free variable, create fresh "new outer vars" at all times,
         rather than putting the existing free variables as the "new outer
         vars".

         Since the existing free variables will become the "new inner vars",
         this map is directly the "new inner to new outer vars" map. *)
      let new_inner_to_new_outer_vars =
        Variable.Map.map (fun outer_var ->
            Variable.rename ~append:variable_suffix)
          free_vars
      in
      let new_function_body =
        Flambda_utils.toplevel_substitution new_parameters function_decl.body
      in
      if new_function_body == function_decl.body then
        None
      else
        let new_specialised_args_indexed_by_new_outer_vars =
          Variable.Map.fold (fun inner_free_var outer_free_var
                new_specialised_args_indexed_by_new_outer_vars ->
              let new_outer_var =
                match
                  Variable.Map.find inner_free_var new_inner_to_new_outer_vars
                with
                | exception Not_found -> assert false
                | new_outer_var -> new_outer_var
              in
              let defining_expr : Flambda.expr = Var outer_free_var in
              Variable.Map.add new_outer_var defining_expr
                new_specialised_args_indexed_by_new_outer_vars)
            free_vars
            Variable.Map.empty
        in
        let what_to_specialise : ASA.what_to_specialise = {
          new_function_body;
          (* One free variable maps to one specialised argument; there is no
             grouping, hence the singleton list. *)
          new_specialised_args_indexed_by_new_outer_vars =
            [new_specialised_args_indexed_by_new_outer_vars];
          new_inner_to_new_outer_vars;
        }
        in
        Some what_to_specialise
end

include ASA.Make_pass (Transform)
