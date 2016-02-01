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

  type user_data = unit

  let precondition ~backend:_ ~env:_
        ~(set_of_closures : Flambda.set_of_closures) =
    let is_ok =
      !Clflags.unbox_closures
        && not (Variable.Map.is_empty set_of_closures.free_vars)
        && (Variable.Map.is_empty set_of_closures.specialised_args
          || Flambda_utils.contains_stub set_of_closures.function_decls)
    in
    if not is_ok then None else Some ()

  let what_to_specialise ~env:_ ~closure_id
        ~(function_decl : Flambda.function_declaration)
        ~(set_of_closures : Flambda.set_of_closures)
        ~user_data:_
        : ASA.what_to_specialise option =
    let free_vars =
      let bound_by_this_closure =
        Flambda_utils.variables_bound_by_the_closure closure_id
          set_of_closures.function_decls
      in
      Variable.Map.filter (fun inner_free_var _outer_free_var ->
          Variable.Set.mem inner_free_var bound_by_this_closure)
        set_of_closures.free_vars
    in
    if function_decl.stub || Variable.Map.cardinal free_vars < 1 then
      None
    else
      (* Just in case more than one inner free variable maps to the same
         outer free variable, create fresh "new outer vars" at all times,
         rather than putting the existing free variables as the "new outer
         vars".
         It might look like the existing free variables will become the
         "new inner vars", but that cannot be the case: we need fresh new
         inner vars because, until simplification has been completed and
         the stub has been eliminated, we need one variable as an inner
         free var and another (equal) variable as an inner specialised
         argument.  (The former for the stub and the latter for the main
         function.) *)
      let existing_inner_free_vars_to_new_inner_vars =
        Variable.Map.fold (fun inner_var _outer_var renaming ->
            let new_inner_var =
              Variable.rename inner_var ~append:variable_suffix
            in
            assert (not (Variable.Map.mem inner_var renaming));
            Variable.Map.add inner_var new_inner_var renaming)
          free_vars
          Variable.Map.empty
      in
      let new_inner_to_new_outer_vars =
        Variable.Map.fold (fun inner_var (outer_var : Flambda.specialised_to)
                  new_inner_to_new_outer_vars ->
            let new_inner_var =
              match
                Variable.Map.find inner_var
                  existing_inner_free_vars_to_new_inner_vars
              with
              | exception Not_found -> assert false
              | new_inner_var -> new_inner_var
            in
            let spec_to : Flambda.specialised_to =
              let var =
                Variable.rename outer_var.var ~append:variable_suffix
              in
              { var;
                projectee = None;
              }
            in
            Variable.Map.add new_inner_var spec_to new_inner_to_new_outer_vars)
          free_vars
          Variable.Map.empty
      in
      let new_function_body =
        Flambda_utils.toplevel_substitution
          existing_inner_free_vars_to_new_inner_vars
          function_decl.body
      in
      let new_specialised_args_indexed_by_new_outer_vars =
        Variable.Map.fold (fun inner_free_var
              (outer_free_var : Flambda.specialised_to)
              new_specialised_args_indexed_by_new_outer_vars ->
            let new_inner_var =
              match
                Variable.Map.find inner_free_var
                  existing_inner_free_vars_to_new_inner_vars
              with
              | exception Not_found -> assert false
              | new_inner_var -> new_inner_var
            in
            let new_outer_var =
              match
                Variable.Map.find new_inner_var new_inner_to_new_outer_vars
              with
              | exception Not_found -> assert false
              | (spec_to : Flambda.specialised_to) -> spec_to.var
            in
            let defining_expr : Flambda.named =
              Expr (Var outer_free_var.var)
            in
            Variable.Map.add new_outer_var defining_expr
              new_specialised_args_indexed_by_new_outer_vars)
          free_vars
          Variable.Map.empty
      in
      let what_to_specialise : ASA.what_to_specialise = {
        new_function_body;
        removed_free_vars = Variable.Map.keys free_vars;
        (* One free variable maps to one specialised argument; there is no
           grouping, hence the singleton list. *)
        new_specialised_args_indexed_by_new_outer_vars =
          [new_specialised_args_indexed_by_new_outer_vars];
        new_inner_to_new_outer_vars;
      }
      in
      Some what_to_specialise
end

include ASA.Make (Transform)
