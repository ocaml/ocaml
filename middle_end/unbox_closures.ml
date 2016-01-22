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

  let precondition ~(set_of_closures : Flambda.set_of_closures) =
    !Clflags.unbox_closures
      && not (Variable.Map.is_empty set_of_closures.free_vars)
      && (Variable.Map.is_empty set_of_closures.specialised_args
        || Flambda_utils.contains_stub set_of_closures.function_decls)

  let what_to_specialise ~names_of_params_to_use_in_definitions:_
        ~closure_id ~function_decl
        ~(set_of_closures : Flambda.set_of_closures) =
        : ASA.what_to_specialise option =
    let free_vars =
      Flambda_utils.variables_bound_by_the_closure closure_id
        set_of_closures.function_decls
    in
    if Variable.Set.cardinal free_vars < 1 then
      None
    else
      let new_parameters =
        Variable.Map.of_set
          (fun var -> Variable.rename ~append:"_unboxed_bound" var)
          free_vars
      in
      let new_function_body =
        Flambda_utils.toplevel_substitution new_parameters function_decl.body
      in
      if new_function_body == function_decl.body then
        None
      else
        let new_specialised_args =
          Variable.Map.fold (fun free_var new_parameter new_specialised_args ->
              let new_specialised_arg : ASA.new_specialised_arg =
                (* Since [free_var] isn't going to be a parameter of the
                   new wrapper, [names_of_params_to_use_in_definitions] is
                   irrelevant. *)
                { definition = Flambda.Var free_var;
                }
              in
              Variable.Map.add new_parameter definition new_specialised_args)
            new_parameters
            Variable.Map.empty
        in
        let what_to_specialise : ASA.what_to_specialise = {
          new_function_body;
          (* One free variable maps to one specialised argument; there is no
             grouping, hence the singleton list. *)
          new_specialised_args = [new_specialised_args];
        }
        in
        Some what_to_specialise
end

include ASA.Make_pass (Transform)
