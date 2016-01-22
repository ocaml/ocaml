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

module Transform = struct
  let pass_name = "unbox-specialised-args"
  let variable_suffix = "unbox_spec_args"

  let precondition ~(set_of_closures : Flambda.set_of_closures) =
    !Clflags.unbox_specialised_args
      && not (Variable.Map.is_empty set_of_closures.specialised_args)

  let what_to_specialise ~env ~closure_id ~function_decl
        ~(set_of_closures : Flambda.set_of_closures) =
        : ASA.what_to_specialise option =
    let extracted =
      Extract_projections.from_function_decl ~env ~function_decl
        ~which_variables:set_of_closures.function_decls.specialised_args
        ~set_of_closures
    in
    match extracted with
    | None -> None
    | Some (new_function_body, extracted_projections,
        additional_free_vars, total_benefit) ->
      let new_specialised_args =
        (* The extracted projections still reference the inner specialised
           args.  They need to be rewritten to reference the outer ones,
           since we are about to lift them. *)
        Flambda_utils.toplevel_substitution set_of_closures.specialised_args
          extracted_projections
      in
      let what_to_specialise : ASA.what_to_specialise = {
        new_function_body;
        additional_free_vars;
        new_specialised_args;
        total_benefit;
      }
      in
      Some what_to_specialise
end

include ASA.Make (Transform)
