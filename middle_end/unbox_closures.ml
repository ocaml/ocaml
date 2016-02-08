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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module ASA = Augment_specialised_args
module W = ASA.What_to_specialise

module Transform = struct
  let pass_name = "unbox-closures"
  let variable_suffix = ""

  let precondition ~(set_of_closures : Flambda.set_of_closures) =
    !Clflags.unbox_closures
      (* CR mshinwell: think about these conditions some more *)
      && not (Variable.Map.is_empty set_of_closures.free_vars)
      && (Variable.Map.is_empty set_of_closures.specialised_args
        || Flambda_utils.contains_stub set_of_closures.function_decls)

  let what_to_specialise ~env:_ ~(set_of_closures : Flambda.set_of_closures) =
    let what_to_specialise = W.create ~set_of_closures in
    if not (precondition ~set_of_closures) then
      what_to_specialise
    else
      Flambda_iterators.fold_function_decls_ignoring_stubs set_of_closures
        ~init:what_to_specialise
        ~f:(fun ~fun_var ~function_decl:_ what_to_specialise ->
          let bound_by_the_closure =
            Flambda_utils.variables_bound_by_the_closure
              (Closure_id.wrap fun_var)
              set_of_closures.function_decls
          in
          Variable.Set.fold (fun inner_free_var what_to_specialise ->
              W.new_specialised_arg what_to_specialise
                ~fun_var ~group:inner_free_var
                ~definition:(Existing_inner_free_var inner_free_var))
            bound_by_the_closure
            what_to_specialise)
end

include ASA.Make (Transform)
