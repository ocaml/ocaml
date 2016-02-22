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
module E = Inline_and_simplify_aux.Env

module Transform = struct
  let pass_name = "unbox-closures"
  let variable_suffix = ""

  let precondition ~env ~(set_of_closures : Flambda.set_of_closures) =
    !Clflags.unbox_closures <> None
      && not (E.at_toplevel env)
      && not (Variable.Map.is_empty set_of_closures.free_vars)

  let what_to_specialise ~env ~(set_of_closures : Flambda.set_of_closures) =
    let what_to_specialise = W.create ~set_of_closures in
    if not (precondition ~env ~set_of_closures) then
      what_to_specialise
    else begin
      let round = E.round env in
      let num_closure_vars = Variable.Map.cardinal set_of_closures.free_vars in
      let module B = Inlining_cost.Benefit in
      let saved_by_not_building_closure =
        (* For the moment assume that we're going to cause all functions in the
           set to become closed. *)
        B.remove_prims (B.remove_alloc B.zero) num_closure_vars
      in
      Flambda_iterators.fold_function_decls_ignoring_stubs set_of_closures
        ~init:what_to_specialise
        ~f:(fun ~fun_var ~(function_decl : Flambda.function_declaration)
              what_to_specialise ->
          let body_size = Inlining_cost.lambda_size function_decl.body in
          let new_size =
            match !Clflags.unbox_closures with
            | None -> assert false
            | Some "nodcs_none" -> 0
            | Some "nodcs_smallthresh"
            | Some "dcs_smallthresh"
            | Some "maybedcs_smallthresh" -> body_size
            | Some "nodcs_bigthresh"
            | Some "dcs_bigthresh" -> body_size / 15
            | Some _ -> failwith "Bad setting for -unbox-closures"
          in
          let has_benefit =
            let module W = Inlining_cost.Whether_sufficient_benefit in
            let wsb =
              W.create_estimate ~original_size:0
                ~toplevel:false
                ~branch_depth:0
                ~new_size:(new_size + 1)
                ~benefit:saved_by_not_building_closure
                ~lifting:false
                ~round
            in
            W.evaluate wsb
          in
          let make_surrogate =
            match !Clflags.unbox_closures with
            | None -> assert false
            | Some "nodcs_none"
            | Some "nodcs_smallthresh"
            | Some "nodcs_bigthresh" -> false
            | Some "dcs_smallthresh"
            | Some "dcs_bigthresh" -> true
            | Some "maybedcs_smallthresh" -> has_benefit
            | Some _ -> assert false
          in
          let what_to_specialise =
            if make_surrogate then
              W.make_direct_call_surrogate_for what_to_specialise ~fun_var
            else
              what_to_specialise
          in
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
end

include ASA.Make (Transform)
