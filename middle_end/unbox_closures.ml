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
    !Clflags.unbox_closures
      && not (E.at_toplevel env)
      && not (Variable.Map.is_empty set_of_closures.free_vars)

  let benefit_outweighs_code_size ~env
        ~(set_of_closures : Flambda.set_of_closures) =
    (* Somewhat approximate assumptions made:
       1. We will cause all closures in the set to become closed, in the end.
       2. All functions in the set have more than one argument and stay
          that way after the transformation. *)
    let round = E.round env in
    let num_closure_vars = Variable.Map.cardinal set_of_closures.free_vars in
    let module B = Inlining_cost.Benefit in
    let saved_by_not_building_closure =
      B.remove_prims (B.remove_alloc B.zero)
        (3 + num_closure_vars)  (* "3" = code pointers & arity *)
    in
    let expr : Flambda.expr =
      Flambda_utils.name_expr (Set_of_closures set_of_closures)
        ~name:"temporary"
    in
    let module W = Inlining_cost.Whether_sufficient_benefit in
    let wsb =
      W.create_estimate ~original_size:0
        ~toplevel:false
        ~branch_depth:0
        ~new_size:(Inlining_cost.lambda_size expr)
        ~benefit:saved_by_not_building_closure
        ~lifting:true
        ~round
    in
    Printf.eprintf "Unbox_closures: %s\n%!" (W.to_string wsb);
    W.evaluate wsb

  let what_to_specialise ~env ~(set_of_closures : Flambda.set_of_closures) =
    let what_to_specialise =
      W.create ~set_of_closures ~make_direct_call_surrogates:true
    in
    if not (precondition ~env ~set_of_closures)
      || not (benefit_outweighs_code_size ~env ~set_of_closures)
    then
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
