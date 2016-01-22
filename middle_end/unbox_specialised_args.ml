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

let pass_name = "unbox-specialised-args"
let () = Pass_manager.register ~pass_name

module Transform = struct
  let precondition ~(set_of_closures : Flambda.set_of_closures) =
    !Clflags.unbox_specialised_args
      && not (Variable.Map.is_empty set_of_closures.specialised_args)

  let what_to_specialise ~names_of_params_to_use_in_definitions:_
        ~closure_id ~function_decl
        ~(set_of_closures : Flambda.set_of_closures) =
        : ASA.what_to_specialise option =

end

let precondition ~var ~(set_of_closures : Flambda.set_of_closures) =
  Variable.Map.mem var set_of_closures.function_decls.specialised_args

let run ~env ~set_of_closures =
  let set_of_closures, lifted_bindings =
    Extract_projections.from_set_of_closures ~env ~precondition
      ~set_of_closures
  in
  if Variable.Map.is_empty lifted_bindings then
    None
  else


    let expr =
      Variable.Map.fold Flambda.create_let lifted_bindings
        (Flambda_utils.name_expr (Set_of_closures set_of_closures)
          ~name:"unbox_free_vars_of_closures")
    in
    Some expr

let run ~env ~set_of_closures =
  Pass_manager.with_dump ~pass_name ~input:set_of_closures
    ~print_input:Flambda.print_set_of_closures
    ~print_output:Flambda.print
    ~f:(fun () -> run ~env ~set_of_closures)
