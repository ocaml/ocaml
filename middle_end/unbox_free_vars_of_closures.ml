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
let () = Pass_manager.register ~pass_name

let precondition ~var ~(set_of_closures : Flambda.set_of_closures) =
  Variable.Map.mem var set_of_closures.function_decls.free_vars

let run ~env ~set_of_closures =
  if !Clflags.classic_inlining then
    None
  else
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
