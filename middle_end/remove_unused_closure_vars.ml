(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** A variable in a closure can either be used by the closure itself
    or by an inlined version of the function. *)
let remove_unused_closure_variables tree =
  let used_vars_within_closure, used_closure_ids =
    let used = ref Var_within_closure.Set.empty in
    let used_fun = ref Closure_id.Set.empty in
    let aux_named (named : Flambda.named) =
      match named with
      | Project_closure { set_of_closures = _; closure_id } ->
        used_fun := Closure_id.Set.add closure_id !used_fun
      | Project_var { closure_id; var } ->
        used := Var_within_closure.Set.add var !used;
        used_fun := Closure_id.Set.add closure_id !used_fun
      | Move_within_set_of_closures { closure = _; start_from; move_to } ->
        used_fun := Closure_id.Set.add start_from !used_fun;
        used_fun := Closure_id.Set.add move_to !used_fun
      | Symbol _ | Const _ | Set_of_closures _ | Prim _ | Expr _ -> ()
    in
    Flambda_iterators.iter_named aux_named tree;
    !used, !used_fun
  in
  let aux_named (named : Flambda.named) : Flambda.named =
    match named with
    | Set_of_closures ({ function_decls; free_vars; _ } as closure) ->
      let all_free_vars =
        Variable.Map.fold (fun _ { Flambda. free_variables } acc ->
            Variable.Set.union free_variables acc)
          function_decls.funs
          Variable.Set.empty
      in
      let free_vars =
        Variable.Map.filter (fun id _var ->
            Variable.Set.mem id all_free_vars
              || Var_within_closure.Set.mem (Var_within_closure.wrap id)
                used_vars_within_closure)
          free_vars
      in
      let funs =
        Variable.Map.filter (fun fun_id _ ->
            Variable.Set.mem fun_id all_free_vars
              || Closure_id.Set.mem (Closure_id.wrap fun_id) used_closure_ids)
          function_decls.funs
      in
      let function_decls = { function_decls with funs } in
      Set_of_closures { closure with free_vars; function_decls }
    | e -> e
  in
  Flambda_iterators.map_named aux_named tree

