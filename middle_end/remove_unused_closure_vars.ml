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

module A = Simple_value_approx
module C = Inlining_cost

(** A variable in a closure can either be used by the closure itself
    or by an inlined version of the function. *)
let remove_unused_closure_variables tree =
  let used_variable_within_closure,
      used_closure_id =
    let used = ref Var_within_closure.Set.empty in
    let used_fun = ref Closure_id.Set.empty in
    let aux (expr : _ Flambda.t) =
      match expr with
      | Fvar_within_closure({ var; closure_id }, _) ->
          used := Var_within_closure.Set.add var !used;
          used_fun := Closure_id.Set.add closure_id !used_fun;
      | Fselect_closure({ closure_id; relative_to }, _) ->
          used_fun := Closure_id.Set.add closure_id !used_fun;
          begin match relative_to with
          | None -> ()
          | Some relative_to ->
              used_fun := Closure_id.Set.add relative_to !used_fun
          end
      | _ -> ()
    in
    Flambdaiter.iter aux tree;
    !used, !used_fun
  in
  let aux (expr : _ Flambda.t) : _ Flambda.t =
    match expr with
    | Fset_of_closures ({ function_decls; free_vars } as closure, eid) ->
       let all_free_var =
         Variable.Map.fold
           (fun _ { Flambda. free_variables } acc ->
             Variable.Set.union free_variables acc)
           function_decls.funs
           Variable.Set.empty in
       let free_vars =
         Variable.Map.filter (fun id expr ->
             Variable.Set.mem id all_free_var
             || Var_within_closure.Set.mem (Var_within_closure.wrap id)
               used_variable_within_closure
             || not (Effect_analysis.no_effects expr))
           free_vars in
       let function_decls =
         { function_decls with
           funs = Variable.Map.filter (fun fun_id _ ->
               Variable.Set.mem fun_id all_free_var
               || Closure_id.Set.mem (Closure_id.wrap fun_id)
                 used_closure_id)
               function_decls.funs } in
       Fset_of_closures ({ closure with free_vars; function_decls }, eid)
    | e -> e
  in
  Flambdaiter.map aux tree
