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

type result = {
  function_offsets : int Closure_id.Map.t;
  free_variable_offsets : int Var_within_closure.Map.t;
}

let add_closure_offsets
      { function_offsets; free_variable_offsets }
      ({ function_decls; free_vars } : Flambda.set_of_closures) =
  (* Format.eprintf "add_closure_offsets:@ %a\n" *)
  (*   Flambda.print_set_of_closures set_of_closures; *)
  (* Build the table mapping the functions declared by the set of closures
     to the positions of their individual "infix" closures inside the runtime
     closure block.  (All of the environment entries will come afterwards.) *)
  let assign_function_offset id function_decl (map, env_pos) =
    let pos = env_pos + 1 in
    let env_pos =
      let arity = Flambda_utils.function_arity function_decl in
      env_pos
        + 1  (* GC header; either [Closure_tag] or [Infix_tag] *)
        + 1  (* full application code pointer *)
        + 1  (* arity *)
        + (if arity > 1 then 1 else 0)  (* partial application code pointer *)
    in
    let map = Closure_id.Map.add (Closure_id.wrap id) pos map in
    (map, env_pos)
  in
  let function_offsets, free_variable_pos =
    Variable.Map.fold assign_function_offset
      function_decls.funs (function_offsets, -1)
  in
  (* CR mshinwell: I'm not sure if this comment is still accurate *)
  (* Adds the mapping of free variables to their offset. It is not
     used inside the body of the function: it is directly
     substituted here. But if the function is inlined, it is
     possible that the closure is accessed from outside its body. *)
  let assign_free_variable_offset var _ (map, pos) =
    let var_within_closure = Var_within_closure.wrap var in
    assert (not (Var_within_closure.Map.mem var_within_closure map));
    let map = Var_within_closure.Map.add var_within_closure pos map in
    (map, pos + 1)
  in
  let free_variable_offsets, _ =
    Variable.Map.fold assign_free_variable_offset
      free_vars (free_variable_offsets, free_variable_pos)
  in
  { function_offsets;
    free_variable_offsets;
  }

let compute (program:Flambda.program) =
  (* Format.eprintf "Closure_offsets.compute@ \n"; *)
  (* let sets = Lifted_flambda_utils.sets_of_closures lifted_constants in *)
  (* List.iter (fun (set_of_closures : Flambda.set_of_closures) -> *)
  (*     Format.eprintf "Closures_offsets.compute set: %a @ " *)
  (*       Set_of_closures_id.print set_of_closures.function_decls.set_of_closures_id) *)
  (*   sets; *)
  let init : result =
    { function_offsets = Closure_id.Map.empty;
      free_variable_offsets = Var_within_closure.Map.empty;
    }
  in
  let r =
    List.fold_left add_closure_offsets
      init (Flambda_utils.all_sets_of_closures program)
  in
  (* Format.eprintf "Closures_offsets results:@ function_offsets:@ %a@." *)
  (*   (Closure_id.Map.print Format.pp_print_int) r.function_offsets; *)
  r
