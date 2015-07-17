

type result = {
  code_pointer_offsets : int Closure_id.Map.t;
  free_variable_offsets : int Var_within_closure.Map.t;
}

let add_closure_offsets
    kind
    { code_pointer_offsets; free_variable_offsets }
    ({ function_decls; free_vars } : Flambda.set_of_closures) =

  (* build the table mapping the function to the offset of its code
     pointer inside the closure value *)
  let aux_fun_offset id func (map,env_pos) =
    let pos = env_pos + 1 in
    let arity = Flambda_utils.function_arity func in
    let env_pos =
      env_pos + 1 + (if arity <> 1 then 3 else 2)
    in
    let map = Closure_id.Map.add (Closure_id.wrap id) pos map in
    (map,env_pos)
  in
  let code_pointer_offsets, fv_pos =
    Variable.Map.fold aux_fun_offset function_decls.funs
      (code_pointer_offsets, -1)
  in

  (* Adds the mapping of free variables to their offset. It is not
     used inside the body of the function: it is directly
     substituted here. But if the function is inlined, it is
     possible that the closure is accessed from outside its body. *)
  let aux_fv_offset var _ (map,pos) =
    if Variable.Map.mem var kind then
      (* This is a constant: don't put it in the closure *)
      (map,pos)
    else
      let var_within_closure = Var_within_closure.wrap var in
      assert(not (Var_within_closure.Map.mem var_within_closure map));
      let map = Var_within_closure.Map.add var_within_closure pos map in
      (map,pos + 1)
  in
  let free_variable_offsets, _ =
    Variable.Map.fold aux_fv_offset
      free_vars (free_variable_offsets, fv_pos)
  in

  { code_pointer_offsets;
    free_variable_offsets }

let compute (lifted_constants:Lift_constants.result) =
  let sets = Lifted_flambda_utils.sets_of_closures lifted_constants in
  List.fold_left
    (add_closure_offsets lifted_constants.kind)
    { code_pointer_offsets = Closure_id.Map.empty;
      free_variable_offsets = Var_within_closure.Map.empty; }
    sets
