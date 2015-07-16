
let sets_of_closures ({ expr; set_of_closures_map } : Lift_constants.result) =
  let l = ref [] in
  let add_set_of_closures : Flambda.named -> unit = function
    | Set_of_closures set_of_closure ->
      l := set_of_closure :: !l
    | _ -> ()
  in
  Flambda_iterators.iter_named add_set_of_closures expr;
  Symbol.Map.iter (fun _ set_of_closures ->
      Flambda_iterators.iter_named_on_named add_set_of_closures
        (Set_of_closures set_of_closures))
    set_of_closures_map;
  !l

type result = {
  code_pointer_offsets : int Closure_id.Map.t;
  free_variable_offsets : int Var_within_closure.Map.t;
}

let add_closure_offsets
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
  let aux_fv_offset id _ (map,pos) =
    let off = Var_within_closure.wrap id in
    assert(not (Var_within_closure.Map.mem off map));
    let map = Var_within_closure.Map.add off pos map in
    (map,pos + 1)
  in
  let free_variable_offsets, _ =
    Variable.Map.fold aux_fv_offset
      free_vars (free_variable_offsets, fv_pos)
  in

  { code_pointer_offsets;
    free_variable_offsets }

let compute (lifted_constants:Lift_constants.result) =
  let sets = sets_of_closures lifted_constants in
  List.fold_left
    add_closure_offsets
    { code_pointer_offsets = Closure_id.Map.empty;
      free_variable_offsets = Var_within_closure.Map.empty; }
    sets
