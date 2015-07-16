
type result = {
  code_pointer_offsets : int Closure_id.Map.t;
  free_variable_offsets : int Var_within_closure.Map.t;
}

val compute : Lift_constants.result -> result
