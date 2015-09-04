
type allocation_point =
  | Symbol of Symbol.t
  | Variable of Variable.t

val run : Flambda.program -> allocation_point Variable.Map.t

type constant_defining_value =
  | Allocated_const of Allocated_const.t
  | Block of Tag.t * Variable.t list
  | Set_of_closures of Flambda.set_of_closures
  | Project_closure of Flambda.project_closure
  | Move_within_set_of_closures of Flambda.move_within_set_of_closures
  | Project_var of Flambda.project_var
  | Field of Variable.t * int
  | Symbol_field of Symbol.t * int
  | Const of Flambda.const
  | Symbol of Symbol.t
  | Variable of Variable.t

type initialize_symbol_field = Variable.t option

val second_take :
  constant_defining_value Variable.Map.t ->
  (initialize_symbol_field list) Symbol.Map.t ->
  Flambda.constant_defining_value Symbol.Map.t ->
  Variable.t Symbol.Map.t ->
  allocation_point Variable.Map.t

