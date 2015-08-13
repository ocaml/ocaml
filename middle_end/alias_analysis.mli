
type allocation_point =
  | Symbol of Symbol.t
  | Variable of Variable.t

val run : Flambda.program -> allocation_point Variable.Map.t

type constant_defining_value =
  | Allocated_const of Allocated_const.t
  | Block of Tag.t * Variable.t list
  | Set_of_closures of Flambda.set_of_closures
  | Project_closure of Flambda.project_closure
  | Project_var of Flambda.project_var
  | Field of Variable.t * int
  | Const of Flambda.const
  | Symbol of Symbol.t
  | Predefined_exn of Ident.t
  | Variable of Variable.t

val second_take :
  constant_defining_value Variable.Map.t ->
  Variable.t Symbol.Map.t ->
  Variable.t Variable.Map.t

