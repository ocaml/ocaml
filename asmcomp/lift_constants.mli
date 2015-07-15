
type constant =
  | Symbol of Symbol.t
  | Int of int
  | Const_pointer of int

type result = {
  expr : Flambda.t;
  tbl : Flambda.named Variable.Tbl.t;
  constant_tbl : constant Variable.Tbl.t;
  set_of_closures_map : Flambda.set_of_closures Variable.Map.t;
}

val lift_constants : Flambda.t -> result
