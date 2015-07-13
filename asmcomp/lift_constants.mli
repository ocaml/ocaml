
type block = int * Variable.t list

type result = {
  expr : Flambda.t;
  block_tbl : block Variable.Tbl.t;
  set_of_closures_tbl : Flambda.set_of_closures Variable.Tbl.t;
}

val lift_constants : Flambda.t -> result
