module type S = sig
  type t

  val simplify_unop
     : Lambda.primitive
    -> t Simple_value_approx.boxed_int
    -> 'a Flambda.t
    -> t
    -> 'a
    -> 'a Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t

  val simplify_binop
     : Lambda.primitive
    -> t Simple_value_approx.boxed_int
    -> 'a Flambda.t
    -> t
    -> t
    -> 'a
    -> 'a Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t

  val simplify_binop_int
     : Lambda.primitive
    -> t Simple_value_approx.boxed_int
    -> 'a Flambda.t
    -> t
    -> int
    -> 'a
    -> size_int:int
    -> 'a Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t
end
