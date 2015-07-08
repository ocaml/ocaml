module type S = sig
  type t

  val simplify_unop
     : Lambda.primitive
    -> t Simple_value_approx.boxed_int
    -> Flambda.named
    -> t
    -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t

  val simplify_binop
     : Lambda.primitive
    -> t Simple_value_approx.boxed_int
    -> Flambda.named
    -> t
    -> t
    -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t

  val simplify_binop_int
     : Lambda.primitive
    -> t Simple_value_approx.boxed_int
    -> Flambda.named
    -> t
    -> int
    -> size_int:int
    -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t
end
