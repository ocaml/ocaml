module type S = sig
  type t

  val simplify_unop
     : Lambda.primitive
    -> t Simple_value_approx.boxed_int
    -> Expr_id.t Flambda.t
    -> t
    -> Expr_id.t
    -> Expr_id.t Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t

  val simplify_binop
     : Lambda.primitive
    -> t Simple_value_approx.boxed_int
    -> Expr_id.t Flambda.t
    -> t
    -> t
    -> Expr_id.t
    -> Expr_id.t Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t

  val simplify_binop_int
     : Lambda.primitive
    -> t Simple_value_approx.boxed_int
    -> Expr_id.t Flambda.t
    -> t
    -> int
    -> Expr_id.t
    -> size_int:int
    -> Expr_id.t Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t
end
