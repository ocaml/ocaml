open Abstract_identifiers

(* Examine a full application of a known closure to determine whether to
   inline.  Then, if inlining is desired, perform inlining using the
   supplied helper functions [inline_by_copying_function_body] and
   [inline_by_copying_function_declaration]. *)
(* CR mshinwell: improve some of the label names to avoid confusion, e.g.
   [clos] vs. [closure]; [func] vs. [funct]. *)
val inlining_decision_for_call_site
   : env:Flambda_inline_env.t
  -> r:Flambda_inline_result.t
  -> clos:'a Flambda.function_declarations
  -> funct:Expr_id.t Flambda.t
  -> fun_id:Closure_id.t
  -> func:'a Flambda.function_declaration
  -> closure:Flambdaapprox.value_set_of_closures
  -> args_with_approxs:((Expr_id.t Flambda.t list) * (Flambdaapprox.t list))
  -> ap_dbg:Debuginfo.t
  -> eid:Expr_id.t
  -> inline_by_copying_function_body:(
         env:Flambda_inline_env.t
      -> r:Flambda_inline_result.t
      -> clos:'a Flambda.function_declarations
      -> lfunc:Expr_id.t Flambda.t
      -> fun_id:Closure_id.t
      -> func:'a Flambda.function_declaration
      -> args:Expr_id.t Flambda.t list
      -> Expr_id.t Flambda.t * Flambda_inline_result.t)
  -> inline_by_copying_function_declaration:(
         env:Flambda_inline_env.t
      -> r:Flambda_inline_result.t
      -> funct:Expr_id.t Flambda.t
      -> clos:'a Flambda.function_declarations
      -> fun_id:Closure_id.t
      -> func:'a Flambda.function_declaration
      -> args_with_approxs:(Expr_id.t Flambda.t list) * (Flambdaapprox.t list)
      -> unchanging_params:Variable.Set.t
      -> specialised_args:Variable.Set.t
      -> ap_dbg:Debuginfo.t
      -> no_transformation:(
          unit -> (Expr_id.t Flambda.t * Flambda_inline_result.t))
      -> Expr_id.t Flambda.t * Flambda_inline_result.t)
  -> Expr_id.t Flambda.t * Flambda_inline_result.t
