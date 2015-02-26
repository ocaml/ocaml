(* Types used for producing statistics about inlining. *)

type tried_unrolling =
  | Tried_unrolling of bool

type decision =
  | Never_inline
  | Can_inline_but_env_says_not_to
  | Inlined_copying_body of
      Flambdacost.benefit * Flambdacost.inline_threshold
  | Tried_copying_body of Flambdacost.benefit * Flambdacost.inline_threshold
  | Unrolled of Flambdacost.benefit * Flambdacost.inline_threshold
  | Inlined_copying_decl of
      tried_unrolling * Flambdacost.benefit * Flambdacost.inline_threshold
  | Tried_copying_decl of
      tried_unrolling * Flambdacost.benefit * Flambdacost.inline_threshold
  | Did_not_try_copying_decl of tried_unrolling
  | Can_inline_but_tried_nothing

type where_entering_closure =
  | Transform_set_of_closures_expression
  | Inline_by_copying_function_body
  | Inline_by_copying_function_declaration
  | Inlining_decision
