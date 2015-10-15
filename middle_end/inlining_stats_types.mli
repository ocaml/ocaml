(* Types used for producing statistics about inlining. *)

module Tried_unrolling : sig
  type t =
    | Tried_unrolling of bool

  val to_string : t -> string
end

module Copying_body : sig
  type t =
    | Unconditionally
    | Decl_local_to_application
    | Evaluated of Inlining_cost.Whether_sufficient_benefit.t

  val to_string : t -> string
end

module Inlined : sig
  type t =
    | Copying_body of Copying_body.t
    | Copying_body_with_subfunctions of Copying_body.t
    | Unrolled of Inlining_cost.Whether_sufficient_benefit.t
    | Copying_decl of
        Tried_unrolling.t * Inlining_cost.Whether_sufficient_benefit.t

  val to_string : t -> string
end

module Decision : sig
  type level_exceeded =
    | Level_exceeded of bool

  type t =
    | Function_obviously_too_large
    | Inlined of Inlined.t
    | Tried of Inlined.t
    | Did_not_try_copying_decl of Tried_unrolling.t
    | Can_inline_but_tried_nothing of level_exceeded

  val to_string : t -> string
end

type where_entering_closure =
  | Transform_set_of_closures_expression
  | Inline_by_copying_function_body
  | Inline_by_copying_function_declaration
  | Inlining_decision

val char_of_where : where_entering_closure -> char
