module Wsb = Inlining_cost.Whether_sufficient_benefit

module Tried_unrolling = struct
  type t =
    | Tried_unrolling of bool

  let to_string = function
    | Tried_unrolling true -> "tried unrolling"
    | Tried_unrolling false -> "did not try unrolling"
end

module Copying_body = struct
  type t =
    | Unconditionally
    | Decl_local_to_application
    | Evaluated of Wsb.t

  let to_string = function
    | Unconditionally -> "unconditionally"
    | Decl_local_to_application -> "decl local to application expression"
    | Evaluated wsb -> Wsb.to_string wsb
end

module Inlined = struct
  type t =
    | Copying_body of Copying_body.t
    | Copying_body_with_subfunctions of Copying_body.t
    | Unrolled of Wsb.t
    | Copying_decl of Tried_unrolling.t * Wsb.t

  let to_string = function
    | Copying_body cb ->
      Printf.sprintf "copying body (%s)" (Copying_body.to_string cb)
    | Copying_body_with_subfunctions cb ->
      Printf.sprintf "copying body using subfunctions (%s)" (Copying_body.to_string cb)
    | Unrolled wsb ->
      Printf.sprintf "unrolled (%s)" (Wsb.to_string wsb)
    | Copying_decl (tried, wsb) ->
      Printf.sprintf "copying decl (%s, %s)"
        (Tried_unrolling.to_string tried) (Wsb.to_string wsb)
end

module Decision = struct

  type level_exceeded =
    | Level_exceeded of bool

  type t =
    | Function_obviously_too_large
    | Inlined of Inlined.t
    | Tried of Inlined.t
    | Did_not_try_copying_decl of Tried_unrolling.t
    | Can_inline_but_tried_nothing of level_exceeded

  let to_string = function
    | Function_obviously_too_large -> "function obviously too large"
    | Inlined inlined ->
      Printf.sprintf "inlined (%s)" (Inlined.to_string inlined)
    | Tried inlined ->
      Printf.sprintf "tried but failed (%s)" (Inlined.to_string inlined)
    | Did_not_try_copying_decl tried ->
      Printf.sprintf "did not try copying decl (%s)"
        (Tried_unrolling.to_string tried)
    | Can_inline_but_tried_nothing (Level_exceeded b) ->
        if b then
          "can inline, but tried nothing, too deep into inlining"
        else
          "can inline, but tried nothing"
end

type where_entering_closure =
  | Transform_set_of_closures_expression
  | Inline_by_copying_function_body
  | Inline_by_copying_function_declaration
  | Inlining_decision

let char_of_where = function
  | Transform_set_of_closures_expression -> 'T'
  | Inline_by_copying_function_body -> 'B'
  | Inline_by_copying_function_declaration -> 'D'
  | Inlining_decision -> 'I'
