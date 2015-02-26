open Abstract_identifiers

include Flambda_inlining_stats_types

let char_of_where = function
  | Transform_set_of_closures_expression -> 'T'
  | Inline_by_copying_function_body -> 'B'
  | Inline_by_copying_function_declaration -> 'D'

module Closure_stack = struct
  type t = (Closure_id.t * where_entering_closure) list

  let create () = []

  let note_entering_closure t ~closure_id ~where =
    (closure_id, where)::t

  let save t ~out_channel =
    let print_elt (closure_id, where) =
      let ppf = Format.formatter_of_out_channel out_channel in
      Printf.fprintf out_channel "%c[" (char_of_where where);
      Closure_id.print ppf closure_id;
      Printf.fprintf out_channel "]"
    in
    let rec loop = function
      | [] -> Printf.fprintf out_channel "[]"
      | [elt] -> print_elt elt
      | elt::elts ->
        print_elt elt;
        Printf.fprintf out_channel "|";
        loop elts
    in
    loop t
end

let decisions : (decision * Closure_stack.t) list ref = ref []

let record_decision decision ~closure_stack =
  decisions := (decision, closure_stack)::!decisions

let char_of_decision = function
  | Never_inline -> 'n'
  | Can_inline_but_env_says_not_to -> 'e'
  | Inlined_copying_body _ -> 'B'
  | Tried_copying_body _ -> 'b'
  | Unrolled _ -> 'U'
  | Inlined_copying_decl _ -> 'D'
  | Tried_copying_decl _ -> 'd'
  | Did_not_try_copying_decl _ -> 'c'
  | Can_inline_but_tried_nothing -> 'x'

let save_then_forget_decisions ~output_prefix =
  let out_channel = open_out (output_prefix ^ ".i") in
  List.iter (fun (decision, closure_stack) ->
      Closure_stack.save closure_stack ~out_channel;
      Printf.fprintf out_channel "=%c" (char_of_decision decision))
    !decisions;
  close_out out_channel;
  decisions := []
