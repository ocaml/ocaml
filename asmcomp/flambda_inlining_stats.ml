open Abstract_identifiers

include Flambda_inlining_stats_types

let char_of_where = function
  | Transform_set_of_closures_expression -> 'T'
  | Inline_by_copying_function_body -> 'B'
  | Inline_by_copying_function_declaration -> 'D'
  | Inlining_decision -> 'I'

module Closure_stack = struct
  type t = (Closure_id.t * where_entering_closure) list

  let create () = []

  let compare t1 t2 =
    match t1, t2 with
    | (id1, _)::_, (id2, _)::_ ->
      let (_ : string) = Format.flush_str_formatter () in
      let (id1 : string) =
        Format.fprintf Format.str_formatter "%a" Closure_id.print id1;
        Format.flush_str_formatter ()
      in
      let id2 =
        Format.fprintf Format.str_formatter "%a" Closure_id.print id2;
        Format.flush_str_formatter ()
      in
      String.compare id1 id2
    | _ -> 0

  let note_entering_closure t ~closure_id ~where =
    t @ [closure_id, where]

  let save t ~out_channel =
    let print_elt (closure_id, where) =
      Printf.fprintf out_channel "%a" Closure_id.output closure_id
(*
      Printf.fprintf out_channel "%c|%a|" (char_of_where where)
        Closure_id.output closure_id
*)
    in
    let rec loop = function
      | [] -> Printf.fprintf out_channel "[]"
      | [elt] -> print_elt elt
      | elt::elts ->
        print_elt elt;
        Printf.fprintf out_channel " -> ";
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

let print_tried () = function
  | Tried_unrolling true -> "yes"
  | Tried_unrolling false -> "no"

let print_benefit () benefit : string =
  let (_ : string) = Format.flush_str_formatter () in
  Format.fprintf Format.str_formatter "%a"
      Flambdacost.print_benefit_summary benefit;
  Format.flush_str_formatter ()

let print_threshold () = function
  | Flambdacost.Never_inline -> "never-inline"
  | Flambdacost.Can_inline threshold ->
    Printf.sprintf "%d" threshold

let string_of_decision = function
  | Never_inline -> "never to be inlined"
  | Can_inline_but_env_says_not_to -> "environment says not to inline"
  | Inlined_copying_body (benefit, threshold) ->
    Printf.sprintf "inlined: function body was copied (benefit %a, \
        threshold %a)" print_benefit benefit print_threshold threshold
  | Tried_copying_body (benefit, threshold) ->
    Printf.sprintf "rejected: tried copying body, but too large (benefit %a, \
        threshold %a)" print_benefit benefit print_threshold threshold
  | Unrolled (benefit, threshold) ->
    Printf.sprintf "recursive function was unrolled (benefit %a, threshold %a)"
        print_benefit benefit print_threshold threshold
  | Inlined_copying_decl (tried, benefit, threshold) ->
    Printf.sprintf "inlined: function declaration was copied (\
        tried unrolling? %a; benefit %a, threshold %a)"
        print_tried tried print_benefit benefit print_threshold threshold
  | Tried_copying_decl (tried, benefit, threshold) ->
    Printf.sprintf "rejected: tried copying decl, but too large (\
        tried unrolling? %a; benefit %a, threshold %a)"
        print_tried tried print_benefit benefit print_threshold threshold
  | Did_not_try_copying_decl tried ->
    Printf.sprintf "didn't try copying decl (tried unrolling? %a)"
        print_tried tried
  | Can_inline_but_tried_nothing -> "can inline but tried nothing"

let save_then_forget_decisions ~output_prefix =
  let out_channel = open_out (output_prefix ^ ".i") in
  let sorted_decisions =
    List.sort (fun (decision1, closure_stack1) (decision2, closure_stack2) ->
        Closure_stack.compare closure_stack1 closure_stack2)
      !decisions
  in
  List.iter (fun (decision, closure_stack) ->
      Closure_stack.save closure_stack ~out_channel;
      Printf.fprintf out_channel ": %s\n" (string_of_decision decision))
    sorted_decisions;
  close_out out_channel;
  decisions := []
