(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module A = Simple_value_approx
module C = Inlining_cost

type lifter = Flambda.program -> Flambda.program

let lift_lets_expr tree =
  let module W = Flambda.With_free_variables in
  let rec aux (expr : Flambda.t) : Flambda.t =
    match expr with
    | Let ({ var = v1;
        defining_expr = Expr (Let ({ var = v2; _ } as let2)); _ }  as let1) ->
      let body1 = W.of_body_of_let let1 in
      let body2 = W.of_body_of_let let2 in
      let inner_let = W.create_let_reusing_both v1 (W.expr body2) body1 in
      let def2 = W.of_defining_expr_of_let let2 in
      W.create_let_reusing_defining_expr v2 def2 (aux inner_let)
    | e -> e
  in
  Flambda_iterators.map aux (fun (named : Flambda.named) -> named) tree

let lift_lets program =
  Flambda_iterators.map_exprs_at_toplevel_of_program program ~f:lift_lets_expr

let lifting_helper exprs ~evaluation_order ~create_body ~name =
  let vars, lets =
    (* [vars] corresponds elementwise to [exprs]; the order is unchanged. *)
    List.fold_right (fun (flam : Flambda.t) (vars, lets) ->
        match flam with
        | Var v ->
          (* Note that [v] is (statically) always an immutable variable. *)
          v::vars, lets
        | expr ->
          let v =
            Variable.create name ~current_compilation_unit:
                (Compilation_unit.get_current_exn ())
          in
          v::vars, (v, expr)::lets)
      exprs ([], [])
  in
  let lets =
    match evaluation_order with
    | `Right_to_left -> lets
    | `Left_to_right -> List.rev lets
  in
  List.fold_left (fun body (v, expr) ->
      Flambda.create_let v (Expr expr) body)
    (create_body vars) lets
