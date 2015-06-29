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

type lifter = Expr_id.t Flambda.t -> Expr_id.t Flambda.t

let lift_lets tree =
  let rec aux (expr : _ Flambda.t) : _ Flambda.t =
    match expr with
    | Fsequence(Flet(str, v, def, body, d1), seq, dseq) ->
        Flet(str, v, def, Fsequence( aux body, seq, dseq), d1)
    | Flet(str1, v1, Flet(str2, v2, def2, body2, d2), body1, d1) ->
        Flet(str2, v2, def2, aux (
          Flambda.Flet(str1, v1, body2, body1, d1)), d2)
    | e -> e
  in
  Flambdaiter.map aux tree

let lifting_helper ~evaluate_right_to_left:exprs ~create_body ~name =
  let exprs, lets =
    List.fold_right (fun (flam : _ Flambda.t) (exprs, lets) ->
        match flam with
        | Fvar (_v, _) as e ->
          (* Assumes that [v] is an immutable variable, otherwise this may
             change the evaluation order. *)
          (* XCR mshinwell for pchambart: Please justify why [v] is always
             immutable.
             pchambart: My bad, this is not the case. I was
             convinced i did remove the reference to variable optimisation from
             Simplif.simplif and this is not the case (this is better done by
             Ref_to_variables).
             We could either remove the optimisation in Simplif in case of native code
             and add an assert requiring that no mutable variables here (in the Let case)
             or get rid of this one that will be done in the end by the first inlining
             pass. *)
          e::exprs, lets
        | expr ->
          let v =
            Variable.create name ~current_compilation_unit:
                (Compilation_unit.get_current_exn ())
          in
          ((Fvar (v, Expr_id.create ())) : _ Flambda.t)::exprs,
            (v, expr)::lets)
      exprs ([], [])
  in
  List.fold_left (fun body (v, expr) ->
      Flambda.Flet (Immutable, v, expr, body, Expr_id.create ()))
    (create_body exprs) lets
