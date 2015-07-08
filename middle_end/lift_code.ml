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

type lifter = Flambda.t -> Flambda.t

let lift_lets tree =
  let rec aux (expr : Flambda.t) : Flambda.t =
    match expr with
    | Fsequence(Let(str, v, def, body, d1), seq, dseq) ->
        Let(str, v, def, Fsequence( aux body, seq, dseq), d1)
    | Let(str1, v1, Let(str2, v2, def2, body2, d2), body1, d1) ->
        Let(str2, v2, def2, aux (
          Flambda.Let(str1, v1, body2, body1, d1)), d2)
    | e -> e
  in
  Flambdaiter.map aux tree

let lifting_helper exprs ~evaluation_order ~create_body ~name =
  let vars, lets =
    (* [vars] corresponds elementwise to [exprs]; the order is unchanged. *)
    List.fold_right (fun (flam : Flambda.t) (vars, lets) ->
        match flam with
        | Var (v, _) ->
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
             pass.
             mshinwell: as a note, we need to check what to do for the other
             cases in which we now use this function (see Closure_conversion)
          *)
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
      Flambda.Let (Immutable, v, expr, body))
    (create_body vars) lets
