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

open Abstract_identifiers

module A = Simple_value_approx
module C = Inlining_cost

let lift_lets tree =
  let rec aux (expr : _ Flambda.t) : _ Flambda.t =
    match expr with
    | Fsequence(Flet(str, v, def, body, d1), seq, dseq) ->
        Flet(str, v, def, Fsequence( aux body, seq, dseq), d1)
    | Flet(str1, v1, Flet(str2, v2, def2, body2, d2), body1, d1) ->
        Flet(str2, v2, def2, aux (Flet(str1, v1, body2, body1, d1)), d2)
    | e -> e
  in
  Flambdaiter.map aux tree

let lift_set_of_closures tree =
  let aux (expr : _ Flambda.t) : _ Flambda.t =
    match expr with
    | Fselect_closure({ set_of_closures = Fset_of_closures(set, dset) } as closure, d) ->
        let decl = Flambdautils.find_declaration closure.closure_id set.function_decls in
        if not decl.stub then
          expr
        else
          (* If the function is a stub, we create an intermediate let to allow
             eliminating it *)
          let set_of_closures_var =
            Variable.create
              ~current_compilation_unit:
                (Symbol.Compilation_unit.get_current_exn ())
              "set_of_closures"
          in
          Flet(Immutable, set_of_closures_var,
               Fset_of_closures(set, dset),
               Fselect_closure({ closure with
                          set_of_closures =
                            Fvar (set_of_closures_var, Expr_id.create ()) },
                        d),
               Expr_id.create ())
    | e -> e
  in
  Flambdaiter.map aux tree
