(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Tsl_ast

let rec tests_in_stmt set stmt =
  match stmt with
  | Environment_statement _ -> set
  | Not test -> tests_in_stmt set test
  | And (t1, t2) | Or (t1, t2) | If (t1, t2, None) ->
    tests_in_stmt (tests_in_stmt set t1) t2
  | If (t1, t2, Some t3) ->
    tests_in_stmt (tests_in_stmt (tests_in_stmt set t1) t2) t3
  | Action { name; _ } ->
    begin match Tsl_semantics.lookup_test name with
    | t -> Tests.TestSet.add t set
    | exception Tsl_semantics.No_such_test_or_action _ -> set
    end

let rec tests_in_tree_aux set (Tsl_ast.Ast (stmts, subs)) =
  let set1 = List.fold_left tests_in_stmt set stmts in
  List.fold_left tests_in_tree_aux set1 subs

let tests_in_tree t = tests_in_tree_aux Tests.TestSet.empty t

let actions_in_test test =
  let add action_set action = Actions.ActionSet.add action action_set in
  List.fold_left add Actions.ActionSet.empty test.Tests.test_actions

let actions_in_tests tests =
  let f test action_set =
    Actions.ActionSet.union (actions_in_test test) action_set in
  Tests.TestSet.fold f tests Actions.ActionSet.empty
