(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris           *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Interpretation of TSL blocks and operations on test trees *)

open Tsl_ast

let variable_already_defined loc variable context =
  let ctxt = match context with
    | None -> ""
    | Some envname -> " while including environment " ^ envname in
  let locstr = Testlib.string_of_location loc in
  Printf.eprintf "%s\nVariable %s already defined%s\n%!" locstr variable ctxt;
  exit 2

let no_such_environment loc environment_name =
  let locstr = Testlib.string_of_location loc in
  Printf.eprintf "%s\nNo such environment %s\n%!" locstr environment_name;
  exit 2

let interprete_environment_statement env statement = match statement.node with
  | Assignment (var, value) ->
    begin
      let variable_name = var.node in
      let variable = match Variables.find_variable variable_name with
        | None -> Variables.make (variable_name, "User variable")
        | Some variable -> variable in
      try Environments.add variable value.node env with
      Environments.Variable_already_defined variable ->
        variable_already_defined statement.loc
          (Variables.name_of_variable variable) None
    end
  | Include env_name ->
    begin
      try Environments.include_ env_name.node env with
      | Environments.Environment_not_found envname ->
        no_such_environment statement.loc envname
      | Environments.Variable_already_defined variable ->
        variable_already_defined statement.loc
          (Variables.name_of_variable variable) (Some env_name.node)
    end

let interprete_environment_statements env l =
  List.fold_left interprete_environment_statement env l

type test_tree =
  | Node of (Tsl_ast.environment_statement located list) * Tests.t * (test_tree list)

let too_deep testname max_level real_level =
  Printf.eprintf "Test %s should have depth atmost %d but has depth %d\n%!"
    testname max_level real_level;
  exit 2

let unexpected_environment_statement s =
  let locstr = Testlib.string_of_location s.loc in
  Printf.eprintf "%s\nUnexpected environment statement\n%!" locstr;
  exit 2

let no_such_test t =
  let locstr = Testlib.string_of_location t.loc in
  Printf.eprintf "%s\nNo such test: %s\n%!" locstr t.node;
  exit 2

let test_trees_of_tsl_block tsl_block =
  let rec env_of_lines = function
    | [] -> ([], [])
    | Environment_statement s :: lines ->
      let (env', remaining_lines) = env_of_lines lines in
      (s :: env', remaining_lines)
    | lines -> ([], lines)
  and tree_of_lines depth = function
    | [] -> (None, [])
    | line::remaining_lines as l ->
      begin match line with
        | Environment_statement s -> unexpected_environment_statement s
        | Test (test_depth, test_name_located) ->
          begin
            let test_name = test_name_located.node in
            if test_depth > depth then too_deep test_name depth test_depth
            else if test_depth < depth then (None, l)
            else
              let (env, rem) = env_of_lines remaining_lines in
              let (trees, rem) = trees_of_lines (depth+1) rem in
              match Tests.lookup test_name with
                | None -> no_such_test test_name_located
                | Some test ->
                  (Some (Node (env, test, trees)), rem)
          end
      end
  and trees_of_lines depth lines =
    let remaining_lines = ref lines in
    let trees = ref [] in
    let continue = ref true in
    while !continue; do
      let (tree, rem) = tree_of_lines depth !remaining_lines in
      remaining_lines := rem;
      match tree with
        | None -> continue := false
        | Some t -> trees := t :: !trees
    done;
    (List.rev !trees, !remaining_lines) in
  let (env, rem) = env_of_lines tsl_block in
  let (trees, rem) = trees_of_lines 1 rem in
  match rem with
    | [] -> (env, trees)
    | (Environment_statement s)::_ -> unexpected_environment_statement s
    | _ -> assert false

let rec tests_in_tree_aux set = function Node (_, test, subtrees) ->
  let set' = List.fold_left tests_in_tree_aux set subtrees in
  Tests.TestSet.add test set'

let tests_in_tree t = tests_in_tree_aux Tests.TestSet.empty t

let tests_in_trees subtrees =
  List.fold_left tests_in_tree_aux Tests.TestSet.empty subtrees

let actions_in_test test =
  let add action_set action = Actions.ActionSet.add action action_set in
  List.fold_left add Actions.ActionSet.empty test.Tests.test_actions

let actions_in_tests tests =
  let f test action_set =
    Actions.ActionSet.union (actions_in_test test) action_set in
  Tests.TestSet.fold f tests Actions.ActionSet.empty
