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

(* Interpretation of TSL blocks and operations on test trees *)

open Tsl_ast

let string_of_location loc =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  Location.print_loc fmt loc;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let no_such_variable loc name =
  let locstr = string_of_location loc in
  Printf.eprintf "%s\nNo such variable %s\n%!" locstr name;
  exit 2

let no_such_modifiers loc name =
  let locstr = string_of_location loc in
  Printf.eprintf "%s\nNo such modifiers %s\n%!" locstr name;
  exit 2

let apply_modifiers env modifiers_name =
  let name = modifiers_name.node in
  let modifier = Environments.Include name in
  try Environments.apply_modifier env modifier with
  | Environments.Modifiers_name_not_found name ->
    no_such_modifiers modifiers_name.loc name

let rec add_to_env decl loc variable_name value env =
  match (Variables.find_variable variable_name, decl) with
    | (None, true) ->
      let newvar = Variables.make (variable_name,"User variable") in
      Variables.register_variable newvar;
      add_to_env false loc variable_name value env
    | (Some variable, false) ->
      Environments.add variable value env
    | (None, false) ->
      raise (Variables.No_such_variable variable_name)
    | (Some _, true) ->
      raise (Variables.Variable_already_registered variable_name)

let append_to_env loc variable_name value env =
  let variable =
    match Variables.find_variable variable_name with
    | None ->
        raise (Variables.No_such_variable variable_name)
    | Some variable ->
        variable
  in
  try
    Environments.append variable value env
  with Variables.No_such_variable name ->
    no_such_variable loc name

let interpret_environment_statement env statement = match statement.node with
  | Assignment (decl, var, value) ->
      add_to_env decl statement.loc var.node value.node env
  | Append (var, value) ->
      append_to_env statement.loc var.node value.node env
  | Include modifiers_name ->
      apply_modifiers env modifiers_name
  | Unset var ->
      let var =
        match Variables.find_variable var.node with
        | None -> Variables.make (var.node,"User variable")
        | Some var -> var
      in
      Environments.unsetenv var env

let interpret_environment_statements env l =
  List.fold_left interpret_environment_statement env l

type test_tree =
  | Node of
    (Tsl_ast.environment_statement located list) *
    Tests.t *
    string located list *
    (test_tree list)

let too_deep testname max_level real_level =
  Printf.eprintf "Test %s should have depth atmost %d but has depth %d\n%!"
    testname max_level real_level;
  exit 2

let unexpected_environment_statement s =
  let locstr = string_of_location s.loc in
  Printf.eprintf "%s\nUnexpected environment statement\n%!" locstr;
  exit 2

let no_such_test_or_action t =
  let locstr = string_of_location t.loc in
  Printf.eprintf "%s\nNo such test or action: %s\n%!" locstr t.node;
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
        | Test (test_depth, located_name, env_modifiers) ->
          begin
            let name = located_name.node in
            if test_depth > depth then too_deep name depth test_depth
            else if test_depth < depth then (None, l)
            else
              let (env, rem) = env_of_lines remaining_lines in
              let (trees, rem) = trees_of_lines (depth+1) rem in
              match Tests.lookup name with
                | None ->
                  begin match Actions.lookup name with
                    | None -> no_such_test_or_action located_name
                    | Some action ->
                      let test = Tests.test_of_action action in
                      (Some (Node (env, test, env_modifiers, trees)), rem)
                  end
                | Some test ->
                  (Some (Node (env, test, env_modifiers, trees)), rem)
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

let rec tests_in_tree_aux set = function Node (_, test, _, subtrees) ->
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
