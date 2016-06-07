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

(* Interpretation of TSL parse trees *)

open Tsl_ast

let interprete_statement env = function
  | Assignment (variable, value) ->
    Environments.add variable value env
  | Include env_name -> Environments.include_ env_name env

let interprete_statements env l =
  List.fold_left interprete_statement env l

let action_of_name ppf name = match Actions.lookup name with
  | None -> Format.fprintf ppf "Unknown action %s\n" name; exit 1
  | Some action -> action

let test_of_ast ppf ast =
  let test = match ast.test_kind, (Tests.lookup ast.test_name) with
    | Declared_test, Some test -> test
    | Declared_test, None ->
      Format.fprintf ppf "No such test %s\n" ast.test_name;
      exit 1
    | New_test _, Some _ ->
      Format.fprintf ppf "Test %s already exists\n" ast.test_name;
      exit 1
    | New_test action_names, None ->
      begin
        let t = {
          Tests.test_name = ast.test_name;
          test_run_by_default = false;
          test_actions = List.map (action_of_name ppf) action_names
        } in
        Tests.register t;
        t
      end in
  (test, ast.test_environment)

let tests_of_ast ppf ast =
  List.map (test_of_ast ppf) ast

let rec memf f x = function
  | [] -> false
  | y::ys -> f x y || memf f x ys

let consf f x l = if memf f x l then l else x::l

let rec appendf f l1 l2 = match l1 with
  | [] -> l2
  | x::xs -> consf f x (appendf f xs l2)

let same_action action_1 action_2 =
  action_1.Actions.action_name = action_2.Actions.action_name

let append_actions = appendf same_action

let actions_of_tests tests =
  let f actions test = append_actions actions (test.Tests.test_actions) in
  List.fold_left f [] tests
