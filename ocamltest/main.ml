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

(* Main program for the test handler *)

open Tsl_ast
open Tsl_semantics

let first_token filename =
  let input_channel = open_in filename in
  let lexbuf = Lexing.from_channel input_channel in
  Location.init lexbuf filename;
  let token =
    try Tsl_lexer.token lexbuf with e -> close_in input_channel; raise e
  in close_in input_channel; token

let is_test filename =
  match first_token filename with
    | exception e -> false
    | Tsl_parser.TSL_BEGIN -> true
    | _ -> false

let tsl_block_of_file test_filename =
  let input_channel = open_in test_filename in
  let lexbuf = Lexing.from_channel input_channel in
  Location.init lexbuf test_filename;
  match Tsl_parser.tsl_block Tsl_lexer.token lexbuf with
    | exception e -> close_in input_channel; raise e
    | _ as tsl_block -> close_in input_channel; tsl_block
  
let print_usage () =
  Printf.printf "Usage: %s testfile\n" Sys.argv.(0)

let rec run_test ppf path rootenv = function
  Node (testenvspec, test, subtrees) ->
  Format.printf "Running test %s (%s) ... %!"
    path test.Tests.test_name;
  let print_test_result str = Format.printf "%s\n%!" str in
  let testenv = interprete_environment_statements rootenv testenvspec in
  match Tests.run ppf testenv test with
    | Actions.Pass newenv ->
      print_test_result "passed";
      List.iteri (run_test_i ppf path newenv) subtrees
    | Actions.Fail _ -> print_test_result "failed"
    | Actions.Skip _ -> print_test_result "skipped"
and run_test_i ppf path rootenv i test_tree =
  let prefix = if path="" then "" else path ^ "." in
  let new_path = Printf.sprintf "%s%d" prefix (i+1) in
  run_test ppf new_path rootenv test_tree

let initial_env filename =
  let add env (variable, value) = Environments.add variable value env in
  let l =
  [
    ("testfile", filename);
  ] in
  List.fold_left add Environments.empty l

let log_filename_of_test_filename test_filename =
  test_filename ^ ".log" (* one may want to remove the extension *)

let main () =
  if Array.length Sys.argv < 2 then begin
    print_usage();
    exit 1
  end;
  let test_filename = Sys.argv.(1) in
  let log_filename = log_filename_of_test_filename test_filename in
  Printf.printf "# reading test file %s, logging test details to %s\n%!"
    test_filename log_filename;
  let log_channel = open_out log_filename in
  let ppf = Format.formatter_of_out_channel log_channel in
  let dirname = Filename.dirname test_filename in
  let basename = Filename.basename test_filename in
  let tsl_block = tsl_block_of_file test_filename in
  Sys.chdir dirname;
  let init_env = (initial_env basename) in
  let (rootenv_statements, test_trees) = test_trees_of_tsl_block tsl_block in
  let root_environment =
    interprete_environment_statements init_env rootenv_statements in
  let test_trees = match test_trees with
    | [] ->
      let default_tests = Tests.default_tests() in
      let make_tree test = Node ([], test, []) in
      List.map make_tree default_tests
    | _ -> test_trees in
  let actions = actions_in_tests (tests_in_trees test_trees) in
  let rootenv = Actions.update_environment root_environment actions in
  List.iteri (run_test_i ppf "" rootenv) test_trees;
  close_out log_channel

let _ = main()
