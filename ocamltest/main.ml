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

(* Main program of the ocamltest test driver *)

open Ocamltest_stdlib
open Tsl_semantics

type behavior =
  | Skip_all_tests
  | Run of Environments.t

(*
let first_token filename =
  let input_channel = open_in filename in
  let lexbuf = Lexing.from_channel input_channel in
  Location.init lexbuf filename;
  let token =
    try Tsl_lexer.token lexbuf with e -> close_in input_channel; raise e
  in close_in input_channel; token

let is_test filename =
  match first_token filename with
    | exception _ -> false
    | Tsl_parser.TSL_BEGIN_C_STYLE | TSL_BEGIN_OCAML_STYLE -> true
    | _ -> false
*)

let tsl_block_of_file test_filename =
  let input_channel = open_in test_filename in
  let lexbuf = Lexing.from_channel input_channel in
  Location.init lexbuf test_filename;
  match Tsl_parser.tsl_block Tsl_lexer.token lexbuf with
    | exception e -> close_in input_channel; raise e
    | _ as tsl_block -> close_in input_channel; tsl_block

let tsl_block_of_file_safe test_filename =
  try tsl_block_of_file test_filename with
  | Sys_error message ->
    Printf.eprintf "%s\n" message;
    exit 1
  | Parsing.Parse_error ->
    Printf.eprintf "Could not read test block in %s\n" test_filename;
    exit 1

let print_usage () =
  Printf.printf "%s\n%!" Options.usage

let rec run_test log common_prefix path behavior = function
  Node (testenvspec, test, env_modifiers, subtrees) ->
  Printf.printf "%s %s (%s) => %!" common_prefix path test.Tests.test_name;
  let (msg, b) = match behavior with
    | Skip_all_tests -> "skipped", Skip_all_tests
    | Run env ->
      let testenv0 = interprete_environment_statements env testenvspec in
      let testenv = List.fold_left apply_modifiers testenv0 env_modifiers in
      let t = Tests.run log testenv test in
      (match t with
      | Actions.Pass env -> "passed", Run env
      | Actions.Skip _ -> "skipped", Skip_all_tests
      | Actions.Fail _ -> "failed", Skip_all_tests) in
  Printf.printf "%s\n%!" msg;
  List.iteri (run_test_i log common_prefix path b) subtrees
and run_test_i log common_prefix path behavior i test_tree =
  let path_prefix = if path="" then "" else path ^ "." in
  let new_path = Printf.sprintf "%s%d" path_prefix (i+1) in
  run_test log common_prefix new_path behavior test_tree

let get_test_source_directory test_dirname =
  if (Filename.is_relative test_dirname) then
    Sys.with_chdir test_dirname Sys.getcwd
  else test_dirname

let get_test_build_directory_prefix test_dirname =
  let ocamltestdir_variable = "OCAMLTESTDIR" in
  let root = try Sys.getenv ocamltestdir_variable with
    | Not_found -> (Filename.concat (Sys.getcwd ()) "_ocamltest") in
  if test_dirname = "." then root
  else Filename.concat root test_dirname

let test_file test_filename =
  (* Printf.printf "# reading test file %s\n%!" test_filename; *)
  (* Save current working directory *)
  let cwd = Sys.getcwd() in
  let tsl_block = tsl_block_of_file_safe test_filename in
  let (rootenv_statements, test_trees) = test_trees_of_tsl_block tsl_block in
  let test_trees = match test_trees with
    | [] ->
      let default_tests = Tests.default_tests() in
      let make_tree test = Node ([], test, [], []) in
      List.map make_tree default_tests
    | _ -> test_trees in
  let used_tests = tests_in_trees test_trees in
  let used_actions = actions_in_tests used_tests in
  let action_names =
    let f act names = StringSet.add (Actions.action_name act) names in
    Actions.ActionSet.fold f used_actions StringSet.empty in
  let test_dirname = Filename.dirname test_filename in
  let test_basename = Filename.basename test_filename in
  let test_prefix = Filename.chop_extension test_basename in
  let test_directory =
    if test_dirname="." then test_prefix
    else Filename.concat test_dirname test_prefix in
  let test_source_directory = get_test_source_directory test_dirname in
  let hookname_prefix = Filename.concat test_source_directory test_prefix in
  let test_build_directory_prefix =
    get_test_build_directory_prefix test_directory in
  Sys.make_directory test_build_directory_prefix;
  Sys.with_chdir test_build_directory_prefix
    (fun () ->
       let log =
         if !Options.log_to_stderr then stderr else begin
           let log_filename = test_prefix ^ ".log" in
           open_out log_filename
         end in
       let install_hook name =
         let hook_name = Filename.make_filename hookname_prefix name in
         if Sys.file_exists hook_name then begin
           let hook = Actions_helpers.run_hook hook_name in
           Actions.set_hook name hook
         end in
       StringSet.iter install_hook action_names;

       let reference_filename = Filename.concat
           test_source_directory (test_prefix ^ ".reference") in
       let initial_environment = Environments.from_bindings
           [
             Builtin_variables.test_file, test_basename;
             Builtin_variables.reference, reference_filename;
             Builtin_variables.test_source_directory, test_source_directory;
             Builtin_variables.test_build_directory_prefix,
               test_build_directory_prefix;
           ] in
       let root_environment =
         interprete_environment_statements
           initial_environment rootenv_statements in
       let rootenv = Environments.initialize log root_environment in
       let common_prefix = " ... testing '" ^ test_basename ^ "' with" in
       List.iteri
         (run_test_i log common_prefix "" (Run rootenv))
         test_trees;
       Actions.clear_all_hooks();
       if not !Options.log_to_stderr then close_out log
    );
  (* Restore current working directory  *)
  Sys.chdir cwd

let main () =
  if !Options.files_to_test = [] then begin
    print_usage();
    exit 1
  end;
  List.iter test_file !Options.files_to_test

let _ = main()
