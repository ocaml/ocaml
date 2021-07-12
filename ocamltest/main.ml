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

(* this primitive announce should be used for tests
   that were aborted on system error before ocamltest
   could parse them *)
let announce_test_error test_filename error =
  Printf.printf " ... testing '%s' => unexpected error (%s)\n%!"
    (Filename.basename test_filename) error

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
    Printf.eprintf "%s\n%!" message;
    announce_test_error test_filename message;
    exit 1
  | Parsing.Parse_error ->
    Printf.eprintf "Could not read test block in %s\n%!" test_filename;
    announce_test_error test_filename "could not read test block";
    exit 1

let print_usage () =
  Printf.printf "%s\n%!" Options.usage

type result_summary = No_failure | Some_failure
let join_summaries sa sb =
  match sa, sb with
  | Some_failure, _ | _, Some_failure -> Some_failure
  | No_failure, No_failure -> No_failure

let summary_of_result res =
  let open Result in
  match res.status with
  | Pass -> No_failure
  | Skip -> No_failure
  | Fail -> Some_failure

let rec run_test log common_prefix path behavior = function
  Node (testenvspec, test, env_modifiers, subtrees) ->
  Printf.printf "%s %s (%s) => %!" common_prefix path test.Tests.test_name;
  let (msg, children_behavior, summary) = match behavior with
    | Skip_all_tests -> "n/a", Skip_all_tests, No_failure
    | Run env ->
      let testenv0 = interpret_environment_statements env testenvspec in
      let testenv = List.fold_left apply_modifiers testenv0 env_modifiers in
      let (result, newenv) = Tests.run log testenv test in
      let msg = Result.string_of_result result in
      let children_behavior =
        if Result.is_pass result then Run newenv else Skip_all_tests in
      let summary = summary_of_result result in
      (msg, children_behavior, summary) in
  Printf.printf "%s\n%!" msg;
  join_summaries summary
    (run_test_trees log common_prefix path children_behavior subtrees)

and run_test_trees log common_prefix path behavior trees =
  List.fold_left join_summaries No_failure
    (List.mapi (run_test_i log common_prefix path behavior) trees)

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
  let root =
    Sys.getenv_with_default_value ocamltestdir_variable
      (Filename.concat (Sys.getcwd ()) "_ocamltest")
  in
  if test_dirname = "." then root
  else Filename.concat root test_dirname

let tests_to_skip = ref []

let init_tests_to_skip () =
  tests_to_skip := String.words (Sys.safe_getenv "OCAMLTEST_SKIP_TESTS")

let test_file test_filename =
  let skip_test = List.mem test_filename !tests_to_skip in
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
    let f act names = String.Set.add (Actions.name act) names in
    Actions.ActionSet.fold f used_actions String.Set.empty in
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
  let clean_test_build_directory () =
    try
      Sys.rm_rf test_build_directory_prefix
    with Sys_error _ -> ()
  in
  clean_test_build_directory ();
  Sys.make_directory test_build_directory_prefix;
  let log_filename =
    Filename.concat test_build_directory_prefix (test_prefix ^ ".log") in
  let log =
    if Options.log_to_stderr then stderr else begin
      open_out log_filename
    end in
  let summary = Sys.with_chdir test_build_directory_prefix
    (fun () ->
       let promote = string_of_bool Options.promote in
       let default_timeout = string_of_int Options.default_timeout in
       let install_hook name =
         let hook_name = Filename.make_filename hookname_prefix name in
         if Sys.file_exists hook_name then begin
           let hook = Actions_helpers.run_hook hook_name in
           Actions.set_hook name hook
         end in
       String.Set.iter install_hook action_names;

       let reference_filename = Filename.concat
           test_source_directory (test_prefix ^ ".reference") in
       let make = try Sys.getenv "MAKE" with Not_found -> "make" in
       let initial_environment = Environments.from_bindings
           [
             Builtin_variables.make, make;
             Builtin_variables.test_file, test_basename;
             Builtin_variables.reference, reference_filename;
             Builtin_variables.test_source_directory, test_source_directory;
             Builtin_variables.test_build_directory_prefix,
               test_build_directory_prefix;
             Builtin_variables.promote, promote;
             Builtin_variables.timeout, default_timeout;
           ] in
       let rootenv =
         Environments.initialize Environments.Pre log initial_environment in
       let rootenv =
         interpret_environment_statements rootenv rootenv_statements in
       let rootenv = Environments.initialize Environments.Post log rootenv in
       let common_prefix = " ... testing '" ^ test_basename ^ "' with" in
       let initial_status =
         if skip_test then Skip_all_tests else Run rootenv
       in
       let summary =
         run_test_trees log common_prefix "" initial_status test_trees in
       Actions.clear_all_hooks();
       summary
    ) in
  if not Options.log_to_stderr then close_out log;
  begin match summary with
  | Some_failure ->
      if not Options.log_to_stderr then
        Sys.dump_file stderr ~prefix:"> " log_filename
  | No_failure ->
      if not Options.keep_test_dir_on_success then
        clean_test_build_directory ()
  end

let is_test s =
  match tsl_block_of_file s with
  | _ -> true
  | exception _ -> false

let ignored s =
  s = "" || s.[0] = '_' || s.[0] = '.'

let sort_strings = List.sort String.compare

let find_test_dirs dir =
  let res = ref [] in
  let rec loop dir =
    let contains_tests = ref false in
    Array.iter (fun s ->
        if ignored s then ()
        else begin
          let s = dir ^ "/" ^ s in
          if Sys.is_directory s then loop s
          else if not !contains_tests && is_test s then contains_tests := true
        end
      ) (Sys.readdir dir);
    if !contains_tests then res := dir :: !res
  in
  loop dir;
  sort_strings !res

let list_tests dir =
  let res = ref [] in
  if Sys.is_directory dir then begin
    Array.iter (fun s ->
        if ignored s then ()
        else begin
          let s' = dir ^ "/" ^ s in
          if Sys.is_directory s' || not (is_test s') then ()
          else res := s :: !res
        end
      ) (Sys.readdir dir)
  end;
  sort_strings !res

let () =
  init_tests_to_skip()

let () =
  let failed = ref false in
  let work_done = ref false in
  let list_tests dir =
    match list_tests dir with
    | [] -> failed := true
    | res -> List.iter print_endline res
  in
  let find_test_dirs dir = List.iter print_endline (find_test_dirs dir) in
  let doit f x = work_done := true; f x in
  List.iter (doit find_test_dirs) Options.find_test_dirs;
  List.iter (doit list_tests) Options.list_tests;
  List.iter (doit test_file) Options.files_to_test;
  if not !work_done then print_usage();
  if !failed || not !work_done then exit 1
