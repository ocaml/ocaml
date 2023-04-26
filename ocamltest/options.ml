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

(* Description of ocamltest's command-line options *)

let show_objects title string_of_object objects =
  let print_object o = print_endline ("  " ^ (string_of_object o)) in
  print_endline title;
  List.iter print_object objects;
  exit 0

let string_of_action a =
  Printf.sprintf "%s: %s" (Actions.name a) (Actions.description a)

let string_of_test test =
  if test.Tests.test_run_by_default then
    test.Tests.test_name ^ " (run by default): " ^ test.Tests.test_description
  else
    test.Tests.test_name ^ ": " ^ test.Tests.test_description

let string_of_variable v =
  Printf.sprintf "%s: %s"
    (Variables.name_of_variable v)
    (Variables.description_of_variable v)

let show_actions () =
  let actions = Actions.get_registered_actions () in
  show_objects "Available actions are:" string_of_action actions

let show_tests () =
  let tests = Tests.get_registered_tests () in
  show_objects "Available tests are:" string_of_test tests

let show_variables () =
  let variables = Variables.get_registered_variables () in
  show_objects "Available variables are:" string_of_variable variables

let log_to_stderr = ref false

let promote = ref false

let default_timeout = ref 0

let keep_test_dir_on_success = ref false

let find_test_dirs = ref []

let list_tests = ref []

let show_timings = ref false

let translate = ref false
let style = ref Translate.Plain
let compact = ref false


let add_to_list r x =
  r := !r @ [x]

let commandline_options =
[
  ("-e", Arg.Set log_to_stderr, " Log to stderr instead of a file.");
  ("-promote", Arg.Set promote,
   " Overwrite reference files with the test output (experimental, unstable)");
  ("-show-actions", Arg.Unit show_actions, " Show available actions.");
  ("-show-tests", Arg.Unit show_tests, " Show available tests.");
  ("-show-variables", Arg.Unit show_variables, " Show available variables.");
  ("-show-timings", Arg.Set show_timings,
   " Show the wall clock time taken for each test file.");
  ("-timeout",
     Arg.Int (fun t -> if t >= 0
                       then default_timeout := t
                       else raise (Arg.Bad "negative timeout")),
     "<seconds> Set maximal execution time for every command (in seconds)");
  ("-find-test-dirs", Arg.String (add_to_list find_test_dirs),
   " Find directories that contain tests (recursive).");
  ("-list-tests", Arg.String (add_to_list list_tests),
   " List tests in given directory.");
  ("-keep-test-dir-on-success", Arg.Set keep_test_dir_on_success,
   " Keep the test directory (with the generated test artefacts) on success.");
  ("-translate", Arg.Set translate,
   " Translate the test script from old to new syntax");
  ("-compact", Arg.Set compact,
   " If translating, output the new script in compact mode.");
  ("-keep-lines", Arg.Unit (fun () -> style := Translate.Lines),
   " If translating, preserve line numbers in the output.");
  ("-keep-chars", Arg.Unit (fun () -> style := Translate.Chars),
   " If translating, preserve char offsets in the output.");
]

let files_to_test = ref []

let usage = "Usage: " ^ Sys.argv.(0) ^ " options files to test"

let () =
  Arg.parse (Arg.align commandline_options) (add_to_list files_to_test) usage

let log_to_stderr = !log_to_stderr
let files_to_test = !files_to_test
let promote = !promote
let default_timeout = !default_timeout
let find_test_dirs = !find_test_dirs
let list_tests = !list_tests
let keep_test_dir_on_success = !keep_test_dir_on_success
let show_timings = !show_timings
let translate = !translate
let style = !style
let compact = !compact
