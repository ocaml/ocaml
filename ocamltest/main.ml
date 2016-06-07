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

let tslprogram_of_file filename =
  let input_channel = open_in filename in
  let lexbuf = Lexing.from_channel input_channel in
  Location.init lexbuf filename;
  match Tsl_parser.tsl_program Tsl_lexer.token lexbuf with
    | exception e -> close_in input_channel; raise e
    | _ as tslprogram -> close_in input_channel; tslprogram
  
let print_usage () =
  Printf.eprintf "Usage: %s testfile\n" Sys.argv.(0)

let dump_tsl_program ppf = ()

let runtest ppf rootenv n (test, envspec) =
  Format.fprintf ppf "Running test #%d: %s\n" (n+1) test.Tests.test_name;
  let testenv = Tsl_semantics.interprete_statements rootenv envspec in
  let result = Tests.run ppf testenv test in
  Format.fprintf ppf "%s\n%!" (Actions.string_of_result result)

let initial_env filename =
  let add env (variable, value) = Environments.add variable value env in
  let l =
  [
    ("testfile", filename);
  ] in
  List.fold_left add Environments.empty l

let main () =
  let ppf = Format.std_formatter in
  if Array.length Sys.argv < 2 then begin
    print_usage();
    exit 1
  end;
  let filename = Sys.argv.(1) in
  let dirname = Filename.dirname filename in
  let basename = Filename.basename filename in
  let tslprogram = tslprogram_of_file filename in
  Sys.chdir dirname;
  let init_env = (initial_env basename) in
  let root_environment =
    Tsl_semantics.interprete_statements init_env tslprogram.Tsl_ast.root_environment in
  let tests_to_run = match tslprogram.Tsl_ast.tests with
    | [] -> List.map (fun t -> (t,[])) ( Tests.default_tests() )
    | _ as l -> Tsl_semantics.tests_of_ast ppf l in
  let actions_to_run =
    let tmptests = List.map fst tests_to_run in
    Tsl_semantics.actions_of_tests tmptests in
  let rootenv = Actions.update_environment root_environment actions_to_run in
  List.iteri (runtest ppf rootenv) tests_to_run

let _ = main()
