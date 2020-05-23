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

(* Helper functions when writing actions *)

open Ocamltest_stdlib
open Actions
open A.Infix

let pass_or_skip test pass_reason skip_reason =
  if test
  then A.return (Eff.pass_with_reason pass_reason)
  else A.return (Eff.skip_with_reason skip_reason)

let skip_with_reason reason =
  Actions.make "skip"
    (A.return (Eff.skip_with_reason reason))

let mkreason what commandline exitcode =
  Printf.sprintf "%s: command\n%s\nfailed with exit code %d"
    what commandline exitcode

let testfile =
  A.map Option.get (A.lookup Builtin_variables.test_file)

let test_source_directory =
  A.safe_lookup Builtin_variables.test_source_directory

let test_build_directory =
  A.safe_lookup Builtin_variables.test_build_directory

let test_build_directory_prefix =
  A.safe_lookup Builtin_variables.test_build_directory_prefix

let words_of_variable variable =
  A.map String.words (A.safe_lookup variable)

let int_of_variable variable =
  let+ s = A.safe_lookup variable in
  Option.value ~default:0 (int_of_string_opt s)

let files = words_of_variable Builtin_variables.files

let setup_build_env add_testfile additional_files =
  let some_files =
    let+ files = files
    and+ additional_files = additional_files in
    additional_files @ files
  in
  let+ files =
    if add_testfile
    then
      let+ testfile = testfile and+ some_files = some_files in
      testfile :: some_files
    else
      some_files
  and+ test_source_directory = test_source_directory
  and+ test_build_directory = test_build_directory in
  Eff.setup_symlinks test_source_directory test_build_directory files

let setup_simple_build_env add_testfile additional_files =
  A.add Builtin_variables.test_build_directory
    test_build_directory_prefix
    (A.both (setup_build_env add_testfile additional_files) A.env)

let run_params
    ?environment
    ?(stdin_variable=Builtin_variables.stdin)
    ?(stdout_variable=Builtin_variables.stdout)
    ?(stderr_variable=Builtin_variables.stderr)
    ?(append=false)
    ?(timeout=0) ()
  =
  let+ strace = A.lookup_as_bool Strace.strace
  and+ strace_logfile =
    let+ strace_logfile_name =
      let+ action_name = A.safe_lookup Actions.action_name in
      Strace.get_logfile_name action_name
    and+ test_build_directory = test_build_directory in
    Filename.make_path [test_build_directory; strace_logfile_name]
  and+ strace_flags = A.safe_lookup Strace.strace_flags
  and+ stdin_filename = A.safe_lookup stdin_variable
  and+ stdout_filename = A.safe_lookup stdout_variable
  and+ stderr_filename = A.safe_lookup stderr_variable
  and+ environment =
    let+ environment =
      match environment with
      | None -> A.return [||]
      | Some a -> a
    and+ env = A.env in
    Array.append environment (Environments.to_system_env env)
  in
  {
    Eff.environment;
    stdin_filename;
    stdout_filename;
    stderr_filename;
    append;
    timeout;
    strace = (strace = Some true);
    strace_logfile;
    strace_flags;
  }

let run
    (log_message : string)
    (can_skip : bool)
    (prog_variable : Variables.t)
    (args_variable : Variables.t option)
  =
  let+ program = A.lookup prog_variable
  and+ arguments =
    match args_variable with
    | None -> A.return ""
    | Some variable -> A.safe_lookup variable
  and+ expected_exit_status =
    int_of_variable Builtin_variables.exit_status
  and+ run_params = run_params () in
  match program with
  | None ->
      let msg =
        Printf.sprintf "%s: variable %s is undefined"
          log_message (Variables.name_of_variable prog_variable)
      in
      Eff.fail_with_reason msg
  | Some program ->
      let commandline = [program; arguments] in
      let run = Eff.run_cmd run_params commandline in
      Eff.with_exit_code expected_exit_status
        (if can_skip then Eff.with_skip_code 125 run else run)

let run
    (log_message : string)
    (redirect_output : bool)
    (can_skip : bool)
    (prog_variable : Variables.t)
    (args_variable : Variables.t option) =
  let run = run log_message can_skip prog_variable args_variable in
  if redirect_output then
    let output = A.safe_lookup Builtin_variables.output in
    A.add_if_undefined Builtin_variables.stdout output
      (A.add_if_undefined Builtin_variables.stderr output run)
  else
    run

let run_program =
  run
    "Running program"
    true
    false
    Builtin_variables.program
    (Some Builtin_variables.arguments)

let run_script =
  let response_file = Filename.temp_file "ocamltest-" ".response" in
  A.add Builtin_variables.ocamltest_response (A.return response_file)
    (let+ eff =
       run "Running script"
         true
         true
         Builtin_variables.script
         None
     in
     Eff.seq [
       Eff.echo "Script should write its response to %s" response_file;
       eff;
       Eff.force_remove response_file ])

       (* (match Modifier_parser.modifiers_of_file response_file with *)
       (*  | modifiers -> *)
       (*      A.apply_modifiers modifiers (let+ (), env = A.with_env (A.return ()) in *)
       (*                                   (fun r -> r, env)) *)
       (*  | exception Failure reason -> *)
       (*      let+ (), env = A.with_env (A.return ()) in *)
       (*      Fun.const (Result.fail_with_reason reason, env) *)
       (*  | exception Variables.No_such_variable name -> *)
       (*      let reason = *)
       (*        Printf.sprintf "error in script response: unknown variable %s" name *)
       (*      in *)
       (*      let+ (), env = A.with_env (A.return ()) in *)
       (*      Fun.const (Result.fail_with_reason reason, env)) *)

let run_hook hook_name =
  let response_file = Filename.temp_file "ocamltest-" ".response" in
  A.add Builtin_variables.ocamltest_response (A.return response_file)
    (let+ env = A.env in
     let settings =
       {
         Eff.environment = Environments.to_system_env env;
         stdin_filename = "";
         stdout_filename = "";
         stderr_filename = "";
         append = false;
         timeout = 0;
         strace = false;
         strace_logfile = "";
         strace_flags = "";
       }
     in
     Eff.seq
       [ Eff.echo "Entering run_hook for hook %s\n%!" hook_name;
         Eff.echo "Hook should write its response to %s" response_file;
         Eff.with_skip_code 125 (Eff.run_cmd settings ["sh"; hook_name]);
         Eff.force_remove response_file ])

let check_output kind_of_output output_variable reference_variable =
  let to_int = function None -> 0 | Some s -> int_of_string s in
  let skip_lines =
    A.map to_int (A.lookup Builtin_variables.skip_header_lines) in
  let skip_bytes =
    A.map to_int (A.lookup Builtin_variables.skip_header_bytes) in
  let reference_filename = A.safe_lookup reference_variable in
  let output_filename = A.safe_lookup output_variable in
  let+ reference_filename = reference_filename
  and+ output_filename = output_filename
  and+ skip_lines = skip_lines
  and+ skip_bytes = skip_bytes
  and+ promote = A.lookup_as_bool Builtin_variables.promote in
  let files =
    {
      Filecompare.filetype = Filecompare.Text;
      Filecompare.reference_filename = reference_filename;
      Filecompare.output_filename = output_filename
    } in
  let ignore_header_conf = {
    Filecompare.lines = skip_lines;
    Filecompare.bytes = skip_bytes;
  } in
  Eff.seq
    [ Eff.echo "Comparing %s output %s to reference %s\n%!"
        kind_of_output output_filename
        reference_filename;
      Eff.check_files ~kind_of_output ~promote ignore_header_conf files ]
