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

let mkreason what commandline exitcode =
  Printf.sprintf "%s: command\n%s\nfailed with exit code %d"
    what commandline exitcode

let testfile env =
  match Environments.lookup Builtin_variables.test_file env with
  | None -> assert false
  | Some t -> t

let test_source_directory env =
  Environments.safe_lookup Builtin_variables.test_source_directory env

let test_build_directory env =
  Environments.safe_lookup Builtin_variables.test_build_directory env

let test_build_directory_prefix env =
  Environments.safe_lookup Builtin_variables.test_build_directory_prefix env

let words_of_variable env variable =
  String.words (Environments.safe_lookup variable env)

let files env = words_of_variable env Builtin_variables.files

let setup_symlinks test_source_directory build_directory files =
  let symlink filename =
    let src = Filename.concat test_source_directory filename in
    let cmd = "ln -sf " ^ src ^" " ^ build_directory in
    Sys.run_system_command cmd in
  let copy filename =
    let src = Filename.concat test_source_directory filename in
    let dst = Filename.concat build_directory filename in
    Sys.copy_file src dst in
  let f = if Sys.os_type="Win32" then copy else symlink in
  Sys.make_directory build_directory;
  List.iter f files

let setup_build_env add_testfile additional_files (_log : out_channel) env =
  let build_dir = (test_build_directory env) in
  let some_files = additional_files @ (files env) in
  let files =
    if add_testfile
    then (testfile env) :: some_files
    else some_files in
  setup_symlinks (test_source_directory env) build_dir files;
  Sys.chdir build_dir;
  Pass env

let setup_simple_build_env add_testfile additional_files log env =
  let build_env = Environments.add
    Builtin_variables.test_build_directory
    (test_build_directory_prefix env) env in
  setup_build_env add_testfile additional_files log build_env

let run_cmd
    ?(environment=[||])
    ?(stdin_variable=Builtin_variables.stdin)
    ?(stdout_variable=Builtin_variables.stdout)
    ?(stderr_variable=Builtin_variables.stderr)
    ?(append=false)
    ?(timeout=0)
    log env cmd
  =
  let log_redirection std filename =
    if filename<>"" then
    begin
      Printf.fprintf log "  Redirecting %s to %s \n%!" std filename
    end in
  let lst = List.concat (List.map String.words cmd) in
  let quoted_lst =
    if Sys.os_type="Win32"
    then List.map Filename.maybe_quote lst
    else lst in
  let cmd' = String.concat " " quoted_lst in
  Printf.fprintf log "Commandline: %s\n" cmd';
  let progname = List.hd quoted_lst in
  let arguments = Array.of_list quoted_lst in
  let stdin_filename = Environments.safe_lookup stdin_variable env in
  let stdout_filename = Environments.safe_lookup stdout_variable env in
  let stderr_filename = Environments.safe_lookup stderr_variable env in
  log_redirection "stdin" stdin_filename;
  log_redirection "stdout" stdout_filename;
  log_redirection "stderr" stderr_filename;
  Run_command.run {
    Run_command.progname = progname;
    Run_command.argv = arguments;
    Run_command.envp = environment;
    Run_command.stdin_filename = stdin_filename;
    Run_command.stdout_filename = stdout_filename;
    Run_command.stderr_filename = stderr_filename;
    Run_command.append = append;
    Run_command.timeout = timeout;
    Run_command.log = log
  }

let caml_ld_library_path = "CAML_LD_LIBRARY_PATH"

let string_of_binding variable value =
  if variable=Builtin_variables.ld_library_path then begin
    let current_value =
      try Sys.getenv caml_ld_library_path with Not_found -> "" in
    let local_value =
      (String.concat Filename.path_sep (String.words value)) in
    let new_value =
      if local_value="" then current_value else
      if current_value="" then local_value else
      String.concat Filename.path_sep [local_value; current_value] in
    Printf.sprintf "%s=%s" caml_ld_library_path new_value
  end else Environments.string_of_binding variable value

let run
    (log_message : string)
    (redirect_output : bool)
    (can_skip : bool)
    (prog_variable : Variables.t)
    (args_variable : Variables.t option)
    (log : out_channel)
    (env : Environments.t)
  =
  match Environments.lookup prog_variable env with
  | None ->
    let msg = Printf.sprintf "%s: variable %s is undefined"
      log_message (Variables.name_of_variable prog_variable) in
    Fail msg
  | Some program ->
    let arguments = match args_variable with
      | None -> ""
      | Some variable -> Environments.safe_lookup variable env in
    let commandline = [program; arguments] in
    let what = log_message ^ " " ^ program ^ " " ^
    begin if arguments="" then "without any argument"
    else "with arguments " ^ arguments
    end in
    let output = program ^ ".output" in
    let execution_env =
      if redirect_output then begin
        let bindings =
        [
          Builtin_variables.stdout, output;
          Builtin_variables.stderr, output
        ] in
        Environments.add_bindings bindings env
      end else env in
    let systemenv =
      Environments.to_system_env ~f:string_of_binding execution_env in
    match run_cmd ~environment:systemenv log execution_env commandline with
      | 0 ->
        let newenv =
          if redirect_output
          then Environments.add Builtin_variables.output output env
          else env in
        Pass newenv
      | _ as exitcode ->
        if exitcode = 125 && can_skip
        then Skip (mkreason
          what (String.concat " " commandline) exitcode)
        else Fail (mkreason
          what (String.concat " " commandline) exitcode)

let run_program =
  run
    "Running program"
    true
    false
    Builtin_variables.program
    (Some Builtin_variables.arguments)

let run_script =
  run
    "Running script"
    false
    true
    Builtin_variables.script
    None

let run_hook hook_name log input_env =
  Printf.fprintf log "Entering run_hook for hook %s\n%!" hook_name;
  let envfile = Filename.temp_file "ocamltest-" ".env" in
  Printf.fprintf log "Hook should write environemnt modifiers to %s\n%!"
    envfile;
  let hookenv = Environments.add
    Builtin_variables.ocamltest_env envfile input_env in
  let systemenv =
    Environments.to_system_env ~f:string_of_binding hookenv in
  let open Run_command in
  let settings = {
    progname = "sh";
    argv = [|"sh"; Filename.maybe_quote hook_name|];
    envp = systemenv;
    stdin_filename = "";
    stdout_filename = "";
    stderr_filename = "";
    append = false;
    timeout = 0;
    log = log;
  } in let exit_status = run settings in
  match exit_status with
    | 0 ->
      let modifiers = Environments.modifiers_of_file envfile in
      Pass (Environments.apply_modifiers hookenv modifiers)
    | _ ->
      let msg = Printf.sprintf "Hook returned %d" exit_status in
      if exit_status=125 then Skip msg else Fail msg

let check_output kind_of_output output_variable reference_variable log env =
  let reference_filename = Environments.safe_lookup reference_variable env in
  let output_filename = Environments.safe_lookup output_variable env in
  Printf.fprintf log "Comparing %s output %s to reference %s\n%!"
    kind_of_output output_filename reference_filename;
  let files =
  {
    Filecompare.filetype = Filecompare.Text;
    Filecompare.reference_filename = reference_filename;
    Filecompare.output_filename = output_filename
  } in
  match Filecompare.check_file files with
    | Filecompare.Same -> Pass env
    | Filecompare.Different ->
      let diff = Filecompare.diff files in
      let diffstr = match diff with
        | Ok difference -> difference
        | Error diff_file -> ("See " ^ diff_file) in
      let reason =
        Printf.sprintf "%s output %s differs from reference %s: \n%s\n"
        kind_of_output output_filename reference_filename diffstr in
      (Fail reason)
    | Filecompare.Unexpected_output ->
      let banner = String.make 40 '=' in
      let unexpected_output = Sys.string_of_file output_filename in
      let unexpected_output_with_banners = Printf.sprintf
        "%s\n%s%s\n" banner unexpected_output banner in
      let reason = Printf.sprintf
        "The file %s was expected to be empty because there is no \
          reference file %s but it is not:\n%s\n"
        output_filename reference_filename unexpected_output_with_banners in
      (Fail reason)
    | Filecompare.Error (commandline, exitcode) ->
      let reason = Printf.sprintf "The command %s failed with status %d"
        commandline exitcode in
      (Fail reason)
