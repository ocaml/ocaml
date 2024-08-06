(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                           Tim McGilchrist, Tarides                     *)
(*                                                                        *)
(*   Copyright 2024 Tarides.                                              *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Ocamltest_stdlib
open Actions

let env_setting env_reader default_setting =
  Printf.sprintf "%s=%s"
    env_reader.Clflags.env_var
    (env_reader.Clflags.print default_setting)

let default_debugger_env = [|
    "TERM=dumb";
    env_setting Clflags.color_reader Misc.Color.default_setting;
    env_setting Clflags.error_style_reader Misc.Error_style.default_setting;
  |]

let env_with_lib_unix env =
  let libunixdir = Ocaml_directories.libunix in
  let newlibs =
    match Environments.lookup Ocaml_variables.caml_ld_library_path env with
    | None -> libunixdir
    | Some libs -> libunixdir ^ " " ^ libs
  in
  Environments.add Ocaml_variables.caml_ld_library_path newlibs env

type debugger_type = LLDB | GDB | Bytecode

let debugger_type_to_string = function
  | LLDB -> "lldb"
  | GDB -> "gdb"
  | Bytecode -> "ocamldebug"

let debug debugger_type log env =
  let program = Environments.safe_lookup Builtin_variables.program env in
  let what = Printf.sprintf "Debugging program %s with %s" program
               (debugger_type_to_string debugger_type) in
  Printf.fprintf log "%s\n%!" what;
  let commandline = match debugger_type with
    | LLDB -> [
        "lldb";
        Debugger_flags.lldb_default_flags;
        "-s " ^ (Environments.safe_lookup
                   Debugger_variables.debugger_script env);
        program ]
    | GDB -> [
        "gdb";
        Debugger_flags.gdb_default_flags;
        "-x " ^ (Environments.safe_lookup
                   Debugger_variables.debugger_script env);
        program ]
    | Bytecode -> [
        Ocaml_commands.ocamlrun_ocamldebug;
        Debugger_flags.ocamldebug_default_flags;
        program
      ]
  in
  let systemenv =
    match debugger_type with
    | LLDB | GDB ->
       Environments.append_to_system_env
         default_debugger_env env
    | Bytecode ->
       Environments.append_to_system_env
         default_debugger_env
         (env_with_lib_unix env)
  in
  let stdin_variable = match debugger_type with
    | LLDB | GDB ->  Builtin_variables.dev_null
    | Bytecode -> Debugger_variables.debugger_script
  in
  let expected_exit_status = 0 in
  let exit_status =
    Actions_helpers.run_cmd
      ~environment:systemenv
      ~stdin_variable:stdin_variable
      ~stdout_variable:Builtin_variables.output
      ~stderr_variable:Builtin_variables.output
      ~append:true
      log (env_with_lib_unix env) commandline in
  if exit_status=expected_exit_status
  then (Result.pass, env)
  else begin
    let reason =
      (Actions_helpers.mkreason
         what (String.concat " " commandline) exit_status) in
    (Result.fail_with_reason reason, env)
  end

let lldb =
  Actions.make ~name:"lldb"
    ~description:"Run LLDB on the program" (debug LLDB)

let gdb =
  Actions.make ~name:"gdb"
    ~description:"Run GDB on the program" (debug GDB)

let ocamldebug =
  Actions.make ~name:"ocamldebug"
    ~description:"Run ocamldebug on the program" (debug Bytecode)

let initialize_test_exit_status_variables _log env =
  Environments.add_bindings
    [
      Builtin_variables.test_pass, "0";
      Builtin_variables.test_fail, "1";
      Builtin_variables.test_skip, "125";
    ] env

let _ =
  Environments.register_initializer Environments.Post
    "test_exit_status_variables" initialize_test_exit_status_variables;
  List.iter register
    [
      lldb;
      gdb;
      ocamldebug;
    ]
