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

(* Definition of a few built-in actions *)

open Ocamltest_stdlib
open Actions

let pass = make
  "pass"
  (fun _log env ->
    let result =
      Result.pass_with_reason "The pass action always succeeds." in
    (result, env))

let skip = make
  "skip"
  (fun _log env ->
    let result = Result.skip_with_reason "The skip action always skips." in
    (result, env))

let fail = make
  "fail"
  (fun _log env ->
    let result = Result.fail_with_reason "The fail action always fails." in
    (result, env))

let dumpenv = make
  "dumpenv"
  (fun log env ->
    Environments.dump log env; (Result.pass, env))

let unix = make
  "unix"
  (Actions_helpers.pass_or_skip Ocamltest_config.unix
    "The unix action succeeds because we are on a Unix system."
    "The unix action skips because we are on a Windows system.")

let windows = make
  "windows"
  (Actions_helpers.pass_or_skip (not Ocamltest_config.unix)
    "The windows action succeeds because we are on a Windows system."
    "The windows action skips because we are on a Unix system.")

let setup_build_env = make
  "setup-build-env"
  (Actions_helpers.setup_build_env true [])

let setup_simple_build_env = make
  "setup-simple-build-env"
  (Actions_helpers.setup_simple_build_env true [])

let run = make
  "run"
  Actions_helpers.run_program

let script = make
  "script"
  Actions_helpers.run_script

let check_program_output = make
  "check-program-output"
  (Actions_helpers.check_output "program"
    Builtin_variables.output
    Builtin_variables.reference)

let initialize_test_exit_status_variables _log env =
  Environments.add_bindings
  [
    Builtin_variables.test_pass, "0";
    Builtin_variables.test_fail, "1";
    Builtin_variables.test_skip, "125";
  ] env

let _ =
  Environments.register_initializer
    "test_exit_status_variables" initialize_test_exit_status_variables;
  List.iter register
  [
    pass;
    skip;
    fail;
    dumpenv;
    unix;
    windows;
    setup_build_env;
    run;
    script;
    check_program_output;
  ]
