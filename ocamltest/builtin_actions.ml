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
  (fun log env ->
    Printf.fprintf log "The pass action always succeeds.\n%!";
    Pass env)

let skip = make
  "skip"
  (fun _log _env -> Skip "The skip action always skips.")

let fail = make
  "fail"
  (fun _log _env -> Fail "The fail action always fails.")

let dumpenv = make
  "dumpenv"
  (fun log env ->
    Environments.dump log env; Pass env)

let unix = make
  "unix"
  (fun log env ->
    if Ocamltest_config.unix then
    begin
      Printf.fprintf log
        "The unix action succeeds because we are on a Unix system.\n%!";
      Pass env
    end else
      Skip "The unix action skips because we are on a Windows system.")

let windows = make
  "windows"
  (fun log env ->
    if not Ocamltest_config.unix then
    begin
      Printf.fprintf log
        "The windows action succeeds because we are on a Windows system.\n%!";
      Pass env
    end else
      Skip "The windows action skips because we are on a Unix system.")

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
