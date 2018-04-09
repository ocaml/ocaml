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

let reason_with_fallback env fallback =
  match Environments.lookup Builtin_variables.reason env with
  | None -> fallback
  | Some reason -> reason

let pass = make
  "pass"
  (fun _log env ->
    let reason = reason_with_fallback env "the pass action always succeeds" in
    let result = Result.pass_with_reason reason in
    (result, env))

let skip = make
  "skip"
  (fun _log env ->
    let reason = reason_with_fallback env "the skip action always skips" in
    let result = Result.skip_with_reason reason in
    (result, env))

let fail = make
  "fail"
  (fun _log env ->
    let reason = reason_with_fallback env "the fail action always fails" in
    let result = Result.fail_with_reason reason in
    (result, env))

let cd = make
  "cd"
  (fun _log env ->
    let cwd = Environments.safe_lookup Builtin_variables.cwd env in
    begin
      try
        Sys.chdir cwd; (Result.pass, env)
      with _ ->
        let reason = "Could not chidir to \"" ^ cwd ^ "\"" in
        let result = Result.fail_with_reason reason in
        (result, env)
    end)    

let dumpenv = make
  "dumpenv"
  (fun log env ->
    Environments.dump log env; (Result.pass, env))

let libunix = make
  "libunix"
  (Actions_helpers.pass_or_skip Ocamltest_config.libunix
    "libunix available"
    "libunix not available")

let libwin32unix = make
  "libwin32unix"
  (Actions_helpers.pass_or_skip (not Ocamltest_config.libunix)
    "libwin32unix available"
    "libwin32unix not available")

let windows_OS = "Windows_NT"

let get_OS () = Sys.safe_getenv "OS"

let windows = make
  "windows"
  (Actions_helpers.pass_or_skip (get_OS () = windows_OS)
    "running on Windows"
    "not running on Windows")

let not_windows = make
  "not-windows"
  (Actions_helpers.pass_or_skip (get_OS () <> windows_OS)
    "not running on Windows"
    "running on Windows")

let bsd_system = "bsd_elf"

let bsd = make
  "bsd"
  (Actions_helpers.pass_or_skip (Ocamltest_config.system = bsd_system)
    "on a BSD system"
    "not on a BSD system")

let not_bsd = make
  "not-bsd"
  (Actions_helpers.pass_or_skip (Ocamltest_config.system <> bsd_system)
    "not on a BSD system"
    "on a BSD system")

let arch32 = make
  "arch32"
  (Actions_helpers.pass_or_skip (Sys.word_size = 32)
    "32-bit architecture"
    "non-32-bit architecture")

let arch64 = make
  "arch64"
  (Actions_helpers.pass_or_skip (Sys.word_size = 64)
    "64-bit architecture"
    "non-64-bit architecture")

let has_symlink = make
  "has_symlink"
  (Actions_helpers.pass_or_skip (Sys.has_symlink () )
    "symlinks available"
    "symlinks not available")

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
    cd;
    dumpenv;
    libunix;
    libwin32unix;
    windows;
    not_windows;
    bsd;
    not_bsd;
    arch32;
    arch64;
    has_symlink;
    setup_build_env;
    run;
    script;
    check_program_output;
  ]
