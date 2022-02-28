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

let hasunix = make
  "hasunix"
  (Actions_helpers.pass_or_skip (Ocamltest_config.libunix <> None)
    "unix library available"
    "unix library not available")

let libunix = make
  "libunix"
  (Actions_helpers.pass_or_skip (Ocamltest_config.libunix = Some true)
    "libunix available"
    "libunix not available")

let libwin32unix = make
  "libwin32unix"
  (Actions_helpers.pass_or_skip (Ocamltest_config.libunix = Some false)
    "win32 variant of the unix library available"
    "win32 variant of the unix library not available")

let hassysthreads = make
  "hassysthreads"
  (Actions_helpers.pass_or_skip Ocamltest_config.systhreads
    "systhreads library available"
    "systhreads library not available")

let hasstr = make
  "hasstr"
  (Actions_helpers.pass_or_skip Ocamltest_config.str
    "str library available"
    "str library not available")

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

let is_bsd_system s =
  match s with
  | "bsd_elf" | "netbsd" | "freebsd" | "openbsd" -> true
  | _ -> false

let bsd = make
  "bsd"
  (Actions_helpers.pass_or_skip (is_bsd_system Ocamltest_config.system)
    "on a BSD system"
    "not on a BSD system")

let not_bsd = make
  "not-bsd"
  (Actions_helpers.pass_or_skip (not (is_bsd_system Ocamltest_config.system))
    "not on a BSD system"
    "on a BSD system")

let macos_system = "macosx"

let macos = make
  "macos"
  (Actions_helpers.pass_or_skip (Ocamltest_config.system = macos_system)
    "on a MacOS system"
    "not on a MacOS system")

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

let arch_arm = make
  "arch_arm"
  (Actions_helpers.pass_or_skip (String.equal Ocamltest_config.arch "arm")
     "Target is ARM architecture"
     "Target is not ARM architecture")

let arch_arm64 = make
  "arch_arm64"
  (Actions_helpers.pass_or_skip (String.equal Ocamltest_config.arch "arm64")
     "Target is ARM64 architecture"
     "Target is not ARM64 architecture")

 let arch_amd64 = make
  "arch_amd64"
  (Actions_helpers.pass_or_skip (String.equal Ocamltest_config.arch "amd64")
     "Target is AMD64 architecture"
     "Target is not AMD64 architecture")

 let arch_i386 = make
  "arch_i386"
  (Actions_helpers.pass_or_skip (String.equal Ocamltest_config.arch "i386")
     "Target is i386 architecture"
     "Target is not i386 architecture")

let arch_power = make
  "arch_power"
  (Actions_helpers.pass_or_skip (String.equal Ocamltest_config.arch "power")
    "Target is POWER architecture"
    "Target is not POWER architecture")

let function_sections = make
  "function_sections"
  (Actions_helpers.pass_or_skip (Ocamltest_config.function_sections)
     "Target supports function sections"
     "Target does not support function sections")

let frame_pointers = make
  "frame_pointers"
  (Actions_helpers.pass_or_skip (Ocamltest_config.frame_pointers)
     "frame-pointers available"
     "frame-pointers not available")

let has_symlink = make
  "has_symlink"
  (Actions_helpers.pass_or_skip (Unix.has_symlink () )
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

let file_exists_action _log env =
  match Environments.lookup Builtin_variables.file env with
    | None ->
      let reason = reason_with_fallback env "the file variable is undefined" in
      let result = Result.fail_with_reason reason in
      (result, env)
    | Some filename ->
      if Sys.file_exists filename
      then begin
        let default_reason = Printf.sprintf "File %s exists" filename in
        let reason = reason_with_fallback env default_reason in
        let result = Result.pass_with_reason reason in
        (result, env)
      end else begin
        let default_reason =
          Printf.sprintf "File %s does not exist" filename
        in
        let reason = reason_with_fallback env default_reason in
        let result = Result.fail_with_reason reason in
        (result, env)
      end
let file_exists = make "file-exists" file_exists_action

let copy_action log env =
  let do_copy src dst =
    let (entry_type, f) =
      if Sys.is_directory src
      then ("directory", Sys.copy_directory)
      else ("file", Sys.copy_file)
    in
    Printf.fprintf log "Copying %s %s to %s\n%!" entry_type src dst;
    f src dst
  in
  let src = Environments.lookup Builtin_variables.src env in
  let dst = Environments.lookup Builtin_variables.dst env in
  match (src, dst) with
    | (None, _) | (_, None) ->
      let reason = reason_with_fallback env "src or dst are undefined" in
      let result = Result.fail_with_reason reason in
      (result, env)
    | (Some src, Some dst) ->
      let f =
        if String.ends_with ~suffix:"/" dst
        then fun src -> do_copy src (dst ^ (Filename.basename src))
        else fun src -> do_copy src dst
      in
      List.iter f (String.words src);
      (Result.pass, env)

let copy = make "copy" copy_action

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
    pass;
    skip;
    fail;
    cd;
    dumpenv;
    hasunix;
    hassysthreads;
    hasstr;
    libunix;
    libwin32unix;
    windows;
    not_windows;
    bsd;
    not_bsd;
    macos;
    arch32;
    arch64;
    has_symlink;
    setup_build_env;
    setup_simple_build_env;
    run;
    script;
    check_program_output;
    arch_arm;
    arch_arm64;
    arch_amd64;
    arch_i386;
    arch_power;
    function_sections;
    frame_pointers;
    file_exists;
    copy;
  ]
