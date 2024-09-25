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
  ~name:"pass"
  ~description:"Always succeed"
  (fun _log env ->
    let reason = reason_with_fallback env "the pass action always succeeds" in
    let result = Result.pass_with_reason reason in
    (result, env))

let skip = make
  ~name:"skip"
  ~description:"Always skip the test"
  (fun _log env ->
    let reason = reason_with_fallback env "the skip action always skips" in
    let result = Result.skip_with_reason reason in
    (result, env))

let fail = make
  ~name:"fail"
  ~description:"Always fail"
  (fun _log env ->
    let reason = reason_with_fallback env "the fail action always fails" in
    let result = Result.fail_with_reason reason in
    (result, env))

let cd = make
  ~name:"cd"
  ~description:"Change working directory"
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
  ~name:"dumpenv"
  ~description:"Dump the environment"
  (fun log env ->
    Environments.dump log env; (Result.pass, env))

let hasunix = make
  ~name:"hasunix"
  ~description:"Pass if the unix library is available"
  (Actions_helpers.pass_or_skip (Ocamltest_config.libunix <> None)
    "unix library available"
    "unix library not available")

let libunix = make
  ~name:"libunix"
  ~description:"Pass if libunix is available"
  (Actions_helpers.pass_or_skip (Ocamltest_config.libunix = Some true)
    "libunix available"
    "libunix not available")

let libwin32unix = make
  ~name:"libwin32unix"
  ~description:"Pass if the win32 variant of the unix library is available"
  (Actions_helpers.pass_or_skip (Ocamltest_config.libunix = Some false)
    "win32 variant of the unix library available"
    "win32 variant of the unix library not available")

let hassysthreads = make
  ~name:"hassysthreads"
  ~description:"Pass if the systhreads library is available"
  (Actions_helpers.pass_or_skip Ocamltest_config.systhreads
    "systhreads library available"
    "systhreads library not available")

let hasstr = make
  ~name:"hasstr"
  ~description:"Pass if the str library is available"
  (Actions_helpers.pass_or_skip Ocamltest_config.str
    "str library available"
    "str library not available")

let windows_OS = "Windows_NT"

let get_OS () = Sys.safe_getenv "OS"

let windows = make
  ~name:"windows"
  ~description:"Pass if running on Windows"
  (Actions_helpers.pass_or_skip (get_OS () = windows_OS)
    "running on Windows"
    "not running on Windows")

let not_windows = make
  ~name:"not-windows"
  ~description:"Pass if not running on Windows"
  (Actions_helpers.pass_or_skip (get_OS () <> windows_OS)
    "not running on Windows"
    "running on Windows")

let not_msvc = make
  ~name:"not-msvc"
  ~description:"Pass if not using MSVC / clang-cl"
  (Actions_helpers.pass_or_skip (Ocamltest_config.ccomptype <> "msvc")
    "not using MSVC / clang-cl"
    "using MSVC / clang-cl")

let is_bsd_system s =
  match s with
  | "bsd_elf" | "netbsd" | "freebsd" | "openbsd" -> true
  | _ -> false

let bsd = make
  ~name:"bsd"
  ~description:"Pass if running on a BSD system"
  (Actions_helpers.pass_or_skip (is_bsd_system Ocamltest_config.system)
    "on a BSD system"
    "not on a BSD system")

let not_bsd = make
  ~name:"not-bsd"
  ~description:"Pass if not running on a BSD system"
  (Actions_helpers.pass_or_skip (not (is_bsd_system Ocamltest_config.system))
    "not on a BSD system"
    "on a BSD system")

let linux_system = "linux"

let linux = make
  ~name:"linux"
  ~description:"Pass if running on a Linux system"
  (Actions_helpers.pass_or_skip (Ocamltest_config.system = linux_system)
     "on a Linux system"
     "not on a Linux system")

let macos_system = "macosx"

let macos = make
  ~name:"macos"
  ~description:"Pass if running on a MacOS system"
  (Actions_helpers.pass_or_skip (Ocamltest_config.system = macos_system)
    "on a MacOS system"
    "not on a MacOS system")

let not_macos_amd64_tsan = make
  ~name:"not_macos_amd64_tsan"
  ~description:"Pass if not running on a MacOS amd64 system with TSan enabled"
  (Actions_helpers.pass_or_skip
     (not ((Ocamltest_config.system = macos_system)
           && (String.equal Ocamltest_config.arch "amd64")
           && (Ocamltest_config.tsan)))
     "not on a MacOS amd64 system with TSan enabled"
     "on a MacOS amd64 system with TSan enabled")

let arch32 = make
  ~name:"arch32"
  ~description:"Pass if running on a 32-bit architecture"
  (Actions_helpers.pass_or_skip (Sys.word_size = 32)
    "32-bit architecture"
    "non-32-bit architecture")

let arch64 = make
  ~name:"arch64"
  ~description:"Pass if running on a 64-bit architecture"
  (Actions_helpers.pass_or_skip (Sys.word_size = 64)
    "64-bit architecture"
    "non-64-bit architecture")

let arch_arm = make
  ~name:"arch_arm"
  ~description:"Pass if target is an ARM architecture"
  (Actions_helpers.pass_or_skip (String.equal Ocamltest_config.arch "arm")
     "Target is ARM architecture"
     "Target is not ARM architecture")

let arch_arm64 = make
  ~name:"arch_arm64"
  ~description:"Pass if target is an ARM64 architecture"
  (Actions_helpers.pass_or_skip (String.equal Ocamltest_config.arch "arm64")
     "Target is ARM64 architecture"
     "Target is not ARM64 architecture")

 let arch_amd64 = make
  ~name:"arch_amd64"
  ~description:"Pass if target is an AMD64 architecture"
  (Actions_helpers.pass_or_skip (String.equal Ocamltest_config.arch "amd64")
     "Target is AMD64 architecture"
     "Target is not AMD64 architecture")

 let arch_i386 = make
  ~name:"arch_i386"
  ~description:"Pass if target is an i386 architecture"
  (Actions_helpers.pass_or_skip (String.equal Ocamltest_config.arch "i386")
     "Target is i386 architecture"
     "Target is not i386 architecture")

let arch_power = make
  ~name:"arch_power"
  ~description:"Pass if target is a POWER architecture"
  (Actions_helpers.pass_or_skip (String.equal Ocamltest_config.arch "power")
    "Target is POWER architecture"
    "Target is not POWER architecture")

let arch_riscv = make
  ~name:"arch_riscv"
  ~description:"Pass if target is a RISC-V architecture"
  (Actions_helpers.pass_or_skip (String.equal Ocamltest_config.arch "riscv")
     "Target is RISC-V architecture"
     "Target is not RISC-V architecture")

let arch_s390x = make
  ~name:"arch_s390x"
  ~description:"Pass if target is a S390x architecture"
  (Actions_helpers.pass_or_skip (String.equal Ocamltest_config.arch "s390x")
     "Target is S390x architecture"
     "Target is not S390x architecture")

let function_sections = make
  ~name:"function_sections"
  ~description:"Pass if target supports function sections"
  (Actions_helpers.pass_or_skip (Ocamltest_config.function_sections)
     "Target supports function sections"
     "Target does not support function sections")

let frame_pointers = make
  ~name:"frame_pointers"
  ~description:"Pass if frame pointers are available"
  (Actions_helpers.pass_or_skip (Ocamltest_config.frame_pointers)
     "frame-pointers available"
     "frame-pointers not available")

let tsan = make
  ~name:"tsan"
  ~description:"Pass if thread sanitizer is supported"
  (Actions_helpers.pass_or_skip (Ocamltest_config.tsan)
     "tsan available"
     "tsan not available")

let no_tsan = make
  ~name:"no-tsan"
  ~description:"Pass if thread sanitizer is not supported"
  (Actions_helpers.pass_or_skip (not Ocamltest_config.tsan)
     "tsan not available"
     "tsan available")

let has_symlink = make
  ~name:"has_symlink"
  ~description:"Pass if symbolic links are available"
  (Actions_helpers.pass_or_skip (Unix.has_symlink () )
    "symlinks available"
    "symlinks not available")

let setup_build_env = make
  ~name:"setup-build-env"
  ~description:"Create a dedicated directory for the test and populates it"
  (Actions_helpers.setup_build_env true [])

let setup_simple_build_env = make
  ~name:"setup-simple-build-env"
  ~description:"Do not create a dedicated directory, but only sets the \
    test_build_directory variable"
  (Actions_helpers.setup_simple_build_env true [])

let run = make
  ~name:"run"
  ~description:"Run the program"
  Actions_helpers.run_program

let script = make
  ~name:"script"
  ~description:"Run the script specified by the script variable"
  Actions_helpers.run_script

let check_program_output = make
  ~name:"check-program-output"
  ~description:"Compare the output of the program with its reference"
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
let file_exists = make
  ~name:"file-exists"
  ~description:"Pass if there is a file at the path contained in variable \
    `file`"
  file_exists_action

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

let copy = make ~name:"copy" ~description:"Copy a file" copy_action

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
    not_msvc;
    bsd;
    not_bsd;
    linux;
    macos;
    not_macos_amd64_tsan;
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
    arch_riscv;
    arch_s390x;
    function_sections;
    frame_pointers;
    file_exists;
    copy;
    tsan;
    no_tsan;
  ]
