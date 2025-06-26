(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        David Allsopp, Tarides                          *)
(*                                                                        *)
(*   Copyright 2024 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Harness.Import

let print_summary config header_size ~prefix ~bindir_suffix ~libdir_suffix
                         ~relocatable ~target_relocatable ~reproducible =
  let summary =
    let choose b t f = (if b then t else f), true in
    let puzzle = [
      "native and ", config.has_ocamlopt;
      "bytecode", true;
      " only", not config.has_ocamlopt;
      " for ", true;
      choose Config.supports_shared_libraries
             "shared and static linking"
             "static linking only";
      " with ocamlnat", config.has_ocamlnat
    ] in
    let summary =
      List.filter_map (fun (s, b) -> if b then Some s else None) puzzle
    in
    String.concat "" summary
  in
  let pp_relocatable f b =
    Format.fprintf f "@{<%s>%srelocatable@}"
      (if b then "hint" else "warning")
      (if b then "" else "not ")
  in
  let pp_reproducible f b =
    if b then
      Format.fprintf f " and @{<hint>reproducible@}"
  in
  Format.printf
    "@{<loc>Test Environment@}\n\
    \    @{<hint>prefix@} = %s\n\
    \    @{<hint>bindir@} = [$prefix/]%s\n\
    \    @{<hint>libdir@} = [$prefix/]%s\n\
    \  - C compiler is %s [%s] for %s\n\
    \  - OCaml is %a%a; target binaries by default are %a\n\
    \  - Executable header size is %.2fKiB (%d bytes)\n\
    \  - Testing %s\n@?"
       prefix bindir_suffix libdir_suffix
       Config.c_compiler Config.c_compiler_vendor Config.target
       pp_relocatable relocatable pp_reproducible reproducible
       pp_relocatable target_relocatable
       (float_of_int header_size /. 1024.0) header_size summary

let run_tests ~sh config env =
  TestDynlink.run config env Bytecode;
  if config.has_ocamlopt && Config.supports_shared_libraries then
    TestDynlink.run config env Native;
  TestToplevel.run config env Bytecode;
  if config.has_ocamlnat then
    TestToplevel.run config env Native;
  Test_ld_conf.run config env;
  TestBytecodeBinaries.run config env;
  TestLinkModes.run ~sh config env

let () =
  let ~config, ~pwd, ~prefix, ~bindir:_, ~bindir_suffix, ~libdir,
      ~libdir_suffix, ~summarise_only, ~verbose =
    match Cmdline.parse Sys.argv with
    | Result.Error (code, msg) ->
        prerr_string msg;
        exit code
    | Result.Ok result ->
        result
  in
  (* The build directory may contain symlinks, and if this is so then the
     reproducibility test must search for both the logical (symlinks not
     resolved) and physical forms. This is particularly relevant on FreeBSD,
     where /home is a symlink to /usr/home and matters because OCaml's debugging
     information writes the physical directory where GCC/clang writes the
     logical directory. The logical version of the current working directory
     would normally just be [Sys.getenv "PWD"] but that can't be relied on
     coming from GNU make, because the invocation of the harness is passed
     through [sh -c] which correctly resets PWD to getcwd() (which is the
     physical version). The logical cwd is therefore passed using the --pwd
     argument from the Makefile. *)
  let test_root, test_root_logical =
    let cwd = Sys.getcwd () in
    (* --pwd is ignored on Windows, since Sys.getcwd is automatically the
       logical CWD. *)
    if Sys.win32 then
      cwd, Unix.realpath cwd
    else
      pwd, cwd
  in
  let test_root_logical =
    if test_root_logical = test_root then
      None
    else
      Some test_root_logical
  in
  (* Finalise the config record *)
  let libraries = List.sort Stdlib.compare config.libraries in
  (* Augment the list of libraries with their dependencies. This is done by hand
     given that there's only one to worry about... *)
  let libraries =
    let add_dependencies = function
    | ["systhreads"] -> ["unix"; "threads"]
    | x -> x
    in
    List.map add_dependencies libraries
  in
  let runtime_launch_info =
    let file = Filename.concat libdir "runtime-launch-info" in
    Bytelink.read_runtime_launch_info file in
  let header_size =
    let {Bytelink.buffer; executable_offset; _} = runtime_launch_info in
    String.length buffer - executable_offset in
  let bytecode_shebangs_by_default =
    runtime_launch_info.launcher <> Bytelink.Executable in
  let launcher_searches_for_ocamlrun = Sys.win32 in
  let target_launcher_searches_for_ocamlrun = Sys.win32 in
  let config =
    {config with libraries;
                 launcher_searches_for_ocamlrun;
                 target_launcher_searches_for_ocamlrun;
                 bytecode_shebangs_by_default}
  in
  (* A compiler distribution is _Relocatable_ if its build, for a given system,
     satisfies the following three properties:

     1. The binaries produced are identical regardless of the installation
        prefix or the working directory in which the compiler was built.
     2. The resulting compiler distribution can be used from any disk location
        on the system without any further alteration to the binaries.
     3. The resulting compiler distribution can be used from any disk location
        on the system without any further alteration to the user's shell
        environment.

     For the compiler's files to be reproducible, the compiler needs to be both
     relocatable and also required support from the assembler and C compiler. *)
  let relocatable = false in
  let reproducible =
    relocatable
    (* At present, the compiler build doesn't actually take advantage of this
       configuration, but this does not matter because the compiler cannot yet
       be relocatable! *)
    && (not config.has_ocamlopt
        || not Toolchain.assembler_embeds_build_path
        || Config.as_has_debug_prefix_map && Config.architecture <> "riscv")
    && not Toolchain.linker_embeds_build_path
    && (not Toolchain.c_compiler_always_embeds_build_path
        || not Toolchain.c_compiler_debug_paths_can_be_absolute)
  in
  let target_relocatable = false in
  (* Use Harness.pp_path unless --verbose was specified *)
  let pp_path =
    if verbose then
      Format.pp_print_string
    else
      Harness.pp_path ~prefix ~bindir_suffix ~libdir_suffix ~test_root in
  (* Force colour display in CI *)
  let style =
    if Sys.getenv_opt "GITHUB_ACTIONS" <> None
    || Sys.getenv_opt "APPVEYOR_BUILD_ID" <> None then
      Some Misc.Color.Always
    else
      None
  in
  Misc.Style.setup style;
  let no_markup ansi = { Misc.Style.ansi; text_close = ""; text_open = "" } in
  (* (Ab)use the tags used in Misc.Style rather than making our own *)
  Misc.Style.(set_styles {
    warning = no_markup [Bold; FG Yellow];
    error = no_markup [Bold; FG Red];
    loc = no_markup [Bold; FG Blue];
    hint = no_markup [Bold; FG Green];
    inline_code = no_markup [FG Blue]});
  print_summary config header_size
                ~prefix ~bindir_suffix ~libdir_suffix
                ~relocatable ~target_relocatable ~reproducible;
  if summarise_only then
    exit 1;
  (* Run all tests in the supplied prefix *)
  if verbose then
    Clflags.verbose := true;
  let make_env =
    Environment.make pp_path ~verbose ~test_root ~test_root_logical in
  let env = make_env ~phase:Original ~prefix ~bindir_suffix ~libdir_suffix in
  let sh =
     match Environment.run_process
             ~quiet:true env "sh" ["-c"; "command -v sh"] with
     | (0, [where]) -> where
     | _ ->
         Harness.fail_because "Unexpected response from command -v sh"
  in
  let run_tests = run_tests ~sh config in
  (* 1. Relocation test *)
  TestRelocation.run ~reproducible config env;
  (* 2. Run the main test battery in the Original phase. The result is a list
        of programs which can be run after the prefix has been renamed *)
  Compmisc.init_path ();
  let programs = run_tests env in
  (* Rename the prefix, appending .new to the directory name *)
  let new_prefix = prefix ^ ".new" in
  let libdir = Filename.concat new_prefix libdir_suffix in
  Format.printf "Renaming %a to %a\n\n%!" pp_path prefix
                                          pp_path new_prefix;
  Sys.rename prefix new_prefix;
  at_exit (fun () ->
    flush stderr;
    flush stdout;
    Format.printf "Restoring %a to %a\n" pp_path new_prefix
                                         pp_path prefix;
    Sys.rename new_prefix prefix);
  let env =
    make_env ~phase:Renamed ~prefix:new_prefix ~bindir_suffix ~libdir_suffix in
  (* 3. Re-run the test programs compiled with the normal prefix *)
  Printf.printf "Re-running test programs\n%!";
  List.iter
    (function `Some f -> assert (f env = `None) | `None -> ()) programs;
  (* 4. Finally re-run the main test battery in the new prefix *)
  Compmisc.init_path ~standard_library:libdir ();
  let programs = run_tests env in
  assert (List.for_all (function `None -> true | _ -> false) programs)
