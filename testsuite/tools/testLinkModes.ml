(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            David Allsopp, University of Cambridge & Tarides            *)
(*                                                                        *)
(*   Copyright 2024 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Harness.Import

(* The test program is designed to exercise various properties. It is linked
   with the Unix library to exercise linking/loading C stubs. It is possible to
   generate a test program which _doesn't_ use the Unix library so that we can
   test the default link mode of ocamlc in a compiler configured with
   --disable-shared (otherwise the default link mode is the same test as
   -custom, since linking with unix.cma would force -custom).
   The test program itself then takes 6 command line arguments:
   - The first is ignored (the harness passes "skip"); the reason is that in
     several errors cases, this first argument may be incorrectly interpreted as
     the name of the bytecode image to execute, so it's set to a name which
     should not be found to trigger an error
   - The second is the expected value of Sys.executable_name
   - The third is the expected value of Sys.argv.(0)
   - The fourth is "true" if the location reported Config.standard_library by
     should exist and "false" otherwise
   - The fifth and sixth are for display output and are the values of prefix
     and libdir_suffix respectively
   The test program passes if:
   1. [Hashtbl.is_randomized ()] returns [~is_randomized]
   2. [Sys.argv.(0)] equals [Sys.argv.(2)]
   3. [Sys.executable_name] equals [Sys.argv.(3)]
   4. [Config.standard_library] existence and directoryness matches Sys.argv.(4)
   On success, the test program is silent unless [~verbose:false]. *)
let write_test_program ~verbose ~is_randomized ~with_unix description =
  let is_directory =
    if with_unix then
{|
  try (Unix.stat dir).Unix.st_kind = Unix.S_DIR
  with Unix.(Unix_error(ENOENT, _, _)) -> false
|}
    else
{|
  try Sys.is_directory dir
  with Sys_error _ -> false
|}
  in
  Out_channel.with_open_text "test_install_script.ml" @@ fun oc ->
    Printf.fprintf oc {|
let expected_executable_name = Sys.argv.(2)
let expected_argv0 = Sys.argv.(3)
let state = bool_of_string Sys.argv.(4)
let prefix = Sys.argv.(5)
let libdir_suffix = Sys.argv.(6)

let is_directory dir =%s

let display_lib =
  let dir = Config.standard_library in
  let f = function '\\' when Sys.win32 -> '/' | c -> c in
  let canonical_dir = String.map f dir in
  let dir =
    if String.starts_with ~prefix canonical_dir then
      let l = String.length prefix in
      "$prefix" ^ String.sub dir l (String.length dir - l)
    else
      dir
  in
  if String.ends_with ~suffix:libdir_suffix canonical_dir then
    let l = String.length libdir_suffix in
    String.sub dir 0 (String.length dir - l) ^ "$libdir"
  else
    dir

let () =
  let kind =
    if Filename.is_implicit Sys.executable_name then
      "implicit (" ^ Sys.executable_name ^ ")"
    else if Filename.is_relative Sys.executable_name then
      "relative (" ^ Sys.executable_name ^ ")"
    else
      "absolute"
  in
  Printf.%sfprintf stdout
    "%s: %%s\n\
     Sys.executable_name is %%s\n\
     Sys.argv.(0) = %%s\n%%!" display_lib kind Sys.argv.(0);
  let is_randomized = Hashtbl.is_randomized () in
  if %sis_randomized then begin
    Printf.eprintf "  *** Hashtbl.is_randomized () should be returning %%b\n"
                   (not is_randomized);
    exit 1
  end else if is_directory Config.standard_library <> state then begin
    Printf.eprintf "  *** Directory %%sfound!\n" (if state then "not " else "");
    exit 1
  end else if Sys.executable_name <> expected_executable_name then begin
    Printf.eprintf "  *** Sys.executable_name should be %%s but is %%s\n"
                   expected_executable_name Sys.executable_name;
    exit 1
  end else if Sys.argv.(0) <> expected_argv0 then begin
    Printf.eprintf "  *** Sys.argv.(0) should be %%s but is %%s\n"
                   expected_argv0 Sys.argv.(0);
    exit 1
  end
|} is_directory (if verbose then "" else "i") description
   (if is_randomized then "not " else "")

(* [run_program env config ~runtime ~stubs test_program expected_executable_name
    ~prefix_path_with_cwd expected_exit_code argv0 expected_argv0
    ~may_segfault ~stdlib_exists_when_renamed] executes a [test_program]
   compiled from sources generated with [write_test_program].
   [env], [~runtime], [~stubs], [~prefix_path_with_cwd], [test_program] are all
   passed unaltered to {Environment.run_process}.
   The remaining arguments are used to construct the arguments list:
   - [expected_executable_name] and [expected_argv0] are passed directly
   - The Standard Library is always expected to be found in the Original phase;
     but the value of stdlib_exists_when_renamed is used in the Renamed phase
   The program must terminate with [expected_exit_code]. [~may_segfault] is an
   escape hatch permitting exit code 139 to be silently ignored. This works
   around some problems with shared runtimes on s390x and riscv which don't
   reliably fail.
*)
let run_program env _config =
  let prefix = Environment.prefix env in
  let libdir_suffix = Environment.libdir_suffix env in
  let prefix, libdir_suffix =
    if Sys.win32 then
      let f = function '\\' -> '/' | c -> c in
      String.map f prefix, String.map f libdir_suffix
    else
      prefix, libdir_suffix
  in
  fun ~runtime ~stubs test_program expected_executable_name
      ~prefix_path_with_cwd expected_exit_code argv0 expected_argv0
      ~may_segfault ~stdlib_exists_when_renamed ->
    let stdlib_exists =
      if Environment.is_renamed env then
        stdlib_exists_when_renamed
      else
        true in
    let args = [string_of_bool stdlib_exists; prefix; libdir_suffix] in
    let argv0 =
      if argv0 = test_program then
        None
      else
        Some argv0
    in
    let args = "skip" :: expected_executable_name :: expected_argv0 :: args in
    let fails = (expected_exit_code <> 0) in
    let (exit_code, output) =
      Environment.run_process
        ~fails ~runtime ~stubs ~prefix_path_with_cwd
        env test_program ?argv0 args
    in
    Environment.display_output output;
    if exit_code <> expected_exit_code
       && (not may_segfault || exit_code <> 139) then
      Harness.fail_because
        "%s is expected to return with exit code %d"
        test_program expected_exit_code

(* Full path to the compiled object for main_in_c.c (compiled by the build
   system at the same time as the harness) *)
let main_in_c =
  let main_in_c_object = "main_in_c" ^ Config.ext_obj in
  Filename.concat (Filename.dirname Sys.executable_name) main_in_c_object

(* [link_with_main_in_c] is passed the result of one of the myriad -output-obj*
   mechanisms for both ocamlc and ocamlopt and links the object with the small
   main stub from main_in_c.c to produce the test program. If the test program
   is linked, then [ocaml_object] is erased and the function returns [true],
   otherwise it returns [false]. *)
let link_with_main_in_c env ~use_shared_runtime ~linker_exit_code mode
                        clibs ocaml_object test_program_path =
  let runtime_lib =
    let suffix = if use_shared_runtime then "_shared" else "" in
    if mode = Native then
      "-lasmrun" ^ suffix
    else
      "-lcamlrun" ^ suffix
  in
  let flags =
    let libraries =
      if mode = Native then
        [runtime_lib; Config.native_c_libraries]
      else
        [runtime_lib; Config.bytecomp_c_libraries]
    in
    clibs @ libraries
  in
  let exit_code =
    let summarise f () =
      let pp x =
        Format.pp_print_char f ' '; (Environment.pp_path env) f x in
      List.iter pp (test_program_path :: ocaml_object :: main_in_c :: flags)
    in
    Format.printf "@{<inline_code>$CC -o%a@}\n%!" summarise ();
    Ccomp.call_linker Ccomp.Exe test_program_path [ocaml_object; main_in_c]
                      (String.concat " " flags)
  in
  if exit_code <> linker_exit_code then
    Harness.fail_because
      "Linker returned with exit code %d instead of %d"
      exit_code linker_exit_code
  else if exit_code <> 0 then
    false
  else begin
    Harness.erase_file ocaml_object;
    true
  end

(* Each execution of a test program sets Sys.argv.(0) and may optionally require
   the current working directory (cwd - i.e. ".") to be added at the start of
   $PATH. *)
type execution = {
  argv0: string;
  prefix_path_with_cwd: bool;
}

(* Additionally, each execution is tagged with whether Sys.argv.(0) either
   doesn't exist or is not an OCaml program and what value it would be after
   being passed to caml_search_exe_in_path *)
type execution_properties = {
  argv0_not_ocaml: bool;
  argv0_resolved: string;
}

(* Given an executable, execution and a platform's details, an outcome describes
   what is expected to happen when running the test - a test should either fail
   with a given non-zero exit code, or return with exit code 0 having verified
   that Sys.argv.(0) and Sys.executable_name match the stated values. *)
type outcome =
| Fail of int
| Success of {executable_name: string; argv0: string}

(* Each executable is invoked with six different values of Sys.argv.(0):
     1. "test-prog";  a non-existent command
     2. "sh"; a command which will resolve in PATH
     3. "./exe-name"; a relative invocation of the executable
     4. "exe-name"; an implicit invocation where "." is not in PATH
     5. "exe-name"; an implicit invocation but with "." in PATH
     6. "/.../exe-name"; an absolute invocation of the executable
   In each instance, the executable is passed additional arguments:
     1: "skip" - this argument is designed to be an implicit filename which
        won't resolve in PATH (since some invocations with Sys.argv.(0) will
        effectively attempt to execute Sys.argv.(1))
     2: The expected value of Sys.executable_name
     3: The expected value of Sys.argv.(0)
     4. true/false depending on whether Config.standard_library should exist
     5. The prefix (used to display names as $prefix/)
     6. The libdir (used to allow $prefix/$libdir)
   The test program returns exit code 1 if:
   - Sys.executable_name doesn't equal Sys.argv.(2)
   - Sys.argv.(0) doesn't equal Sys.argv.(3)
   - Config.standard_library exists when it shouldn't (or vice versa) *)
let test_runs usr_bin_sh test_program_path test_program
              _config env ~via_ocamlrun =
  let tests =
    let test_program_relative =
      Filename.concat Filename.current_dir_name test_program
    in [
      (* Run 1 - Sys.argv.(0) is /path/to/test_program (absolute) *)
      {argv0 = test_program_path; prefix_path_with_cwd = Sys.win32},
      {argv0_not_ocaml = false; argv0_resolved = test_program_path};
      (* Run 2 - Sys.argv.(0) = "test-prog" *)
      {argv0 = "test-prog"; prefix_path_with_cwd = Sys.win32},
      {argv0_not_ocaml = true; argv0_resolved = "test-prog"};
      (* Run 3 - Sys.argv.(0) = "sh" *)
      {argv0 = "sh"; prefix_path_with_cwd = Sys.win32},
      {argv0_not_ocaml = true; argv0_resolved = usr_bin_sh};
      (* Run 4 - Sys.argv.(0) is ./test_program (relative) *)
      {argv0 = test_program_relative; prefix_path_with_cwd = Sys.win32},
      {argv0_not_ocaml = false; argv0_resolved = test_program_relative};
      (* Run 5 - Sys.argv.(0) is test_program (implicit, without PATH) *)
      {argv0 = test_program; prefix_path_with_cwd = Sys.win32},
      {argv0_not_ocaml = false; argv0_resolved = test_program};
      (* Run 6 - Sys.argv.(0) is test_program (implicit, with PATH) *)
      {argv0 = test_program; prefix_path_with_cwd = true},
      {argv0_not_ocaml = false; argv0_resolved = test_program_relative}
    ]
  in
  let test_with_outcome (({argv0; _} as test), properties) =
    let {argv0_not_ocaml; argv0_resolved} = properties in
    let outcome =
      (* If strategy has been specified, this program is going to be executed as
         ocamlrun test_program_path ... *)
      if Environment.is_renamed env && via_ocamlrun then
        Success {executable_name = test_program_path;
                 argv0 = test_program_path}
      else
        match Environment.classify_executable test_program_path with
        | Tendered {header = Header_shebang; _} ->
            (* Likewise, shebang executables, regardless of the input argv[0],
               will just see test_program_path *)
            Success {executable_name = test_program_path;
                     argv0 = test_program_path}
        | Tendered {header = Header_exe; _} ->
            if argv0_not_ocaml then
              if Sys.win32 then
                (* stdlib/headernt.c will find ocamlrun (because it effectively
                   uses caml_executable_name) but fails to hand off the bytecode
                   image, which causes ocamlrun to exit with code 127 *)
                Fail 127
              else
                (* stdlib/header.c will fail to find ocamlrun, because it never
                   uses caml_executable_name and so will either fail to find the
                   executable or will identify that it is not a bytecode
                   executable. Somewhat confusingly, it exits with code 2 *)
                Fail 2
            else if Sys.win32 then
              (* stdlib/headernt.c correctly preserves argv[0] *)
              Success {executable_name = test_program_path; argv0}
            else
              (* stdlib/header.c does not preserve argv[0] *)
              Success {executable_name = argv0_resolved;
                       argv0 = argv0_resolved}
        | Custom ->
            if Harness.no_caml_executable_name then
              if argv0_not_ocaml then
                (* -custom executables are ocamlrun, but will be unable to
                   launch the bytecode image without caml_executable_name.
                   ocamlrun exits with code 127 in this situation *)
                Fail 127
              else
                Success {executable_name = argv0_resolved; argv0}
            else
              if Sys.win32 || argv0_not_ocaml then
                (* SearchPath will resolve the relative/implicit arguments to
                   absolute paths *)
                Success {executable_name = test_program_path; argv0}
              else
                Success {executable_name = argv0_resolved; argv0}
        | Vanilla ->
            if Harness.no_caml_executable_name then
              Success {executable_name = argv0_resolved; argv0}
            else
              Success {executable_name = test_program_path; argv0}
    in
    test, outcome
  in
  List.map test_with_outcome tests

(* Each test is compiled in both the Original and Renamed phases. Additionally,
   the programs successfully compiled in the Original phase are _executed_ a
   second time in the Renamed phase. [make_test_runner] takes all the
   configuration details for a given test program and returns [`Some f] where
   [f] takes an environment and uses it to actually execute the test. The result
   of calling [f] is either [`Some g] if the test can be executed in the Renamed
   phase or `None if the test was already run in the Renamed phase or cannot be
   run in the Renamed phase for other reasons. *)
let make_test_runner ~stdlib_exists_when_renamed ~may_segfault ~with_unix
                     ~tendered ~target_launcher_searches_for_ocamlrun usr_bin_sh
                     test_program_path test_program config _env =
  (* Bytecode executables with absolute headers will need to be
     invoked via ocamlrun after the prefix has been renamed. *)
  let via_ocamlrun =
    tendered && not target_launcher_searches_for_ocamlrun
  in
  let rec run env =
    let runs =
      test_runs usr_bin_sh test_program_path test_program
                config env ~via_ocamlrun in
    let execute ({argv0; prefix_path_with_cwd}, outcome) =
      let expected_executable_name, expected_exit_code, expected_argv0 =
        match outcome with
        | Fail code -> "", code, ""
        | Success {executable_name; argv0} -> executable_name, 0, argv0
      in
      let stubs = tendered && with_unix in
      run_program
        env config ~runtime:via_ocamlrun ~stubs
        test_program_path ~prefix_path_with_cwd expected_executable_name
        expected_exit_code argv0 expected_argv0 ~may_segfault
        ~stdlib_exists_when_renamed
    in
    List.iter execute runs;
    print_newline ();
    if Environment.is_renamed env then
      (Harness.erase_file test_program_path; `None)
    else
      `Some run
  in
  `Some run

(* Describe the various ways in which executables can be produced by our two
   compilers... *)
type linkage =
| Default_ocamlc of launch_mode
| Default_ocamlopt
| Custom_runtime of runtime_mode
| Output_obj of compiler * runtime_mode
| Output_complete_obj of compiler * runtime_mode
| Output_complete_exe of runtime_mode
and compiler = C_ocamlc | C_ocamlopt
and runtime_mode = Shared | Static

(* [compile_test usr_bin_sh config env test test_program description] builds
   [test_program] to execute [test] in [env]. The compiler is invoked explicitly
   (PATH-resolution is not used). *)
let compile_test usr_bin_sh config env test test_program description =
    (* Convert a test to the required properties needed to build and run it:
       - use_shared_runtime is true if -runtime-variant _shared is needed, etc.
       - options is a list of flags to be passed to the compiler
       - main_in_c is true if the compiler is expected to be a produce an
         intermediate object file which must then be linekd with the test
         harness's own main_in_c.o
       - compilation_exit_code, linker_exit_code and may_segfault allow known
         issues with the tests to be expressed, permitting the process to fail
         at either compilation, linking or execution time.
       - tendered is true if the image searches for a runtime (i.e. the default
         mode of ocamlc)
       - clibs prepends any additional C libraries which must be passed when
         linking (implies main_in_c is true) *)
    let use_shared_runtime, mode, options, main_in_c,
        compilation_exit_code, linker_exit_code, may_segfault, tendered,
        target_launcher_searches_for_ocamlrun, clibs =
      let f ?(use_shared_runtime = false) ?(mode = Bytecode)
            ?(calls_linker = (mode = Native)) ?(compilation_exit_code = 0)
            ?(linker_exit_code = 0) ?(may_segfault = false) ?(tendered = false)
            ?(target_launcher_searches_for_ocamlrun =
                config.target_launcher_searches_for_ocamlrun) ?clibs options =
        let main_in_c = clibs <> None in
        let clibs = Option.value ~default:[] clibs in
        let compilation_exit_code, linker_exit_code =
          (* If the prefix has been renamed,
             If the linker is needed,
             If the linker is flexlink, not the C compiler,
             If the system does support native compilation,
             If the launcher does not search for ocamlrun,
             Yours is... an error, my son! *)
          if Environment.is_renamed env && calls_linker
             && Toolchain.linker_is_flexlink && not config.has_ocamlopt
             && not config.launcher_searches_for_ocamlrun then
            (* If the main program is in C, then the error will happen during
               the _explicit_ linking stage (hence we override linker_exit_code
               to 2. If the entire program is being linked by ocamlc/ocamlopt,
               then the error will happen in the compilation stage, and the
               override is to compilation_exit_code. *)
            if main_in_c then
              compilation_exit_code, 2
            else
              2, linker_exit_code
          else
            compilation_exit_code, linker_exit_code
        in
        use_shared_runtime, mode, options, main_in_c,
        compilation_exit_code, linker_exit_code, may_segfault, tendered,
        target_launcher_searches_for_ocamlrun, clibs
      in
      let fails_if ?(compilation_exit_code = 2) cond =
        if cond then
          compilation_exit_code
        else
          0
      in
      match test with
      | Default_ocamlc _launch_method ->
          f ~tendered:true []
      | Default_ocamlopt ->
          f ~mode:Native []
      | Custom_runtime Static ->
          f ~calls_linker:true ["-custom"]
      | Custom_runtime Shared ->
          (* Shared compilation isn't available on native Windows and fails on
             Cygwin *)
          let compilation_exit_code = fails_if (Sys.win32 || Sys.cygwin) in
          f ~calls_linker:true ~use_shared_runtime:true ~compilation_exit_code
            ["-custom"]
      | Output_obj(C_ocamlc, Static) ->
          f ~clibs:["-lunixbyt"] ["-output-obj"]
      | Output_obj(C_ocamlc, Shared) ->
          (* Shared compilation isn't available on native Windows and fails on
             Cygwin *)
          let linker_exit_code = fails_if (Sys.win32 || Sys.cygwin) in
          f ~use_shared_runtime:true ~clibs:["-lunixbyt"] ~linker_exit_code
            ["-output-obj"]
      | Output_obj(C_ocamlopt, Static) ->
          f ~mode:Native
            ~clibs:["-lcomprmarsh"; "-lunixnat"; Config.compression_c_libraries]
            ["-output-obj"]
      | Output_obj(C_ocamlopt, Shared) ->
          (* cf. ocaml/ocaml#13693 - on Fedora/RHEL, this executable
             segfaults *)
          let may_segfault = List.mem Config.architecture ["s390x"; "riscv"] in
          (* Shared compilation isn't available on native Windows and fails on
             Cygwin *)
          let linker_exit_code = fails_if (Sys.win32 || Sys.cygwin) in
          f ~mode:Native ~use_shared_runtime:true ~may_segfault
            ~clibs:["-lcomprmarsh"; "-lunixnat"; Config.compression_c_libraries]
            ~linker_exit_code ["-output-obj"]
      | Output_complete_obj(C_ocamlc, Static) ->
          (* At the moment, the partial linker will pass -lws2_32 and -ladvapi32
             on to the partial linker on mingw-w64 which causes a failure. Until
             this is fixed, pass the libraries manually, using -noautolink. *)
          f ~clibs:[]
            ["-output-complete-obj"; "-noautolink"; "-cclib"; "-lunixbyt"]
      | Output_complete_obj(C_ocamlc, Shared) ->
          (* The partial linker doesn't correctly process
             -runtime-variant _shared, as the .so gets passed to the partial
             linker. On macOS, this causes a warning; on other systems, it's an
             error. *)
          let compilation_exit_code = fails_if (Config.system <> "macosx") in
          (* Shared compilation isn't available on native Windows and fails on
             Cygwin *)
          let linker_exit_code = fails_if (Sys.win32 || Sys.cygwin) in
          f ~use_shared_runtime:true ~clibs:[] ~compilation_exit_code
            ~linker_exit_code ["-output-complete-obj"]
      | Output_complete_obj(C_ocamlopt, Static) ->
          let linker_exit_code =
            (* cf. ocaml/ocaml#13692 - linking fails on ppc64 *)
            if Config.architecture = "power" then
              1
            else
              0
          in
          (* At the moment, the partial linker will pass -lzstd to ld -r which
             will (normally) fail). Until this is done, pass the libraries
             manually, using -noautolink. *)
          f ~mode:Native ~clibs:[Config.compression_c_libraries]
            ~linker_exit_code
            ["-output-complete-obj"; "-noautolink"; "-cclib"; "-lunixnat";
                                                    "-cclib"; "-lcomprmarsh"]
      | Output_complete_obj(C_ocamlopt, Shared) ->
          (* ocamlopt doesn't correctly implement -runtime-variant _shared *)
          let compilation_exit_code = fails_if true in
          f ~mode:Native ~use_shared_runtime:true
            ~compilation_exit_code ~clibs:[Config.compression_c_libraries]
            ["-output-complete-obj"; "-noautolink"; "-cclib"; "-lunixnat";
                                                    "-cclib"; "-lcomprmarsh"]
      | Output_complete_exe Static ->
          f ~calls_linker:true ["-output-complete-exe"]
      | Output_complete_exe Shared ->
          (* Shared compilation isn't available on native Windows and fails on
             Cygwin *)
          let compilation_exit_code = fails_if (Sys.win32 || Sys.cygwin) in
          f ~calls_linker:true ~use_shared_runtime:true ~compilation_exit_code
            ["-output-complete-exe"]
    in
    if use_shared_runtime && not Config.supports_shared_libraries
    || mode = Native && not config.has_ocamlopt then
      (* This test cannot be compiled because OCaml has been configured without
         required support *)
      `None
    else
      let test_program_path =
        Environment.in_test_root env (Harness.exe test_program) in
      let compiler = Environment.tool_path env mode "ocamlc" "ocamlopt" in
      let output =
        if main_in_c then
          "test_install_ocaml" ^ Config.ext_obj
        else
          test_program_path
      in
      let with_unix = (Config.supports_shared_libraries || not tendered) in
      let is_randomized = false in
      let verbose = Environment.verbose env in
      write_test_program ~verbose ~is_randomized ~with_unix description;
      let options =
        if use_shared_runtime then
          "-runtime-variant" :: "_shared" :: options
        else
          options
      in
      let args =
        "-o" :: output ::
        "test_install_script.ml" :: options
      in
      let args =
        if with_unix then
          "-I" :: "+unix" :: Harness.lib mode "unix" :: args
        else
          args
      in
      let args =
        "-I" :: "+compiler-libs" :: Harness.lib mode "ocamlcommon" :: args
      in
      let args =
        if verbose then
          "-verbose" :: args
        else
          args
      in
      let exit_code =
        let exit_code, output =
          let fails = (compilation_exit_code <> 0) in
          (* For bytecode-only installations, ocamlc will be ocamlc.byte and so
             need to be invoked via ocamlrun in the Renamed phase *)
          let runtime =
            mode = Bytecode && Harness.ocamlc_fails_after_rename config in
          (* If shared libraries are being used, ocamlc will need to be able to
             load the stub libraries to check the primitives table *)
          let stubs = with_unix && tendered in
          (* In the Renamed phase, Config.standard_library will still point to
             the Original location *)
          let stdlib = true in
          Environment.run_process
            ~fails ~runtime ~stubs ~stdlib env compiler args
        in
        Environment.display_output output;
        exit_code
      in
      if exit_code <> compilation_exit_code then
        Harness.fail_because "%s is expected to return with exit code %d"
                             compiler compilation_exit_code
      else if exit_code <> 0 then
        (* Nothing to run because compilation of the test is known to fail *)
        `None
      else
        (* OCaml part of the program successfully compiled: erase the
           compilation artefacts *)
        let () =
          List.iter Harness.erase_file
            (Harness.files_for mode "test_install_script" [])
        in
        (* If the test is for -output-obj*, link the resulting object (this
           process also cleans up the OCaml object) *)
        if main_in_c
           && not (link_with_main_in_c env ~use_shared_runtime ~linker_exit_code
                                       mode clibs output test_program_path) then
          (* Nothing to run because linking the test is known to fail *)
          `None
        else
          let stdlib_exists_when_renamed =
            (* Config.standard_library is an absolute path, and therefore will
               always point to the Original location in the Renamed phase. *)
            false
          in
          make_test_runner ~stdlib_exists_when_renamed ~may_segfault ~with_unix
                           ~tendered ~target_launcher_searches_for_ocamlrun
                           usr_bin_sh test_program_path test_program config env

let compiler_where env ?runtime mode =
  let compiler = Environment.tool_path env mode "ocamlc" "ocamlopt" in
  match Environment.run_process ?runtime env compiler ["-where"] with
  | (0, [where]) -> where
  | _ ->
      Harness.fail_because "Unexpected response from %s -where" compiler

(* This test verifies both that all compilation mechanisms are working and that
   each of these programs can correctly identify the Standard Library location.
   Any failures will cause either an exception or a compilation error. *)
let run ~sh config env =
  let pp_path = Environment.pp_path env in
  Format.printf "\nTesting compilation mechanisms for %a\n%!"
                pp_path (Environment.bindir env);
  let ocamlc_where =
    let runtime = Harness.ocamlc_fails_after_rename config in
    compiler_where env ~runtime Bytecode in
  let ocamlopt_where =
    if config.has_ocamlopt then
      compiler_where env Native
    else
      "n/a"
  in
  Format.printf "ocamlc -where: %a\nocamlopt -where: %a\n%!"
                pp_path ocamlc_where pp_path ocamlopt_where;
  let compile_test = compile_test sh config env in
  let launch_method =
    if config.bytecode_shebangs_by_default then
      Header_shebang
    else
      Header_exe
  in
  let tests = [
    compile_test (Default_ocamlc launch_method)
      "byt_default" "with tender";
    compile_test (Custom_runtime Static)
      "custom_static" "-custom static runtime";
    compile_test (Custom_runtime Shared)
      "custom_shared" "-custom shared runtime";
    compile_test (Output_obj(C_ocamlc, Static))
      "byt_obj_static" "-output-obj static runtime";
    compile_test (Output_obj(C_ocamlc, Shared))
      "byt_obj_shared" "-output-obj shared runtime";
    compile_test (Output_complete_obj(C_ocamlc, Static))
      "byt_complete_obj_static" "-output-complete-obj static runtime";
    compile_test (Output_complete_obj(C_ocamlc, Shared))
      "byt_complete_obj_shared" "-output-complete-obj shared runtime";
    compile_test (Output_complete_exe Static)
      "byt_complete_exe_static" "-output-complete-exe static runtime";
    compile_test (Output_complete_exe Shared)
      "byt_complete_exe_shared" "-output-complete-exe shared runtime";
    compile_test Default_ocamlopt
      "nat_default" "static runtime";
    compile_test (Output_obj(C_ocamlopt, Static))
      "nat_obj_static" "-output-obj static runtime";
    compile_test (Output_obj(C_ocamlopt, Shared))
      "nat_obj_shared" "-output-obj shared runtime";
    compile_test (Output_complete_obj(C_ocamlopt, Static))
      "nat_complete_obj_static" "-output-complete-obj static runtime";
    compile_test (Output_complete_obj(C_ocamlopt, Shared))
      "nat_complete_obj_shared" "-output-complete-obj shared runtime";
  ] in
  Printf.printf "Running programs\n%!";
  List.map (function `Some f -> f env | `None -> `None) tests
