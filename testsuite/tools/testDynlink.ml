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

(* This test verifies that a series of libraries can be loaded via Dynlink.
   Any failures will cause either an exception or a compilation error. *)
let run config env mode =
  Format.printf "\nTesting loading of libraries with %s dynlink\n"
                (if mode = Native then "native" else "bytecode");
  let test_program =
    Environment.in_test_root env (Harness.exe "test_script") in
  (* The test program is given a list of library names (e.g. unix threads) on
     the command line and uses Dynlink to load that library from a subdirectory
     with the same name in libdir *)
  let compile_test_program () =
    Out_channel.with_open_text "test_install_script.ml" (fun oc ->
      Printf.fprintf oc {|
let load_library basename =
  let lib = Dynlink.adapt_filename (basename ^ ".cma") in
  let dir = Filename.concat %S basename in
  Dynlink.loadfile (Filename.concat dir lib);
  Printf.printf "Loaded %%s\n" lib

let () =
  let () = Dynlink.allow_unsafe_modules true in
  List.iter load_library (List.tl (Array.to_list Sys.argv))
|} (Environment.libdir env)
    );
    flush stdout;
    let compiler = Environment.tool_path env mode "ocamlc" "ocamlopt" in
    let args = [
      "-I"; "+dynlink"; Harness.lib mode "dynlink"; "-linkall";
      "-o"; test_program; "test_install_script.ml"
    ] in
    let files = Harness.files_for mode "test_install_script" [test_program] in
    let compile ?(custom = false) () =
      if Sys.file_exists test_program then
        Harness.erase_file test_program;
      let args = if custom then "-custom" :: args else args in
      (* In the Renamed phase for a bytecode-only build, ocamlc will be
         ocamlc.byte and will need to be called via ocamlrun *)
      let runtime =
        mode = Bytecode && Harness.ocamlc_fails_after_rename config in
      (* In the Renamed phase, Config.standard_library will still point to the
         Original location *)
      let stdlib = true in
      let (_, output) =
        Environment.run_process ~runtime ~stdlib env compiler args in
      Environment.display_output output
    in
    compile ();
    files, compile
  in
  (* Call the test program individually for each library. ~expected_exit_code is
     part of a mechanism required for Relocatable OCaml which is at present not
     needed. *)
  let test_libraries_in_prog ?expected_exit_code env libraries =
    (* For simplicity, the test for whether libraries have C stubs is based on
       the names, rather than inspecting the library metadata *)
    let has_c_stubs library = (mode = Bytecode && library <> "dynlink") in
    let has_c_stubs = List.exists has_c_stubs libraries in
    (* In the Renamed phase, the test driver will need to be launched with
       ocamlrun, unless executables produced by the compiler are capable of
       searching for the runtime (as the Windows executable launcher does) *)
    let runtime =
      mode = Bytecode
      && expected_exit_code = None
      && not config.target_launcher_searches_for_ocamlrun
    in
    (* If the library needs C stubs to be loaded dynamically, then the runtime
       will need CAML_LD_LIBRARY_PATH set in the Renamed phase. *)
    let stubs =
      has_c_stubs
      && expected_exit_code = None
    in
    let expected_exit_code =
      match expected_exit_code with
      | Some code ->
          (* The test driver is _not_ expected to work *)
          code
      | None ->
          (* Systems configured with --disable-shared can't load bytecode
             libraries which need C stubs *)
          if (Sys.cygwin && mode = Native && List.mem "unix" libraries)
             || (not Config.supports_shared_libraries && has_c_stubs) then
            (* cf. ocaml/flexdll#146 - Cygwin's natdynlink can't load
                   unix.cmxs *)
            2
          else
            0
    in
    let exit_code, output =
      let fails = (expected_exit_code <> 0) in
      Environment.run_process ~fails ~runtime ~stubs env test_program libraries
    in
    Environment.display_output output;
    if exit_code <> expected_exit_code then
      Harness.fail_because "%s is expected to return with exit code %d"
                           test_program expected_exit_code;
  in
  let test_libraries_in_prog ?expected_exit_code env libraries =
    if mode = Native && List.mem "threads" libraries then
      (* cf. ocaml/ocaml#12250 - no threads.cmxs *)
      let threads_plugin =
        Environment.in_libdir env (Filename.concat "threads" "threads.cmxs")
      in
      if Sys.file_exists threads_plugin then
        Harness.fail_because "threads.cmxs is not expected to exist"
      else
        ()
    else
      test_libraries_in_prog ?expected_exit_code env libraries
  in
  let not_dynlink l = not (List.mem "dynlink" l) in
  let files, re_compile = compile_test_program () in
  let expected_exit_code =
    (* Bytecode executables launched using the executable header require
       caml_executable_name to know where the runtime is. As the Standard
       Library is only stored as an absolute path, this doesn't affect the
       execution of the test driver (yet). *)
    None in
  let libraries = List.filter not_dynlink config.libraries in
  let () =
    List.iter (test_libraries_in_prog ?expected_exit_code env) libraries;
    if expected_exit_code <> None then
      (* The test driver is unable to locate ocamlrun, which means that it can't
         locate ld.conf. Re-build the test driver using -custom to work around
         this. *)
      let () = re_compile ~custom:true () in
      List.iter (test_libraries_in_prog env) libraries
  in
  List.iter Harness.erase_file files
