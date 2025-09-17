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

(* Test for executable bit on files *)
let is_executable =
  if Sys.win32 then
    Fun.const true
  else
    fun binary ->
      try Unix.access binary [Unix.X_OK]; true
      with Unix.Unix_error _ -> false

(* Look for all executables in $bindir/flexlink* and $bindir/ocaml*. All the
   distribution binaries support the -vnum flag, so it's used as a check that
   the launchers are operating correctly. Some additional testing is done on
   Windows checking the behaviour of running foo versus foo.exe *)
let run config env =
  let bindir = Environment.bindir env in
  Format.printf "\nTesting bytecode binaries in %a\n"
                (Environment.pp_path env) bindir;
  let ocamlrun = Environment.ocamlrun env in
  let exec_magic =
    Environment.run_process env ocamlrun ["-M"]
  in
  let test_binary binary =
    if String.starts_with ~prefix:"ocaml" binary
    || String.starts_with ~prefix:"flexlink" binary then
    let program = Filename.concat bindir binary in
    if is_executable program then
      let classification = Environment.classify_executable program in
      if classification <> Vanilla then
        let fails =
          (* After the prefix has been renamed, bytecode executables compiled
             with -custom will still work. Otherwise, only executables where the
             header can search for ocamlrun and which do not require any C stubs
             to be loaded will still work. *)
          Environment.is_renamed env
          && match classification with
             | Tendered {dlls; _} ->
                 not config.launcher_searches_for_ocamlrun || dlls
             | _ ->
                 false
        in
        match Environment.run_process ~fails env program ["-vnum"] with
        | (0, ((output::rest) as all_output)) when not fails ->
            if rest <> [] then begin
              Environment.display_output all_output;
              Harness.fail_because "%s: expected only one line of output"
                                   program
            end;
            let runtime =
              let compiled_by_boot_ocamlc =
                let name =
                  if Filename.extension binary = ".exe" then
                    Filename.remove_extension binary
                  else
                    binary
                in
                name <> "ocamldoc" && name <> "ocamldebug"
              in
              match classification with
              | Vanilla -> assert false
              | Custom ->
                  if Config.supports_shared_libraries
                     || compiled_by_boot_ocamlc then
                    Harness.fail_because "%s: unexpected -custom runtime"
                                         program
                  else
                    "compiled with -custom"
              | Tendered {runtime; header; _} ->
                  let is_expected_runtime =
                    if Sys.win32 then
                      runtime = "ocamlrun"
                    else
                      runtime = ocamlrun
                  in
                  let expected_launch_mode =
                    if Config.shebangscripts then
                      Header_shebang
                    else
                      Header_exe
                  in
                  if is_expected_runtime then
                    if header = expected_launch_mode then
                      runtime
                    else
                      Harness.fail_because "%s: unexpected launch mode" program
                  else
                    Harness.fail_because "%s: unexpected runtime %S"
                                         program runtime
            in
            Printf.printf "  Runtime: %s\n  Output: %s\n" runtime output;
            if Sys.win32 && Filename.extension binary = ".exe" then
              (* This additional part of the test ensures that the executable
                 launcher on Windows can correctly hand-over to ocamlrun on
                 Windows. The check is that a binary named ocamlc.byte.exe
                 can be invoked as ocamlc.byte. -M is used as a previous bug
                 caused ocamlc.byte to act solely as ocamlrun, the test being
                 that ocamlrun -M returning the runtime's magic number would
                 be likely distinct from the behaviour of any of the
                 distribution's tools when called with -M. *)
              let without_exe = Filename.remove_extension binary in
              let (this_exit_code, _) as this =
                let fails = not (String.contains without_exe '.') in
                Environment.run_process
                  ~fails env program ~argv0:without_exe ["-M"]
              in
              if this_exit_code = 0 then
                if this = exec_magic then
                  let (that_exit_code, _) as that =
                    Environment.run_process
                      ~fails:true env program ~argv0:binary ["-M"]
                  in
                  if this = that then
                    Harness.fail_because
                      "Neither %s nor %s seem to load the bytecode image"
                      without_exe binary
                  else if that_exit_code = 0 then
                    Harness.fail_because
                      "%s is not expected to return with exit code 0"
                      binary
                  else if not (String.contains without_exe '.') then
                    Harness.fail_because
                      "%s is not expected to return the exec magic number!"
                      without_exe
                  else () (* Expected outcome was the exec magic number *)
                else () (* Expected outcome is a zero exit code *)
              else () (* Expected outcome is a non-zero exit code *)
        | _ ->
            if not fails then
              Harness.fail_because "%s: not expected to have failed" program
  in
  let binaries = Sys.readdir bindir in
  Array.sort String.compare binaries;
  Array.iter test_binary binaries
