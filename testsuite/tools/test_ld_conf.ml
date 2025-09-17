(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            David Allsopp, University of Cambridge & Tarides            *)
(*                                                                        *)
(*   Copyright 2025 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Harness.Import

(* Tests for the handling of the DLL search path. *)
type ld_conf_test = {
  description: string;
    (* Test description *)
  caml_ld_library_path: var_setting;
    (* [Set l] sets CAML_LD_LIBRARY_PATH to be the entries of [l], concatenated
       with the separator appropriate to the platform. Note that [Blank] and
       [Set []] both set CAML_LD_LIBRARY_PATH to [""] *)
  ocamllib: var_setting;
    (* [Set l] causes the entries of [l] to be written to an ld.conf in a
       directory whose location is put in OCAMLIB. [Empty] only sets OCAMLLIB to
       [""]. *)
  camllib: var_setting;
    (* As for ocamllib, but using the CAMLLIB environment variable directory.
       A different temporary directory is used from OCAMLLIB (i.e. both CAMLLIB
       and OCAMLLIB can be set). *)
  stdlib: string list;
    (* As for ocamllib and camllib, but for the ld.conf in the Standard Library
       directory (the file is erased if the list is empty). *)
  outcome: string list;
    (* The expect result from [ocamlrun -config] / [Dll.init_compile false] *)
}
and var_setting = Unset | Empty | Set of string list

(* Set of tests to run in a given environment *)
let tests _config env =
  (* Convenience function - [if_ld_conf_found outcome] returns the empty list in
     the Renamed phase. *)
  let if_ld_conf_found outcome =
    (* ocamlrun can't find ld.conf after the prefix has been renamed *)
    if Environment.is_renamed env then
      []
    else
      outcome
  in
  let base =
    {description = "";
     caml_ld_library_path = Unset; ocamllib = Unset; camllib = Unset;
     stdlib = []; outcome = []}
  in
  (* Batch 1: various interesting kinds of line, tested when read through
     CAML_LD_LIBRARY_PATH and ld.conf *)
  let tests =
    let main, main_outcome, main_outcome_cr =
      let libdir =
        if Environment.is_renamed env then
          Environment.libdir env
        else
          Config.standard_library in
      let (/) = Filename.concat in
      let data = [
        (* Root directory (both forms) preserved *)
        "/", "/", None;
        "//", "//", None;
        (* Current and Parent directory names *)
        ".", ".", None;
        "..", "..", None;
        (* Current and Parent directory names with OS-default trailing separator
           (i.e. ./ and ../ on Unix and .\ and ..\ on Windows) *)
        "." / "", "." / "", None;
        ".." / "", ".." / "", None;
        (* "stublibs" relative to the Current and Parent directory (using OS-
           default separator) *)
        "." / "stublibs", "." / "stublibs", None;
        ".." / "stublibs", ".." / "stublibs", None;
        (* Other cases - implicit and absolute entries, and entries beginning
           with the Current and Parent directory names *)
        "stublibs", "stublibs", None;
        ".stublibs", ".stublibs", None;
        "..stublibs", "..stublibs", None;
        libdir, libdir, None;
        "/lib/ocaml", "/lib/ocaml", Some "/lib/ocaml\r";
      ] in
      let fold (main, main_outcome, main_outcome_cr) (line, outcome, cr) =
        let cr = match cr with
        | Some cr -> cr
        | None ->
            (* Windows opens ld.conf in text mode, so the \r are stripped *)
            if Sys.win32 then
              outcome
            else
              outcome ^ "\r"
        in
        line::main, outcome::main_outcome, cr::main_outcome_cr
      in
      List.fold_left fold ([], [], []) (List.rev data)
    in
    let tests =
      (* Various test lines above all fed via ld.conf in the Standard Library *)
      let outcome =
        (* Known issue: Windows strips out the blank entries in the search path
           (somewhat counterintuitively!) *)
        if Sys.win32 then
          main_outcome
        else
          "." :: main_outcome
      in
      [{base with description = "Base ld.conf test";
                  stdlib = "" :: main;
                  outcome = if_ld_conf_found outcome}] in
    let tests =
      (* As first, but with the same entries in CAML_LD_LIBRARY_PATH too *)
      let stdlib =
        if Sys.win32 then
          (* Known issue: Windows ignores empty entries in the search path, and
             it's slightly easier to test this only once in this test *)
          main
        else
          "" :: main
      in
      (* Part of the outcome from ld.conf *)
      let outcome_ld_conf =
        if Sys.win32 then
          main_outcome
        else
          "." :: main_outcome
      in
      (* Part of the outcome from CAML_LD_LIBRARY_PATH *)
      let outcome_caml_ld_library_path =
        if Sys.win32 then
          (* No blank entry at the start: Windows returns the same entries *)
          main
        else
          (* Unix displays "." for the blank, but otherwise returns the same
             entries *)
          "." :: main
      in
      {base with description = "Base ld.conf + CAML_LD_LIBRARY_PATH";
                 caml_ld_library_path = Set stdlib;
                 stdlib;
                 outcome = outcome_caml_ld_library_path
                             @ if_ld_conf_found outcome_ld_conf} :: tests in
    let tests =
      (* As first, but with entries in CAML_LD_LIBRARY_PATH including quotes and
         separators. No effect on Unix, as the colon separator is always
         expressly prohibited in PATH-like environment variables, but the semi-
         colon separator in Windows PATH-like environment variables is permitted
         and quoting rules are actively used on Windows systems. *)
      let caml_ld_library_path, outcome_caml_ld_library_path =
        let entries = [
          (* Quote characters should be stripped (it's a common misconception on
             Windows systems, but space characters do not require quoting in
             PATH-like variables, but often are.
             Result should be: quoted *)
          {|"quoted"|}, [{|"quoted"|}];
          (* Quote characters should be stripped internally too.
             Result should be: quoteinentry *)
          {|quote"in"entry|}, [{|quote"in"entry|}];
          (* Quote characters should protect separators.
             Result should be: one;entry *)
          {|one";"entry|}, [{|one"|}; {|"entry|}];
          (* The final quote character is optional.
             Result should be: one;two;three *)
          {|one";"two";three|}, [{|one"|}; {|"two"|}; "three"];
        ] in
        let test, windows_outcome =
          List.split entries
        in
        if Sys.win32 then
          test, List.flatten windows_outcome
        else
          test, test
      in
      {base with description = "Base ld.conf + quoted CAML_LD_LIBRARY_PATH";
                 caml_ld_library_path = Set caml_ld_library_path;
                 stdlib = main;
                 outcome = outcome_caml_ld_library_path
                             @ if_ld_conf_found main_outcome} :: tests in
    let tests =
      (* As first, but with a CR at the end of each line *)
      let outcome =
        (* Windows opens ld.conf in text mode, so the line with just \r is
           read as an empty string and consequently stripped *)
        if Sys.win32 then
          main_outcome_cr
        else
          "\r" :: main_outcome_cr
      in
      {base with description = "Base ld.conf with CRLF endings";
                 stdlib = List.map (Fun.flip (^) "\r") ("" :: main);
                 outcome = if_ld_conf_found outcome} :: tests in
    tests
  in
  (* Batch 2: effects of empty (vs unset) environment variables *)
  let tests =
    let tests =
      (* Empty CAML_LD_LIBRARY_PATH should add "." to the start of the search
         path *)
      let outcome_caml_ld_library_path =
        if Sys.win32 then
          []
        else
          ["."]
      in
      {base with description = "Empty CAML_LD_LIBRARY_PATH";
                 caml_ld_library_path = Empty;
                 stdlib = ["ld.conf"];
                 outcome = outcome_caml_ld_library_path
                             @ if_ld_conf_found ["ld.conf"]} :: tests in
    let tests =
      (* Embedded empty entries in CAML_LD_LIBRARY_PATH should add equivalent
         "." entries to the search path *)
      let outcome_caml_ld_library_path =
        if Sys.win32 then
          []
        else
          ["."; "."]
      in
      {base with description = "Embedded empty entry in CAML_LD_LIBRARY_PATH";
            caml_ld_library_path = Set [""; ""];
            stdlib = ["ld.conf"];
            outcome = outcome_caml_ld_library_path
                        @ if_ld_conf_found ["ld.conf"]} :: tests in
    let tests =
      (* An empty CAMLLIB should cause ld.conf in the Standard Library to be
         ignored, but not CAML_LD_LIBRARY PATH *)
      {base with description = "Empty CAMLLIB";
                 caml_ld_library_path = Set ["env"];
                 camllib = Empty;
                 stdlib = ["masked-stdlib"];
                 outcome = ["env"]} :: tests in
    let tests =
      (* An empty OCAMLLIB should cause ld.conf in both the Standard Library and
         CAMLLIB to be ignored, but not CAML_LD_LIBRARY_PATH *)
      {description = "Empty OCAMLLIB";
       caml_ld_library_path = Set ["env"];
       ocamllib = Empty;
       camllib = Set ["masked-camllib"];
       stdlib = ["masked-stdlib"];
       outcome = ["env"]} :: tests in
    tests
  in
  (* Batch 3: load priority, embedded NUL characters, EOL-at-EOF, etc. *)
  let tests =
    let tests =
      (* OCAMLLIB should have priority over CAMLLIB and the Standard Library *)
      {description = "$OCAMLLIB/ld.conf";
       caml_ld_library_path = Set ["env"];
       ocamllib = Set ["ocamllib\000"; "hidden"];
       camllib = Set ["camllib\000"; "hidden"];
       stdlib = ["libdir"];
       outcome = ["env"; "ocamllib"]} :: tests in
    let tests =
      (* CAMLLIB should have priority over the Standard Library *)
      {base with description = "$CAMLLIB/ld.conf";
                 caml_ld_library_path = Set ["env"];
                 camllib = Set ["camllib\000"; "hidden"];
                 stdlib = ["libdir"];
                 outcome = ["env"; "camllib"]} :: tests in
    let tests =
      (* EOL-at-EOF should not add a blank entry to the search path *)
      {base with description = "EOF-at-EOF";
            stdlib = (if Sys.win32 then ["libdir\r\n"] else ["libdir\n"]);
            outcome = if_ld_conf_found ["libdir"]} :: tests in
    tests
  in
  tests

(* [compile_ld_conf_test_programs config env] produces a program intended to
   print out the contents of [Dll.init_compile false; Dll.search_path ()].
   Because of the various differences between ocamlrun and ocamlc's
   implementations of ld.conf handling, and because of the vagaries of various
   platforms, in order to "simplify" the testing, the output has various
   corrections applied (note that if the underlying behaviours were harmonised,
   then these corrections will correctly cause the tests to fail - i.e. the
   "corrections" being applied really are testing the behaviours).
   The test program is returned as a function which takes a partially-applied
   version of Environment.run_process_with_test_env which has only the ~runtime,
   program and args parameters remaining along with a test record and which runs
   the test program using that function. The function returns a the
   possibly-corrected output of the program with the driver's description as the
   first line.
   When native compilation is available, the test program is compiled with both
   ocamlc.byte and ocamlc.opt. [compile_ld_conf_test_programs] returns a pair
   where [fst] is the list of functions (either one or two elements) for running
   the tests and [snd] is the list of files which need deleting when the test
   programs are finished with. *)
let compile_ld_conf_test_programs config env =
  let write_ld_conf_test_driver () =
    Out_channel.with_open_text "test_install_script.ml" (fun oc ->
      output_string oc {|
(* Known issue: Sys.getenv processes blank environment variables differently
   from _wgetenv. We therefore do not expect to observe the empty values for
   CAMLLIB or OCAMLLIB. *)
let () =
  if Sys.win32 then
    assert (Sys.getenv_opt "CAMLLIB" <> Some ""
            && Sys.getenv_opt "OCAMLLIB" <> Some "")

let () =
  let print s =
    (* Known issue: ocamlrun -config suppresses blank lines on Windows, but
       displays them as "." on other platforms. Do a similar transformation
       here, but suppress the lines entirely on Windows. *)
    if s <> "" then
      print_endline s
    else if not Sys.win32 then
      print_endline "."
  in
  Dll.init_compile false;
  List.iter print (Dll.search_path ())
|})
  in
  let compile_test_program mode files test_program description =
    (* The test driver simply calls Dll.init_compile to trigger the processing
       and then prints the resulting search path to standard output. *)
    let test_program =
      Environment.in_test_root env (Harness.exe test_program) in
    let compiler = Environment.tool_path env mode "ocamlc" "ocamlopt" in
    let args = [
      "-I"; "+compiler-libs";
      Harness.lib mode "ocamlcommon"; Harness.lib mode "ocamlbytecomp";
      "-o"; test_program; "test_install_script.ml"
    ] in
    (* For bytecode-only installations, ocamlc will be ocamlc.byte and so need
       to be invoked via ocamlrun in the Renamed phase *)
    let runtime =
      mode = Bytecode && Harness.ocamlc_fails_after_rename config in
    (* In the Renamed phase, Config.standard_library will still point to the
       Original location *)
    let stdlib = true in
    let (_, output) =
      Environment.run_process ~runtime ~stdlib env compiler args in
    Environment.display_output output;
    let files = test_program :: files in
    let files =
      (* The bytecode version is always built; add the native files if it's
         being built *)
      if mode = Native then
        Harness.files_for ~source_and_cmi:false mode "test_install_script" files
      else
        files
    in
    (* In the Renamed phase, the test driver will need to be launched with
       ocamlrun, unless executables produced by the compiler are capable of
       searching for the runtime (as the Windows executable launcher does) *)
    let runtime =
      mode = Bytecode
      && not config.target_launcher_searches_for_ocamlrun in
    let run run_process test =
      let code, lines =
        run_process ~runtime test_program []
      in
      if code = 0 then
        let lines =
          (* Known issue: Sys.getenv processes blank environment variables
             differently from _wgetenv which in the tests will cause it load
             ld.conf files. The tests have been written to allow for this by
             having the lines which are _not_ expected to appear on Unix be
             prefixed with "masked-". *)
          if Sys.win32 then
            if ((test.camllib = Empty
                   && not (Environment.is_renamed env))
                || test.ocamllib = Empty) then
              let unmask s = not (String.starts_with ~prefix:"masked-" s) in
              let lines' = List.filter unmask lines in
              (* If Windows behaviour has been harmonised, then the filtered
                 list of lines would be the same as the unfiltered list. If this
                 happens, insert an extra line to "poison" the test output to
                 prevent this behaviour from being silently fixed. *)
              if lines = lines' then
                "poisoned"::lines
              else
                lines'
            else
              lines
          else
            lines
        in
        let lines =
          (* Known issue: ocamlc opens ld.conf in text mode on Cygwin but
             ocamlrun opens it in binary mode (the default). This means that
             ocamlrun will return lines ending with \r, but ocamlc will both
             strip the \r and ignore a line consisting of just \r (because that
             appears blank in text mode). This is mitigated by ensuring that the
             \r line is always first in the test, and then adding back the \r to
             the output on Cygwin. This will clearly fail if the behaviour of
             ocamlrun and ocamlc is harmonised. *)
          match test.stdlib with
          | "\r" :: _ when Sys.cygwin && lines <> [] ->
              "\r" :: List.map (Fun.flip (^) "\r") (List.tl lines)
          | _ ->
              lines
        in
        let lines =
          (* Known issue: Misc.split_path_contents ignores empty strings where
             caml_decompose_path does not. Mitigate it by detecting the
             environment setting and simulating the line. *)
          if test.caml_ld_library_path = Set []
             || test.caml_ld_library_path = Empty then
            "." :: lines
          else
            lines
        in
        (* Known issue: Windows strips out the blank entries in the search path
           (somewhat counterintuitively!) *)
        let lines =
          if not Sys.win32 then
            lines
          else
            List.drop_while (String.equal ".") lines
        in
        let lines =
          (* Known issue: Dll.ld_conf_contents preserves NUL characters in lines
             where caml_parse_ld_conf terminates processing. This is mitigated
             in the test by putting a single line "hidden" after the line with
             an embedded NUL. *)
          let includes_nulls =
            let includes_nulls = function
            | Unset | Empty -> false
            | Set l -> List.exists (Fun.flip String.contains '\000') l
            in
            includes_nulls test.ocamllib || includes_nulls test.camllib
          in
          if includes_nulls then
            let strip_null s =
              match String.index s '\000' with
              | index ->
                  String.sub s 0 index
              | exception Not_found ->
                  s
            in
            let lines' = List.map strip_null lines in
            if lines <> lines' then
              List.filter ((<>) "hidden") lines'
            else
              (* As with empty environment variables above, if this behaviour
                 appears to have been fixed, poison the output of the test so
                 that doesn't happen silently. *)
              "poisoned" :: lines
          else
            lines
        in
        description :: lines
      else
        Harness.fail_because "%s is expected to exit with code 0"
                             test_program
    in
    run, files
  in
  let files = Harness.files_for Bytecode "test_install_script" [] in
  let () = write_ld_conf_test_driver () in
  let byte, files =
    compile_test_program Bytecode files "test_ld_conf.byte" "ocamlc.byte"
  in
  if config.has_ocamlopt then
    let opt, files =
      compile_test_program Native files "test_ld_conf.opt" "ocamlc.opt"
    in
    [byte; opt], files
  else
    [byte], files

let remove_if_exists file =
  if Sys.file_exists file then
    Sys.remove file

(* Produces a function with the same signature as those returned by
   [compile_ld_conf_test_programs] but calling [ocamlrun -config] instead. The
   output is returned in the same format. *)
let ocamlrun_config env run_process _test =
  let ocamlrun = Environment.ocamlrun env in
  let code, lines =
    run_process ~runtime:false ocamlrun ["-config"] in
  if code = 0 then
    let strip s =
      let len = String.length s in
      if len < 2 || s.[0] <> ' ' || s.[1] <> ' ' then
        Harness.fail_because "Unexpected output from ocamlrun -config: %S" s
      else
        String.sub s 2 (len - 2)
    in
    let lines =
      List.rev lines
      |> List.take_while ((<>) "shared_libs_path:")
      |> List.rev_map strip
    in
    "ocamlrun -config" :: lines
  else
    Harness.fail_because "Unexpected exit code %d from ocamlrun -config" code

(* Formats a string list list as a table *)
let display_results =
  let pad_column l =
    let max =
      List.fold_left (fun a s -> Int.max a (String.length s)) 0 l
    in
    let f s = s ^ String.make (max - String.length s) ' ' ^ " | " in
    List.map f l
  in
  fun pp_path columns ->
    assert (columns <> []);
    let columns =
      let format_string s =
        let s = Format.asprintf "%a" pp_path s in
        let s = Printf.sprintf "%S" s in
        String.sub s 1 (String.length s - 2)
      in
      List.map (fun column -> List.map format_string column) columns
    in
    let[@ocaml.warning "-8"] right :: rest = List.rev columns in
    let rec display rev_columns =
      let (row, _, finished), rev_columns =
        let f (row, rightmost, finished) = function
        | [] ->
            assert false
        | hd::tl ->
            let next =
              if tl = [] then
                if rightmost then
                  [""]
                else
                  [String.make (String.length hd - 2) ' ' ^ "| "]
              else
                tl
            in
            (hd::row, false, finished && tl = []), next
        in
        List.fold_left_map f ([], true, true) rev_columns
      in
      Environment.display_output [String.concat "" row];
      if not finished then
        display rev_columns
    in
    display (right :: List.map pad_column rest)

(* Run a single test, using scratch directories [~ocamllib_dir] and
   [~camllib_dir]. *)
let run_test ~ocamllib_dir ~camllib_dir env programs test =
  let libdir_ld_conf = Environment.in_libdir env "ld.conf" in
  Printf.printf "- %s\n" test.description;
  (* The main ld.conf is backed up at the start of the test - set its content
     (or delete it) *)
  let () =
    if test.stdlib = [] then
      remove_if_exists libdir_ld_conf
    else
      Out_channel.with_open_bin libdir_ld_conf (fun oc ->
        output_string oc (String.concat "\n" test.stdlib))
  in
  (* [process_env dir setting] creates ld.conf in [dir] if [setting] requires
     one *)
  let process_env dir setting =
    let ld_conf = Filename.concat dir "ld.conf" in
    match setting with
    | Set dirs ->
        if dirs = [] && Sys.file_exists ld_conf then
          Sys.remove ld_conf
        else
          Out_channel.with_open_bin ld_conf (fun oc ->
            output_string oc (String.concat "\n" dirs));
        Some dir
    | Empty ->
        Some ""
    | Unset ->
        None
  in
  (* Set-up the environment variables *)
  let caml_ld_library_path =
    match test.caml_ld_library_path with
    | Unset -> None
    | Empty -> Some []
    | Set l -> Some l
  in
  let ocamllib = process_env ocamllib_dir test.ocamllib in
  let camllib = process_env camllib_dir test.camllib in
  let run_process ~runtime program args =
    Environment.run_process_with_test_env
      ~runtime ~caml_ld_library_path ~ocamllib ~camllib env program args
  in
  (* Now run the test for all the supplied programs *)
  match List.map (fun f -> f run_process test) programs with
  | [] -> assert false
  | (ocamlrun::rest) as results ->
      let pp_path = Environment.pp_path env in
      (* First check that each program returned the same output *)
      if List.exists (fun r -> List.tl ocamlrun <> List.tl r) rest then begin
        display_results pp_path results;
        Harness.fail_because "All mechanisms should produce the same output"
      (* Then check that the output was as expected *)
      end else if List.tl ocamlrun <> test.outcome then begin
        display_results pp_path [ocamlrun; "Expected outcome"::test.outcome];
        Harness.fail_because "Output differs from the expected results"
      (* If called with --verbose, display the output anyway *)
      end else if Environment.verbose env then
        display_results pp_path (("Expected outcome"::test.outcome)::results)

(* This test tests the processing of ld.conf by ocamlrun (which processes it in
   order to load stub libraries referenced by a bytecode image's DLLS section)
   and ocamlc (which processes it in order to determine the primitives made
   available by stub libraries referenced by .cma files). The test ensures that
   both implementations are producing the same results. *)
let run config env =
  let pp_path = Environment.pp_path env in
  print_endline "\nTesting processing of ld.conf";
  (* ld.conf is picked up from $OCAMLLIB, $CAMLLIB or from the pre-configured
     default location of the standard library (this is why the test can only be
     performed in-prefix). During the test, temporary directories are created to
     be used for $OCAMLLIB and $CAMLLIB to point to if needed which can then
     have temporary ld.conf files placed in them. The ld.conf in libdir is
     backed up and restored after the test. *)
  let programs, files = compile_ld_conf_test_programs config env in
  (* ocamlrun must be first in the list *)
  let programs = ocamlrun_config env :: programs in
  let backed_up_ld_conf = Environment.in_libdir env "ld.conf.bak" in
  let libdir_ld_conf = Environment.in_libdir env "ld.conf" in
  let ocamllib_dir = Environment.in_test_root env "ocamllib" in
  let camllib_dir = Environment.in_test_root env "camllib" in
  let ensure_dir dir =
    if not (Sys.file_exists dir) then
      Sys.mkdir dir 0o775
    else if not (Sys.is_directory dir) then begin
      Sys.rmdir dir;
      Sys.mkdir dir 0o775
    end
  in
  let restore =
    let restored = ref false in
    fun () ->
      if not !restored then begin
        restored := true;
        Format.printf "Restoring %a to %a\n" pp_path backed_up_ld_conf
                                             pp_path libdir_ld_conf;
        remove_if_exists libdir_ld_conf;
        Sys.rename backed_up_ld_conf libdir_ld_conf
      end
  in
  (* Create the scratch directories and backup ld.conf *)
  ensure_dir ocamllib_dir;
  ensure_dir camllib_dir;
  Format.printf "Backing up %a to %a\n" pp_path libdir_ld_conf
                                        pp_path backed_up_ld_conf;
  Sys.rename libdir_ld_conf backed_up_ld_conf;
  (* Ensure ld.conf is restored even if a test fails *)
  at_exit restore;
  (* Run the tests *)
  List.iter (run_test ~ocamllib_dir ~camllib_dir env programs)
            (List.rev (tests config env));
  (* Clean-up the scratch directories *)
  remove_if_exists (Filename.concat ocamllib_dir "ld.conf");
  remove_if_exists (Filename.concat camllib_dir "ld.conf");
  Sys.rmdir ocamllib_dir;
  Sys.rmdir camllib_dir;
  restore ();
  List.iter Harness.erase_file files
