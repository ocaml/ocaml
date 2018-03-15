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

(* Definition of variables used by built-in actions *)

(* The variables are listed in alphabetical order *)

(*
  The name of the identifier representing a variable and its string name
  should be similar. Is there a way to enforce this?
*)

open Variables (* Should not be necessary with a ppx *)

let arguments = make ("arguments",
  "Arguments passed to executed programs and scripts")

let exit_status = make ("exit_status",
  "Expected program exit status")

let files = make ("files",
  "Files used by the tests")

let ocamltest_response = make ("ocamltest_response",
  "File used by hooks to send back information.")

let ocamltest_log = make ("ocamltest_log",
  "Path to log file for the current test")

let output = make ("output",
  "Where the output of executing the program is saved")

let program = make ("program",
  "Name of program produced by ocamlc.byte and ocamlopt.byte")
let program2 = make ("program2",
  "Name of program produced by ocamlc.opt and ocamlopt.opt")

let promote = make ("promote",
  "Set to \"true\" to overwrite reference files with the test output")

let reference = make ("reference",
  "Path of file to which program output should be compared")

let skip_header_lines =
  make ( "skip_header_lines",
         "The number of lines to skip when comparing program output \
          with the reference file")

let skip_header_bytes =
  make ( "skip_header_bytes",
         "The number of bytes to skip when comparing program output \
          with the reference file")

let script = make ("script",
  "External script to run")

let stdin = make ("stdin", "Default standard input")
let stdout = make ("stdout", "Default standard output")
let stderr = make ("stderr", "Default standard error")

let test_build_directory = make ("test_build_directory",
  "Directory for files produced during a test")

let test_build_directory_prefix = make ("test_build_directory_prefix",
  "Directory under which all test directories should be created")

let test_file = make ("test_file",
  "Name of file containing the specification of which tests to run")

let test_source_directory = make ("test_source_directory",
  "Directory containing the test source files")

let test_pass = make ("TEST_PASS",
  "Exit code to let a script report success")

let test_skip = make ("TEST_SKIP",
  "Exit code to let a script report skipping")

let test_fail = make ("TEST_FAIL",
  "Exit code to let a script report failure")



let _ = List.iter register_variable
  [
    arguments;
    exit_status;
    files;
    ocamltest_response;
    ocamltest_log;
    output;
    program; program2;
    reference;
    skip_header_lines;
    skip_header_bytes;
    script;
    stdin;
    stdout;
    stderr;
    test_build_directory;
    test_file;
    test_source_directory;
    test_pass;
    test_skip;
    test_fail;
  ]
