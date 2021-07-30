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

let arguments = Variables.make ("arguments",
  "Arguments passed to executed programs and scripts")

let cwd = Variables.make ("cwd",
  "Used to change current working directory, but not updated")

let commandline = Variables.make ("commandline",
  "Specify the commandline of a tool")

let dst = Variables.make ("dst", "Location where to copy files and directories")

let exit_status = Variables.make ("exit_status",
  "Expected program exit status")

let file = Variables.make ("file",
  "File whose existence should be tested")

let readonly_files = Variables.make ("readonly_files",
  "Files which are only read by the tests")

let make = Variables.make ("MAKE",
  "Command used to invoke make")

let ocamltest_response = Variables.make ("ocamltest_response",
  "File used by hooks to send back information.")

let ocamltest_log = Variables.make ("ocamltest_log",
  "Path to log file for the current test")

let output = Variables.make ("output",
  "Where the output of executing the program is saved")

let program = Variables.make ("program",
  "Name of program produced by ocamlc.byte and ocamlopt.byte")
let program2 = Variables.make ("program2",
  "Name of program produced by ocamlc.opt and ocamlopt.opt")

let promote = Variables.make ("promote",
  "Set to \"true\" to overwrite reference files with the test output")

let reason = Variables.make ("reason",
  "Let a test report why it passed/skipped/failed.")

let reference = Variables.make ("reference",
  "Path of file to which program output should be compared")

let skip_header_lines =
  Variables.make ( "skip_header_lines",
         "The number of lines to skip when comparing program output \
          with the reference file")

let skip_header_bytes =
  Variables.make ( "skip_header_bytes",
         "The number of bytes to skip when comparing program output \
          with the reference file")

let script = Variables.make ("script",
  "External script to run")

let src = Variables.make ("src", "Files and directories to copy")

let stdin = Variables.make ("stdin", "Default standard input")
let stdout = Variables.make ("stdout", "Default standard output")
let stderr = Variables.make ("stderr", "Default standard error")

let subdirectories = Variables.make ("subdirectories",
  "Subdirectories to copy recursively from test source to test build directory")

let test_build_directory = Variables.make ("test_build_directory",
  "Directory for files produced during a test")

let test_build_directory_prefix = Variables.make ("test_build_directory_prefix",
  "Directory under which all test directories should be created")

let test_file = Variables.make ("test_file",
  "Name of file containing the specification of which tests to run")

let test_source_directory = Variables.make ("test_source_directory",
  "Directory containing the test source files")

let test_pass = Variables.make ("TEST_PASS",
  "Exit code to let a script report success")

let test_skip = Variables.make ("TEST_SKIP",
  "Exit code to let a script report skipping")

let test_fail = Variables.make ("TEST_FAIL",
  "Exit code to let a script report failure")

let timeout = Variables.make ("timeout",
  "Maximal execution time for every command (in seconds)")

let _ = List.iter Variables.register_variable
  [
    arguments;
    cwd;
    commandline;
    dst;
    exit_status;
    file;
    readonly_files;
    make;
    ocamltest_response;
    ocamltest_log;
    output;
    program; program2;
    reason;
    reference;
    src;
    skip_header_lines;
    skip_header_bytes;
    script;
    stdin;
    stdout;
    stderr;
    subdirectories;
    test_build_directory;
    test_file;
    test_source_directory;
    test_pass;
    test_skip;
    test_fail;
    timeout;
  ]
