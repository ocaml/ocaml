(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris            *) 
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* File comparison tools *)

type result =
  | Same
  | Different
  | Unexpected_output
  | Error of string * int

type tool = {
  tool_name : string;
  tool_flags : string;
  result_of_exitcode : string -> int -> result
}

let cmp_result_of_exitcode commandline = function
  | 0 -> Same
  | 1 -> Different
  | exit_code -> (Error (commandline, exit_code))

let default_comparison_tool = {
  tool_name = "cmp";
  tool_flags = "-s";
  result_of_exitcode = cmp_result_of_exitcode
}

type files = {
  reference_filename : string;
  output_filename : string;
}

let compare_files ?(tool = default_comparison_tool) files =
  let command = String.concat " "
  [
    tool.tool_name;
    tool.tool_flags;
    files.reference_filename;
    files.output_filename
  ] in
  tool.result_of_exitcode command (Sys.command command)

let check_file ?(tool = default_comparison_tool) files =
  if Sys.file_exists files.reference_filename
  then compare_files ~tool:tool files
  else begin
    if Testlib.file_is_empty files.output_filename
    then Same
    else Unexpected_output
  end

let diff files =
  let temporary_file = Filename.temp_file "ocamltest" "diff" in
  let diff_commandline = String.concat " "
  [
    "diff -u";
    files.reference_filename;
    files.output_filename;
    "> " ^ temporary_file
  ] in
  if (Sys.command diff_commandline) = 2 then Pervasives.Error "diff"
  else Ok (Testlib.string_of_file temporary_file)
