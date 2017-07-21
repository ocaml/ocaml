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

let make_cmp_tool bytes_to_ignore =
  let i_flag =
    if bytes_to_ignore <= 0
    then ""
    else ("-i " ^ (string_of_int bytes_to_ignore)) in
  {
    tool_name = "cmp";
    tool_flags = "-s " ^ i_flag;
    result_of_exitcode = cmp_result_of_exitcode
  }

let default_comparison_tool = make_cmp_tool 0

type filetype = Binary | Text

type files = {
  filetype : filetype;
  reference_filename : string;
  output_filename : string;
}

let unixify_end_of_lines filename =
  let commandline = "sed -i -e s/\\r$// " ^ filename in
  let status = Run_command.run_commandline commandline in
  (status, commandline)

let compare_files ?(tool = default_comparison_tool) files =
  let (status, commandline) =
    if files.filetype = Text && Sys.os_type="Win32"
    then unixify_end_of_lines files.output_filename
    else (0, "") in
  match status with
    | 0 ->
      let commandline = String.concat " "
      [
        tool.tool_name;
        tool.tool_flags;
        files.reference_filename;
        files.output_filename
      ] in
      let dev_null = match Sys.os_type with
        | "Win32" -> "NUL"
        | _ -> "/dev/null" in
      let settings = Run_command.settings_of_commandline
        ~stdout_fname:dev_null ~stderr_fname:dev_null commandline in
      let status = Run_command.run settings in
      tool.result_of_exitcode commandline status
    | _ -> Error (commandline, status)

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
