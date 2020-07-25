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

open Ocamltest_stdlib

type result =
  | Same
  | Different
  | Unexpected_output
  | Error of string * int

type ignore = {bytes: int; lines: int}
type tool =
  |  External of {
                   tool_name : string;
                   tool_flags : string;
                   result_of_exitcode : string -> int -> result
                }
  | Internal of ignore

let cmp_result_of_exitcode commandline = function
  | 0 -> Same
  | 1 -> Different
  | exit_code -> (Error (commandline, exit_code))

let make_cmp_tool ~ignore =
  Internal ignore

let make_comparison_tool ?(result_of_exitcode = cmp_result_of_exitcode)
                         name flags =
  External
    {
      tool_name = name;
      tool_flags = flags;
      result_of_exitcode
    }

let default_comparison_tool = make_cmp_tool ~ignore:{bytes=0;lines=0}

type filetype = Binary | Text

type files = {
  filetype : filetype;
  reference_filename : string;
  output_filename : string;
}

let last_is_cr s =
  let l = String.length s in
  l > 0 && s.[l - 1] = '\r'

(* Returns last character of an input file. Fails for an empty file. *)
let last_char ic =
  seek_in ic (in_channel_length ic - 1);
  input_char ic

(* [line_seq_of_in_channel ~normalise ic first_line] constructs a sequence of
   the lines of [ic] where [first_line] is the already read first line of [ic].
   Strings include the line terminator and CRLF is normalised to LF if
   [normalise] is [true]. The sequence raises [Exit] if normalise is [true] and
   a terminated line is encountered which does not end CRLF. The final line of
   the sequence only includes a terminator if it is present in the file (and a
   terminating CR is never normalised if not strictly followed by LF). *)
let line_seq_of_in_channel ~normalise ic =
  let normalise =
    if normalise then
      fun s ->
        if last_is_cr s then
          String.sub s 0 (String.length s - 1)
        else
          raise Exit
    else
      Fun.id
  in
    let rec read_line last () =
      (* Read the next line to determine if the last line ended with LF *)
      match input_line ic with
      | line ->
          Seq.Cons (normalise last ^ "\n", read_line line)
      | exception End_of_file ->
          (* EOF reached - seek the last character to determine if the final
             line ends in LF *)
          let last =
            if last_char ic = '\n' then
              normalise last ^ "\n"
            else
              last
          in
            Seq.Cons (last, Seq.empty)
    in
      read_line

let compare_text_files ignored_lines file1 file2 =
  Sys.with_input_file ~bin:true file2 @@ fun ic2 ->
    (* Get the first non-dropped line of file2 and determine if could be
       CRLF-normalised (it can't be in any of the dropped lines didn't end
       CRLF. *)
    let (crlf_endings2, line2, reached_end_file2) =
      let rec loop crlf_endings2 k =
        match input_line ic2 with
        | line ->
            let crlf_endings2 = crlf_endings2 && last_is_cr line in
            if k = 0 then
              (crlf_endings2, line, false)
            else
              loop crlf_endings2 (pred k)
        | exception End_of_file ->
            (false, "", true)
      in
        loop true ignored_lines
    in
      Sys.with_input_file ~bin:true file1 @@ fun ic1 ->
        if reached_end_file2 then
          (* We reached the end of file2 while ignoring lines, so only an empty
             file can be identical, as in the binary comparison case. *)
          if in_channel_length ic1 = 0 then
            Same
          else
            Different
        else
          (* file2 has at least one non-ignored line *)
          match input_line ic1 with
          | exception End_of_file -> Different
          | line1 ->
              let crlf_endings1 = last_is_cr line1 in
              (* If both files appear to have CRLF endings, then there's no need
                 to attempt to normalise either. *)
              let seq1 =
                let normalise = crlf_endings1 && not crlf_endings2 in
                line_seq_of_in_channel ~normalise ic1 line1 in
              let seq2 =
                let normalise = crlf_endings2 && not crlf_endings1 in
                line_seq_of_in_channel ~normalise ic2 line2 in
              try
                if Seq.equal seq1 seq2 then
                  Same
                else
                  raise Exit
              with Exit ->
                (* Either the lines weren't equal, or the file which was being
                   normalised suddenly had a line which didn't end CRLF. In this
                   case, the files must differ since only one file is ever being
                   normalised, so the earlier lines differed too. *)
                Different

(* Version of Stdlib.really_input which stops at EOF, rather than raising
   an exception. *)
let really_input_up_to ic =
  let block_size = 8192 in
  let buf = Bytes.create block_size in
  let rec read pos =
    let bytes_read = input ic buf pos (block_size - pos) in
    let new_pos = pos + bytes_read in
    if bytes_read = 0 || new_pos = block_size then
      new_pos
    else
      read new_pos
  in
  let bytes_read = read 0 in
  if bytes_read = block_size then
    buf
  else
    Bytes.sub buf 0 bytes_read

let compare_binary_files bytes_to_ignore file1 file2 =
  Sys.with_input_file ~bin:true file1 @@ fun ic1 ->
  Sys.with_input_file ~bin:true file2 @@ fun ic2 ->
  seek_in ic1 bytes_to_ignore;
  seek_in ic2 bytes_to_ignore;
  let rec compare () =
    let block1 = really_input_up_to ic1 in
    let block2 = really_input_up_to ic2 in
    if block1 = block2 then
      if Bytes.length block1 > 0 then
        compare ()
      else
        Same
    else
      Different
  in
  compare ()

let compare_files ?(tool = default_comparison_tool) files =
  match tool with
  | External {tool_name; tool_flags; result_of_exitcode} ->
      let commandline = String.concat " "
      [
        tool_name;
        tool_flags;
        files.reference_filename;
        files.output_filename
      ] in
      let settings = Run_command.settings_of_commandline
        ~stdout_fname:Filename.null ~stderr_fname:Filename.null commandline in
      let status = Run_command.run settings in
      result_of_exitcode commandline status
  | Internal ignore ->
      match files.filetype with
        | Text ->
            (* bytes_to_ignore is silently ignored for text files *)
            compare_text_files ignore.lines
              files.reference_filename files.output_filename
        | Binary ->
            compare_binary_files ignore.bytes
                                 files.reference_filename files.output_filename

let check_file ?(tool = default_comparison_tool) files =
  if Sys.file_exists files.reference_filename
  then compare_files ~tool:tool files
  else begin
    if Sys.file_is_empty files.output_filename
    then Same
    else Unexpected_output
  end

let diff files =
  let temporary_file = Filename.temp_file "ocamltest" "diff" in
  let diff_commandline =
    Filename.quote_command "diff" ~stdout:temporary_file
      [ "--strip-trailing-cr"; "-u";
        files.reference_filename;
        files.output_filename ]
  in
  let result =
    match Sys.command diff_commandline with
    | 0 -> Ok "Inconsistent LF/CRLF line-endings"
    | 2 -> Stdlib.Error "diff"
    | _ -> Ok (Sys.string_of_file temporary_file)
  in
  Sys.force_remove temporary_file;
  result

let promote {filetype; reference_filename; output_filename} ignore_conf =
  match filetype, ignore_conf with
  | Text, {lines = skip_lines; _} ->
      Sys.with_output_file reference_filename @@ fun reference ->
      Sys.with_input_file output_filename @@ fun output ->
      for _ = 1 to skip_lines do
        try ignore (input_line output) with End_of_file -> ()
      done;
      Sys.copy_chan output reference
  | Binary, {bytes = skip_bytes; _} ->
      Sys.with_output_file ~bin:true reference_filename @@ fun reference ->
      Sys.with_input_file ~bin:true output_filename @@ fun output ->
      seek_in output skip_bytes;
      Sys.copy_chan output reference
