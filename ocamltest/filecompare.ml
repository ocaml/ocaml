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

let read_text_file lines_to_drop fn =
  (* 1. Read the entire file into a string (hopefully we'll never have reference
     files over 16MiB!) *)
  let ic_len, data =
    Sys.with_input_file ~bin:true fn @@ fun ic ->
      let ic_len = in_channel_length ic in
      let data = Bytes.create ic_len in
      really_input ic data 0 ic_len;
      ic_len, data
  in

  (* 2. Scan the data and check if *every* '\n' is preceded by a '\r' *)
  let has_crlf_eols =
    let rec loop last_was_lf i =
      if i < 0 then
        (* Guard against the first character of the file being '\n' *)
        not last_was_lf
      else if last_was_lf then
        (* Check that '\n' iss preceded by '\r': fast path to false if not *)
        if Bytes.get data i = '\r' then
          loop false (i - 1)
        else
          false
      else
        (* Scan the file, noting if '\n' just seen *)
        loop (Bytes.get data i = '\n') (i - 1)
    in
      loop false (ic_len - 1)
  in

  (* [extract_string start length] returns a string from data, but strips the CR
     if required *)
  let extract_string start length =
    (* This function will never be asked to return an empty string - if the last
       line of a file is the empty string, then the file *must* be terminated
       with an EOL *)
    if has_crlf_eols && Bytes.get data (start + length - 1) = '\n' then begin
      (* Change the '\r' to '\n' *)
      Bytes.set data (start + length - 2) '\n';
      Bytes.sub_string data start (length - 1)
    end else
      Bytes.sub_string data start length
  in

  (* 3. Scan the data a second time, returning the lines *)
  let rec loop acc current_end i =
    if i < 0 then
      (* Reached the beginning *)
      (extract_string 0 (current_end + 1))::acc
    else if Bytes.get data i = '\n' then
      (* Emit the previous line, mark the end of this one *)
      loop ((extract_string (i + 1) (current_end - i))::acc) i (i - 1)
    else
      loop acc current_end (i -  1)
  in
    if ic_len = 0 then
      []
    else
      let lines =
        (* Handle whether there's EOL at EOF here - loop would otherwise emit
           an empty string as the last line for an EOL-at-EOF file. *)
        if Bytes.get data (ic_len - 1) = '\n' then
          loop [] (ic_len - 1) (ic_len - 2)
        else
          loop [] (ic_len - 1) (ic_len - 1)
      in
        List.drop_first lines_to_drop lines

let compare_text_files dropped_lines file1 file2 =
  if read_text_file 0 file1 = read_text_file dropped_lines file2 then
    Same
  else
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
