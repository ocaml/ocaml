(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This small script prints a file annotated with line directives using the
    source currently pointed at by the last line directive. It aims to check
    that vendored libraries don't diverge from their original sources. *)

module Opened_files = Map.Make(String)

type 'a diff = { chimera:'a; source:'a }

let chimera lines index = lines.chimera.(index.chimera)
let source lines index = lines.source.(index.source)

let open_file opened filename = match Opened_files.find_opt filename opened with
  | Some a -> a, opened
  | None ->
      let lines =
        filename
        |> In_channel.open_text
        |> In_channel.input_lines
        |> Array.of_list
      in
      lines, Opened_files.add filename lines opened

let parse_line_directive s =
  Scanf.sscanf_opt s "#%d %S" (fun pos file -> pos-1,file)

let one_line (cursor,lines,opened_files) =
  if cursor.chimera >= Array.length lines.chimera then `Stop
  else
    match parse_line_directive (chimera lines cursor) with
    | Some (pos,file) ->
        Printf.printf "%s\n" (chimera lines cursor);
        let cursor = { source = pos; chimera = cursor.chimera + 1 } in
        let file, opened_files = open_file opened_files file in
        `Next (cursor, { lines with source = file}, opened_files)
    | None ->
        Printf.printf "%s\n" (source lines cursor);
        let cursor = {
          source = cursor.source + 1;
          chimera = cursor.chimera + 1
        }
        in
        `Next (cursor, lines, opened_files)

let rec all_lines state =
  match one_line state with
  | `Next state -> all_lines state
  | `Stop -> ()

let replay file =
  let chimera, opened_files = open_file Opened_files.empty file in
  all_lines ({chimera=0;source=0}, {chimera;source=chimera}, opened_files)

let () = replay Sys.argv.(1)
