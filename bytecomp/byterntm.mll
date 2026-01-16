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

{
type search_method =
| Disable of string
| Fallback of string
| Enable

(* First word of the current line being analysed - [exec ...], [r=...], or
   [c=...] *)
type state = Exec | R | C of string

let cut_runtime_id search name =
  let len = String.length name in
  let id =
    if len < 6 || name.[len - 5] <> '-' then
      None
    else
      Misc.RuntimeID.of_string (String.sub name (len - 4) 4)
  in
  let name =
    if id = None then
      name
    else
      String.sub name 0 (len - 5)
  in
  Some (name, id, search)
}

rule analyze = parse
(* RNTM section for -runtime-search absolute or shebang directly to the
   runtime *)
  | "#!" ([^ ' ' '\n']* as dir) ('/' as sep) ([^ '/' ' ' '\n']+ as runtime) '\n'
  | ([^ '\000']* as dir) (['/' '\\' '\000'] as sep) (* Directory portion *)
    ([^ '\\' '/' '\000']+ as runtime) eof           (* Runtime portion *)
      { if sep = '\000' then
          if dir = "" then
            cut_runtime_id Enable runtime
          else
            let dir = Filename.concat dir "" in
            cut_runtime_id (Fallback dir) runtime
        else
          let dir = dir ^ String.make 1 sep in
          cut_runtime_id (Disable dir) runtime }

(* Shell script launcher (if it matches, this always matches more than the above
   regexp) *)
  | "#!" [^ ' ' '\n']+ "/sh\n" (("exec '" | "r='") as next)
      { let state = if next.[0] = 'r' then R else Exec in
        analyze_sh_launcher state (Buffer.create 1024) lexbuf }

  | _ | eof
      { None }

and analyze_sh_launcher state b = parse
(* An embedded single quote *)
  | "'\\''"
      { analyze_sh_launcher state (Buffer.add_char b '\''; b) lexbuf }

  | [^ '\'' ]+ as s
      { analyze_sh_launcher state (Buffer.add_string b s; b) lexbuf }

(* exec line for -runtime-search disable *)
  | "' \"$0\" \"$@\"\n"
      { if state = Exec then
          let name = Buffer.contents b in
          let runtime = Filename.basename name in
          let dir =
            String.sub name 0 (String.length name - String.length runtime)
          in
          cut_runtime_id (Disable dir) runtime
        else
          None }

(* r= line for -runtime-search {fallback,enable} *)
  | "'\n" ("c='" as c)?
      { if state = R then
          let runtime = Buffer.contents b in
          if c = None then
            cut_runtime_id Enable runtime
          else
            analyze_sh_launcher (C runtime) (Buffer.clear b; b) lexbuf
        else
          None }

(* c= line for -runtime-search fallback *)
  | "'\"$r\"\n"
      { match state with
        | C runtime ->
            cut_runtime_id (Fallback (Buffer.contents b)) runtime
        | _ ->
            None }

  | _ | eof
      { None }

{
let read_runtime t ic =
  seek_in ic 0;
  let lexbuf =
    try
      if really_input_string ic 2 = "#!" then
        let () = seek_in ic 0 in
        Some (Lexing.from_channel ic)
      else
        let rntm = Bytesections.(read_section_string t ic Name.RNTM) in
        Some (Lexing.from_string rntm)
    with End_of_file | Not_found -> None
  in
  Option.bind lexbuf analyze
}
