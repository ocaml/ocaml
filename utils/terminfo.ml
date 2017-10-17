(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Paris                  *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Printf

external isatty : out_channel -> bool = "caml_sys_isatty"
external terminfo_rows: out_channel -> int = "caml_terminfo_rows"

type status =
  | Uninitialised
  | Bad_term
  | Good_term

let setup () = 
  let term = try Sys.getenv "TERM" with Not_found -> "" in
  (* Same heuristics as in Misc.Color.should_enable_color *)
  if term <> "" && term <> "dumb" && isatty stdout
  then Good_term
  else Bad_term

let num_lines () =
  let rows = terminfo_rows stdout in
  if rows > 0 then rows else 24
    (* 24 is a reasonable default for an ANSI-style terminal *)

let backup n =
  if n >= 1 then begin
    printf "\027[%dA" n; flush stdout
  end

let resume n =
  if n >= 1 then begin
    printf "\027[%dB" n; flush stdout
  end

let standout b =
  output_string stdout (if b then "\027[4m" else "\027[0m"); flush stdout
