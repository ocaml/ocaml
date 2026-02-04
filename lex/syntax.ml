(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This apparently useless implementation file is in fact required
   by the pa_ocamllex syntax extension *)

open Printf

(* The shallow abstract syntax *)

type location = {
  loc_file : string;
  start_pos : int;
  end_pos : int;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

let line_col_of_pos (x : Lexing.position) : int * int =
  (x.pos_lnum, x.pos_cnum - x.pos_bol)

let location_of_positions (a : Lexing.position) (b : Lexing.position)
  : location =
  let start_line, start_col = line_col_of_pos a in
  let end_line, end_col = line_col_of_pos b in
  {
    loc_file = a.pos_fname;
    start_pos = a.pos_cnum;
    end_pos = b.pos_cnum;
    start_line;
    start_col;
    end_line;
    end_col;
  }

type regular_expression =
    Epsilon
  | Characters of Cset.t
  | Eof
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression
  | Bind of regular_expression * (string * location)

type ('arg, 'action) entry = {
  name: string;
  shortest: bool;
  args: 'arg;
  body_location: location;
  clauses: (regular_expression * 'action) list
}

type lexer_definition = {
  header: location;
  entrypoints: ((string list, location) entry) list;
  trailer: location;
  refill_handler : location option;
}

(* Using the format described at https://github.com/ocaml/ocaml/pull/8541
   for multiline locations *)
let show_location loc =
  let open Printf in
  let lines =
    if loc.start_line = loc.end_line then
      sprintf "line %d" loc.start_line
    else
      sprintf "lines %d-%d" loc.start_line loc.end_line
  in
  sprintf "File %S, %s, characters %d-%d"
    loc.loc_file
    lines
    loc.start_col
    loc.end_col

(*
   Roughly the same format as Lexer.warning.
   TODO: reuse code to ensure consistency?

   - We could have command-line options to enable or disable
     warnings, or make them fatal if desired.
   - It would be nice to use the same function as the OCaml compilers
     to print and highlight the affected snippet of code.
*)
let print_warning ?(fatal = false) ?name loc msg =
  Printf.eprintf
    "ocamllex %swarning%s:\n\
     %s: %s\n"
    (if fatal then "fatal " else "")
    (match name with
     | None -> ""
     | Some name -> sprintf " [%s]" name)
    (show_location loc) msg;
  flush stderr
