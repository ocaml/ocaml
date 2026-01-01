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

(* The shallow abstract syntax *)

type location = {
  loc_file : string;
  start_pos : int;
  end_pos : int;
  start_line : int;
  start_col : int;
}

let location_of_positions (a : Lexing.position) (b : Lexing.position)
  : location =
  let f = a.pos_fname in
  let n1 = a.pos_cnum
  and l1 = a.pos_lnum
  and s1 = a.pos_bol in
  let n2 = b.pos_cnum in
  {
    loc_file = f;
    start_pos = n1;
    end_pos = n2;
    start_line = l1;
    start_col = n1 - s1
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

let show_location loc =
  Printf.sprintf "File %S, line %d, characters %d-%d"
    loc.loc_file
    loc.start_line
    loc.start_col
    (loc.start_col + loc.end_pos - loc.start_pos)

(*
   Roughly the same format as Lexer.warning.
   TODO: reuse code to ensure consistency?

   - We could have command-line options to enable or disable
     warnings, or make them fatal if desired.
   - It would be nice to use the same function as the OCaml compilers
     to print and highlight the affected snippet of code.
*)
let print_warning loc msg =
  Printf.eprintf
    "ocamllex warning:\n\
     %s: %s\n"
    (show_location loc) msg;
  flush stderr
