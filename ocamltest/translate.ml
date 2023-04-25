(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Damien Doligez, projet Cambium, INRIA Paris              *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translate a test file from old to new syntax. *)

open Stdlib
open Printf

let copy ic oc up_to =
  try
    while pos_in ic < up_to do
      output_char oc (input_char ic)
    done
  with End_of_file -> ()

let text =
  "Filler_text_added_to_preserve_locations_while_translating_from_old_syntax__"
let len = String.length text
let index = ref (-1)
let lorem () = incr index; text.[!index mod len]

type mode =
| Keep_chars of int (* how many chars to skip before keeping chars *)
| Keep_lines

let copy_newlines ~mode ic oc up_to =
  let skip, insert =
    match mode with
    | Keep_lines ->
      ref max_int, ref "(* Blank lines added here to preserve locations. *)"
    | Keep_chars n -> ref n, ref ""
  in
  try
    while pos_in ic < up_to do
      let c = input_char ic in
      if c = '\n' || c = '\r' then begin
        output_char oc c;
        output_string oc !insert;
        insert := "";
      end else if !skip <= 0 then
        output_char oc (lorem ())
      else
        decr skip
    done
  with End_of_file -> ()

let tsl_block_of_file test_filename =
  let input_channel = open_in test_filename in
  let lexbuf = Lexing.from_channel input_channel in
  Location.init lexbuf test_filename;
  try
    let block = Tsl_parser.tsl_block Tsl_lexer.token lexbuf in
    close_in input_channel;
    if !Tsl_lexer.has_comments then
      eprintf "%s:1.0: warning: test script has comments\n" test_filename;
    block
  with
  | Parsing.Parse_error ->
    let open Lexing in
    let p = lexbuf.lex_start_p in
    Printf.eprintf "%s:%d.%d: syntax error in test script\n%!"
      test_filename p.pos_lnum (p.pos_cnum - p.pos_bol);
    raise Parsing.Parse_error

(* In what style to output the translated test file *)
type style =
| Plain
| Lines
| Chars

(* What kind of comments are used in the test file *)
type kind = { opening : string; closing : string }
let c_kind = { opening = "/*"; closing = "*/" }
let ocaml_kind = { opening = "(*"; closing = "*)" }

let file ~style ~compact f =
  let tsl_block = tsl_block_of_file f in
  let (rootenv_statements, test_trees) =
    Tsl_semantics.test_trees_of_tsl_block tsl_block
  in
  let ast =
    Tsl_semantics.tsl_ast_of_test_trees (rootenv_statements, test_trees)
  in
  let lex_ic = open_in f in
  let copy_ic = open_in f in
  let lexbuf = Lexing.from_channel lex_ic in
  Location.init lexbuf f;
  let rec seek_to_begin () =
    match Tsl_lexer.token lexbuf with
    | Tsl_parser.TSL_BEGIN_C_STYLE position -> (c_kind, position)
    | Tsl_parser.TSL_BEGIN_OCAML_STYLE position -> (ocaml_kind, position)
    | _ -> seek_to_begin ()
  in
  let rec seek_to_end () =
    match Tsl_lexer.token lexbuf with
    | Tsl_parser.TSL_END_C_STYLE -> ()
    | Tsl_parser.TSL_END_OCAML_STYLE -> ()
    | _ -> seek_to_end ()
  in
  let (kind, position) = seek_to_begin () in
  copy copy_ic stdout Lexing.(lexbuf.lex_curr_p.pos_cnum);
  if position = `Below || style = Plain then begin
    print_string (if ast = Tsl_ast.Ast ([], []) then " " else "\n");
    Tsl_semantics.print_tsl_ast ~compact stdout ast;
    seek_to_end ();
    seek_in copy_ic Lexing.(lexbuf.lex_start_p.pos_cnum);
    copy copy_ic stdout max_int;
  end else begin
    printf "_BELOW";
    seek_to_end ();
    let limit = Lexing.(lexbuf.lex_start_p.pos_cnum) in
    let mode =
      match style with
      | Lines -> Keep_lines
      | Chars -> Keep_chars 6
      | Plain -> assert false
    in
    copy_newlines ~mode copy_ic stdout limit;
    copy copy_ic stdout max_int;
    printf "\n%s TEST\n" kind.opening;
    Tsl_semantics.print_tsl_ast ~compact stdout ast;
    printf "%s\n" kind.closing;
  end;
  flush stdout;
  close_in lex_ic;
  close_in copy_ic;
