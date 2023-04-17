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

let copy_newlines ic oc up_to =
  try
    while pos_in ic < up_to do
      let c = input_char ic in
      if c = '\n' || c = '\r' then output_char oc c
    done
  with End_of_file -> ()

let tsl_block_of_file test_filename =
  let input_channel = open_in test_filename in
  let lexbuf = Lexing.from_channel input_channel in
  Location.init lexbuf test_filename;
  let block = Tsl_parser.tsl_block Tsl_lexer.token lexbuf in
  close_in input_channel;
  block


type style = { opening : string; closing : string }
let c_style = { opening = "/*"; closing = "*/" }
let ocaml_style = { opening = "(*"; closing = "*)" }

let file force_below f =
  let tsl_block = tsl_block_of_file f in
  let (rootenv_statements, test_trees) =
    match tsl_block with
    | Tsl_ast.Old l -> Tsl_semantics.test_trees_of_tsl_block l
    | Tsl_ast.New asts -> Tsl_semantics.test_trees_of_tsl_asts asts
  in
  let asts =
    Tsl_semantics.tsl_asts_of_test_trees (rootenv_statements, test_trees)
  in
  let lex_ic = open_in f in
  let copy_ic = open_in f in
  let lexbuf = Lexing.from_channel lex_ic in
  Location.init lexbuf f;
  let rec seek_to_begin () =
    match Tsl_lexer.token lexbuf with
    | Tsl_parser.TSL_BEGIN_C_STYLE below -> (c_style, below)
    | Tsl_parser.TSL_BEGIN_OCAML_STYLE below -> (ocaml_style, below)
    | _ -> seek_to_begin ()
  in
  let rec seek_to_end () =
    match Tsl_lexer.token lexbuf with
    | Tsl_parser.TSL_END_C_STYLE -> ()
    | Tsl_parser.TSL_END_OCAML_STYLE -> ()
    | _ -> seek_to_end ()
  in
  let (style, below) = seek_to_begin () in
  copy copy_ic stdout Lexing.(lexbuf.lex_curr_p.pos_cnum);
  if below || not force_below then begin
    printf "\n";
    List.iter (Tsl_semantics.print_tsl_ast stdout) asts;
    seek_to_end ();
    seek_in copy_ic Lexing.(lexbuf.lex_start_p.pos_cnum);
    copy copy_ic stdout max_int;
  end else begin
    printf "_BELOW";
    seek_to_end ();
    copy_newlines copy_ic stdout lexbuf.Lexing.lex_start_pos;
    copy copy_ic stdout max_int;
    printf "\n%s TEST\n" style.opening;
    List.iter (Tsl_semantics.print_tsl_ast stdout) asts;
    printf "%s\n" style.closing;
  end;
  flush stdout;
  close_in lex_ic;
  close_in copy_ic;
