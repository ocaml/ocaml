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

(* Entry points in the parser *)

(* Skip tokens to the end of the phrase *)

module Unsafe = struct
let rec skip_phrase lexbuf =
  try
    if !Clflags.safe_syntax then
    match Lexer.token lexbuf with
      Parser.SEMISEMI | Parser.EOF -> ()
    | _ -> skip_phrase lexbuf
  with
    | Lexer.Error (Lexer.Unterminated_comment _, _)
    | Lexer.Error (Lexer.Unterminated_string, _)
    | Lexer.Error (Lexer.Unterminated_string_in_comment _, _)
    | Lexer.Error (Lexer.Illegal_character _, _) -> skip_phrase lexbuf
;;

let maybe_skip_phrase lexbuf =
  if Parsing.is_current_lookahead Parser.SEMISEMI
  || Parsing.is_current_lookahead Parser.EOF
  then ()
  else skip_phrase lexbuf

let wrap parsing_fun lexbuf =
  try
    Docstrings.init ();
    Lexer.init ();
    let ast = parsing_fun Lexer.token lexbuf in
    Parsing.clear_parser();
    Docstrings.warn_bad_docstrings ();
    ast
  with
  | Lexer.Error(Lexer.Illegal_character _, _) as err
    when !Location.input_name = "//toplevel//"->
      skip_phrase lexbuf;
      raise err
  | Syntaxerr.Error _ as err
    when !Location.input_name = "//toplevel//" ->
      maybe_skip_phrase lexbuf;
      raise err
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      if !Location.input_name = "//toplevel//"
      then maybe_skip_phrase lexbuf;
      raise(Syntaxerr.Error(Syntaxerr.Other loc))
end

module Safe = struct
let rec skip_phrase lexbuf =
  try
    if !Clflags.safe_syntax then
    match Lexer_safe.token lexbuf with
      Parser_safe.SEMISEMI | Parser_safe.EOF -> ()
    | _ -> skip_phrase lexbuf
  with
    | Lexer_safe.Error (Lexer_safe.Unterminated_comment _, _)
    | Lexer_safe.Error (Lexer_safe.Unterminated_string, _)
    | Lexer_safe.Error (Lexer_safe.Unterminated_string_in_comment _, _)
    | Lexer_safe.Error (Lexer_safe.Illegal_character _, _) -> skip_phrase lexbuf
;;

let maybe_skip_phrase lexbuf =
  if Parsing.is_current_lookahead Parser_safe.SEMISEMI
  || Parsing.is_current_lookahead Parser_safe.EOF
  then ()
  else skip_phrase lexbuf

let wrap parsing_fun lexbuf =
  try
    Docstrings.init ();
    Lexer_safe.init ();
    let ast = parsing_fun Lexer_safe.token lexbuf in
    Parsing.clear_parser();
    Docstrings.warn_bad_docstrings ();
    ast
  with
  | Lexer_safe.Error(Lexer_safe.Illegal_character _, _) as err
    when !Location.input_name = "//toplevel//"->
      skip_phrase lexbuf;
      raise err
  | Syntaxerr.Error _ as err
    when !Location.input_name = "//toplevel//" ->
      maybe_skip_phrase lexbuf;
      raise err
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      if !Location.input_name = "//toplevel//"
      then maybe_skip_phrase lexbuf;
      raise(Syntaxerr.Error(Syntaxerr.Other loc))
end

let wrap unsafe safe x =
  if !Clflags.safe_syntax then Safe.wrap safe x else Unsafe.wrap unsafe x

let implementation = wrap Parser.implementation Parser_safe.implementation
and interface = wrap Parser.interface Parser_safe.interface
and toplevel_phrase = wrap Parser.toplevel_phrase Parser_safe.toplevel_phrase
and use_file = wrap Parser.use_file Parser_safe.use_file
and core_type = wrap Parser.parse_core_type Parser_safe.parse_core_type
and expression = wrap Parser.parse_expression Parser_safe.parse_expression
and pattern = wrap Parser.parse_pattern Parser_safe.parse_pattern
