(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Entry points in the parser *)

open Location

(* Skip tokens to the end of the phrase *)

let rec skip_phrase lexbuf =
  try
    match Lexer.token lexbuf with
      Parser.SEMISEMI | Parser.EOF -> ()
    | _ -> skip_phrase lexbuf
  with
    | Lexer.Error (Lexer.Unterminated_comment, _) -> ()
    | Lexer.Error (Lexer.Unterminated_string, _) -> ()
    | Lexer.Error (Lexer.Unterminated_string_in_comment, _) -> ()
    | Lexer.Error (Lexer.Illegal_character _, _) -> skip_phrase lexbuf
;;

let maybe_skip_phrase lexbuf =
  if Parsing.is_current_lookahead Parser.SEMISEMI
  || Parsing.is_current_lookahead Parser.EOF
  then ()
  else skip_phrase lexbuf

let wrap parsing_fun lexbuf =
  try
    let ast = parsing_fun Lexer.token lexbuf in
    Parsing.clear_parser();
    ast
  with
  | Lexer.Error(Lexer.Unterminated_comment, _) as err -> raise err
  | Lexer.Error(Lexer.Unterminated_string, _) as err -> raise err
  | Lexer.Error(Lexer.Unterminated_string_in_comment, _) as err -> raise err
  | Lexer.Error(Lexer.Illegal_character _, _) as err ->
      if !Location.input_name = "//toplevel//" then skip_phrase lexbuf;
      raise err
  | Syntaxerr.Error _ as err ->
      if !Location.input_name = "//toplevel//" then maybe_skip_phrase lexbuf;
      raise err
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      if !Location.input_name = "//toplevel//"
      then maybe_skip_phrase lexbuf;
      raise(Syntaxerr.Error(Syntaxerr.Other loc))
;;

let implementation = wrap Parser.implementation
and interface = wrap Parser.interface
and toplevel_phrase = wrap Parser.toplevel_phrase
and use_file = wrap Parser.use_file
