(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Entry points in the parser *)

exception Error of int * int        (* Syntax error *)
(* Skip tokens to the end of the phrase *)

let rec skip_phrase lexbuf =
  try
    match Lexer.token lexbuf with
      Parser.SEMISEMI | Parser.EOF -> ()
    | _ -> skip_phrase lexbuf
  with
    | Lexer.Error (Lexer.Unterminated_comment, _, _) -> ()
    | Lexer.Error (Lexer.Unterminated_string, _, _) -> ()
    | Lexer.Error(_,_,_) -> skip_phrase lexbuf

let maybe_skip_phrase lexbuf =
  if Parsing.is_current_lookahead Parser.SEMISEMI
  or Parsing.is_current_lookahead Parser.EOF
  then ()
  else skip_phrase lexbuf

let wrap parsing_fun lexbuf =
  try
    let ast = parsing_fun Lexer.token lexbuf in
    Parsing.clear_parser();
    ast
  with
    | Lexer.Error(Lexer.Unterminated_comment, _, _) as err -> raise err
    | Lexer.Error(Lexer.Unterminated_string, _, _) as err -> raise err
    | Lexer.Error(_, _, _) as err ->
        if !Location.input_name = "" then skip_phrase lexbuf;
        raise err
    | Parsing.Parse_error ->
        let start = Lexing.lexeme_start lexbuf
        and stop  = Lexing.lexeme_end lexbuf in
        if !Location.input_name = "" 
        then maybe_skip_phrase lexbuf;
        raise(Error(start, stop))

let implementation = wrap Parser.implementation
and interface = wrap Parser.interface
and toplevel_phrase = wrap Parser.toplevel_phrase
and use_file = wrap Parser.use_file
