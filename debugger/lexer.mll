(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

{

open Primitives
open Parser

}

rule line =	(* Read a whole line *)
  parse
    [ ^ '\n' ]* '\n'
      { let line =
          Lexing.lexeme lexbuf
        in
          String.sub line 0 (String.length line - 1) }
  | [ ^ '\n' ]*
      { Lexing.lexeme lexbuf }
  | eof
      { raise Exit }

and argument =	(* Read a raw argument *)
  parse
    [ ^ ' ' '\t' ]+
      { ARGUMENT (Lexing.lexeme lexbuf) }
  | [' ' '\t']+
      { argument lexbuf }
  | eof
      { EOL }
  | _
      { raise Parsing.Parse_error }

and line_argument =
  parse
    _*
      { ARGUMENT (Lexing.lexeme lexbuf) }
  | eof
      { EOL }

and lexeme =	(* Read a lexeme *)
  parse
    [' ' '\t'] +
      { lexeme lexbuf }
  | "prefix"
      { PREFIX }
  | ['A'-'Z' 'a'-'z' '\192'-'\214' '\216'-'\246' '\248'-'\255' ]
    ( '_' ? ['A'-'Z' 'a'-'z' '\192'-'\214' '\216'-'\246' '\248'-'\255' ''' '0'-'9' ] ) *
      { IDENTIFIER (Lexing.lexeme lexbuf) }
  | '"' [^ '"']* "\""
      { let s = Lexing.lexeme lexbuf in
        IDENTIFIER (String.sub s 1 (String.length s - 2)) }
  | ['0'-'9']+
    | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
    | '0' ['o' 'O'] ['0'-'7']+
    | '0' ['b' 'B'] ['0'-'1']+
      { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
  | '*'
      { STAR }
  | "-"
      { MINUS }
  | "__"
      { UNDERUNDER }
  | "#"
      { SHARP }
  | "@"
      { AT }
  | "::"
      { COLONCOLON }
  | ","
      { COMMA }
  | "_"
      { UNDERSCORE }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "["
      { LBRACKET }
  | "]"
      { RBRACKET }
  | "{"
      { LBRACE }
  | "}"
      { RBRACE }
  | ";"
      { SEMI }
  | "="
      { EQUAL }
  | ">"
      { SUPERIOR }
  | [ '!' '?' '=' '<' '>' '@' '^' '|' '&' '~' '+' '-' '*' '/' '%' ]
    [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' ';' 
      '<' '=' '>' '?' '@' '^' '|' '~'] *
      { OPERATOR (Lexing.lexeme lexbuf) }
  | eof
      { EOL }
  | _
      { raise Parsing.Parse_error }
