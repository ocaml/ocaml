(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

{

open Debugger_parser


let ident_for_extended raw_name =
  match Misc.Utf8_lexeme.normalize raw_name with
  | Error _ -> raise Parsing.Parse_error
  | Ok name ->
  match Misc.Utf8_lexeme.validate_identifier name with
  | Misc.Utf8_lexeme.Valid -> name
  | Misc.Utf8_lexeme.Invalid_character _
  | Misc.Utf8_lexeme.Invalid_beginning _ ->
    raise Parsing.Parse_error

exception Int_overflow

}

let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identstart = lowercase | uppercase
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let utf8 = ['\192'-'\255'] ['\128'-'\191']*
let identstart_ext = identstart | utf8
let identchar_ext = identchar | utf8
let ident_ext = identstart_ext  identchar_ext*

rule line =     (* Read a whole line *)
  parse
    ([ ^ '\n' '\r' ]* as s) ('\n' | '\r' | "\r\n")
      { s }
  | [ ^ '\n' '\r' ]*
      { Lexing.lexeme lexbuf }
  | eof
      { raise Exit }

and argument =  (* Read a raw argument *)
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
    _ *
      { ARGUMENT (Lexing.lexeme lexbuf) }
  | eof
      { EOL }

and lexeme =    (* Read a lexeme *)
  parse
    [' ' '\t'] +
      { lexeme lexbuf }
  | lowercase identchar*
      { LIDENT(Lexing.lexeme lexbuf) }
  | uppercase identchar*
      { UIDENT(Lexing.lexeme lexbuf) }
  | ident_ext as raw_name
      {
        let name = ident_for_extended raw_name in
        if Misc.Utf8_lexeme.is_capitalized name
        then UIDENT name
        else LIDENT name
      }
  | '"' [^ '"']* "\""
      { let s = Lexing.lexeme lexbuf in
        LIDENT(String.sub s 1 (String.length s - 2)) }
  | ['0'-'9']+
    | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
    | '0' ['o' 'O'] ['0'-'7']+
    | '0' ['b' 'B'] ['0'-'1']+
      { try INTEGER (Int64.of_string (Lexing.lexeme lexbuf))
        with Failure _ -> raise Int_overflow
      }
  | '*'
      { STAR }
  | "-"
      { MINUS }
  | "."
      { DOT }
  | "#"
      { HASH }
  | "@"
      { AT }
  | "$"
      { DOLLAR }
  | ":"
      { COLON }
  | "!"
      { BANG }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "["
      { LBRACKET }
  | "]"
      { RBRACKET }
  | ['!' '?' '~' '=' '<' '>' '|' '&' '$' '@' '^' '+' '-' '*' '/' '%']
    ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'] *
      { OPERATOR (Lexing.lexeme lexbuf) }
  | eof
      { EOL }
  | _
      { raise Parsing.Parse_error }
