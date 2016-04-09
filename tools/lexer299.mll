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

(* The lexer definition *)

{
open Lexing
open Misc

type token =
    AMPERAMPER
  | AMPERSAND
  | AND
  | AS
  | ASSERT
  | BACKQUOTE
  | BAR
  | BARBAR
  | BARRBRACKET
  | BEGIN
  | CHAR of (char)
  | CLASS
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | COLONGREATER
  | COMMA
  | CONSTRAINT
  | DO
  | DONE
  | DOT
  | DOTDOT
  | DOWNTO
  | ELSE
  | END
  | EOF
  | EQUAL
  | EXCEPTION
  | EXTERNAL
  | FALSE
  | FLOAT of (string)
  | FOR
  | FUN
  | FUNCTION
  | FUNCTOR
  | GREATER
  | GREATERRBRACE
  | GREATERRBRACKET
  | IF
  | IN
  | INCLUDE
  | INFIXOP0 of (string)
  | INFIXOP1 of (string)
  | INFIXOP2 of (string)
  | INFIXOP3 of (string)
  | INFIXOP4 of (string)
  | INHERIT
  | INITIALIZER
  | INT of (int)
  | LABEL of (string)
  | LABELID of (string)
  | LAZY
  | LBRACE
  | LBRACELESS
  | LBRACKET
  | LBRACKETBAR
  | LBRACKETLESS
  | LESS
  | LESSMINUS
  | LET
  | LIDENT of (string)
  | LPAREN
  | MATCH
  | METHOD
  | MINUSGREATER
  | MODULE
  | MUTABLE
  | NEW
  | OBJECT
  | OF
  | OPEN
  | OR
  | PARSER
  | PREFIXOP of (string)
  | PRIVATE
  | QUESTION
  | QUESTION2
  | QUOTE
  | RBRACE
  | RBRACKET
  | REC
  | RPAREN
  | SEMI
  | SEMISEMI
  | SHARP
  | SIG
  | STAR
  | STRING of (string)
  | STRUCT
  | SUBTRACTIVE of (string)
  | THEN
  | TO
  | TRUE
  | TRY
  | TYPE
  | UIDENT of (string)
  | UNDERSCORE
  | VAL
  | VIRTUAL
  | WHEN
  | WHILE
  | WITH

type error =
  | Illegal_character of char
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_string_in_comment
;;

exception Error of error * int * int

(* The table of keywords *)

let keyword_table =
  create_hashtable 149 [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "lazy", LAZY;
    "let", LET;
    "match", MATCH;
    "method", METHOD;
    "module", MODULE;
    "mutable", MUTABLE;
    "new", NEW;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
    "parser", PARSER;
    "private", PRIVATE;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lor", INFIXOP3("lor");
    "lxor", INFIXOP3("lxor");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr")
]

(* To buffer string literals *)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  Char.chr(c land 0xFF)

(* To store the position of the beginning of a string and comment *)
let string_start_pos = ref 0;;
let comment_start_pos = ref [];;

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Unterminated_comment ->
      fprintf ppf "Comment not terminated"
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment ->
      fprintf ppf "This comment contains an unterminated string literal"
;;

}

let blank = [' ' '\010' '\013' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let symbolchar2 =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' '<' '=' '>' '?' '@' '^' '|' '~']
(*  ['!' '$' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'] *)
let decimal_literal = ['0'-'9']+
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let oct_literal = '0' ['o' 'O'] ['0'-'7']+
let bin_literal = '0' ['b' 'B'] ['0'-'1']+
let float_literal =
  ['0'-'9']+ ('.' ['0'-'9']* )? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

rule token = parse
    blank +
      { token lexbuf }
  | "_"
      { UNDERSCORE }
  | lowercase identchar * ':' [ ^ ':' '=' '>']
      { let s = Lexing.lexeme lexbuf in
        lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1;
        lexbuf.lex_curr_p <-
          {lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum - 1};
        LABEL (String.sub s 0 (String.length s - 2)) }
(*
  | lowercase identchar * ':'
      { let s = Lexing.lexeme lexbuf in
        LABEL (String.sub s 0 (String.length s - 1)) }
  | '%' lowercase identchar *
*)
  | ':' lowercase identchar *
      { let s = Lexing.lexeme lexbuf in
        let l = String.length s - 1 in
        LABELID (String.sub s 1 l) }
  | lowercase identchar *
      { let s = Lexing.lexeme lexbuf in
          try
            Hashtbl.find keyword_table s
          with Not_found ->
            LIDENT s }
  | uppercase identchar *
      { UIDENT(Lexing.lexeme lexbuf) }       (* No capitalized keywords *)
  | decimal_literal | hex_literal | oct_literal | bin_literal
      { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | float_literal
      { FLOAT (Lexing.lexeme lexbuf) }
  | "\""
      { reset_string_buffer();
        let string_start = Lexing.lexeme_start lexbuf in
        string_start_pos := string_start;
        string lexbuf;
        lexbuf.Lexing.lex_start_pos <-
          string_start - lexbuf.Lexing.lex_abs_pos;
        STRING (get_stored_string()) }
  | "'" [^ '\\' '\''] "'"
      { CHAR(Lexing.lexeme_char lexbuf 1) }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHAR(char_for_decimal_code lexbuf 2) }
  | "(*"
      { comment_start_pos := [Lexing.lexeme_start lexbuf];
        comment lexbuf;
        token lexbuf }
  | "(*)"
      { let loc = { Location.loc_start = Lexing.lexeme_start_p lexbuf;
                    Location.loc_end = Lexing.lexeme_end_p lexbuf;
                    Location.loc_ghost = false }
        in
        Location.prerr_warning loc (Warnings.Comment_start);
        comment_start_pos := [Lexing.lexeme_start lexbuf];
        comment lexbuf;
        token lexbuf
      }
  | "*)"
      { let loc = { Location.loc_start = Lexing.lexeme_start_p lexbuf;
                    Location.loc_end = Lexing.lexeme_end_p lexbuf;
                    Location.loc_ghost = false }
        in
        Location.prerr_warning loc Warnings.Comment_not_end;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        STAR
      }
  | "#" [' ' '\t']* ['0'-'9']+ [^ '\n' '\r'] * ('\n' | '\r' | "\r\n")
      (* # linenum ...  *)
      { token lexbuf }
  | "#"  { SHARP }
  | "&"  { AMPERSAND }
  | "&&" { AMPERAMPER }
  | "`"  { BACKQUOTE }
  | "'"  { QUOTE }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "*"  { STAR }
  | ","  { COMMA }
  | "?"  { QUESTION }
  | "??" { QUESTION2 }
  | "->" { MINUSGREATER }
  | "."  { DOT }
  | ".." { DOTDOT }
  | ":"  { COLON }
  | "::" { COLONCOLON }
  | ":=" { COLONEQUAL }
  | ":>" { COLONGREATER }
  | ";"  { SEMI }
  | ";;" { SEMISEMI }
  | "<"  { LESS }
  | "<-" { LESSMINUS }
  | "="  { EQUAL }
  | "["  { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "[<" { LBRACKETLESS }
  | "]"  { RBRACKET }
  | "{"  { LBRACE }
  | "{<" { LBRACELESS }
  | "|"  { BAR }
  | "||" { BARBAR }
  | "|]" { BARRBRACKET }
  | ">"  { GREATER }
  | ">]" { GREATERRBRACKET }
  | "}"  { RBRACE }
  | ">}" { GREATERRBRACE }

  | "!=" { INFIXOP0 "!=" }
  | "-"  { SUBTRACTIVE "-" }
  | "-." { SUBTRACTIVE "-." }

  | ['!' '~'] symbolchar *
            { PREFIXOP(Lexing.lexeme lexbuf) }
  | '?' symbolchar2 *
            { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['=' '<' '>' '|' '&' '$'] symbolchar *
            { INFIXOP0(Lexing.lexeme lexbuf) }
  | ['@' '^'] symbolchar *
            { INFIXOP1(Lexing.lexeme lexbuf) }
  | ['+' '-'] symbolchar *
            { INFIXOP2(Lexing.lexeme lexbuf) }
  | "**" symbolchar *
            { INFIXOP4(Lexing.lexeme lexbuf) }
  | ['*' '/' '%'] symbolchar *
            { INFIXOP3(Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _
      { raise (Error(Illegal_character ((Lexing.lexeme lexbuf).[0]),
                     Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }

and comment = parse
    "(*"
      { comment_start_pos := Lexing.lexeme_start lexbuf :: !comment_start_pos;
        comment lexbuf;
      }
  | "*)"
      { match !comment_start_pos with
        | [] -> assert false
        | [x] -> ()
        | _ :: l -> comment_start_pos := l;
                    comment lexbuf;
       }
  | "\""
      { reset_string_buffer();
        string_start_pos := Lexing.lexeme_start lexbuf;
        begin try string lexbuf
        with Error (Unterminated_string, _, _) ->
          let st = List.hd !comment_start_pos in
          raise (Error (Unterminated_string_in_comment, st, st + 2))
        end;
        string_buff := initial_string_buffer;
        comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'" [^ '\\' '\''] "'"
      { comment lexbuf }
  | "'\\" ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  | eof
      { let st = List.hd !comment_start_pos in
        raise (Error (Unterminated_comment, st, st + 2));
      }
  | _
      { comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof
      { raise (Error (Unterminated_string,
                      !string_start_pos, !string_start_pos+1)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }
