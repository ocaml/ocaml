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

(* The lexical analyzer for lexer definitions. Bootstrapped! *)

{
open Syntax
open Parser

(* Auxiliaries for the lexical analyzer *)

let brace_depth = ref 0
and comment_depth = ref 0

exception Lexical_error of string * int * int

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length !string_buff then begin
    let new_buff = String.create (String.length !string_buff * 2) in
    String.blit !string_buff 0 new_buff 0 (String.length !string_buff);
    string_buff := new_buff
  end;
  !string_buff.[!string_index] <- c;
  incr string_index

let get_stored_string () =
  String.sub !string_buff 0 !string_index

let char_for_backslash = function
    'n' -> '\n'
  | 't' -> '\t'
  | 'b' -> '\b'
  | 'r' -> '\r'
  | c   -> c

let char_for_decimal_code lexbuf i =
  Char.chr(100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
               10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                    (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48))

let line_num = ref 1
let line_start_pos = ref 0

let handle_lexical_error fn lexbuf =
  let line = !line_num
  and column = Lexing.lexeme_start lexbuf - !line_start_pos in
  try
    fn lexbuf
  with Lexical_error(msg, _, _) ->
    raise(Lexical_error(msg, line, column))
}

rule main = parse
    [' ' '\013' '\009' '\012' ] + 
    { main lexbuf }
  | '\010'
    { line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      main lexbuf }
  | "(*" 
    { comment_depth := 1;
      handle_lexical_error comment lexbuf;
      main lexbuf }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9'] *
    { match Lexing.lexeme lexbuf with
        "rule" -> Trule
      | "parse" -> Tparse
      | "and" -> Tand
      | "eof" -> Teof
      | "let" -> Tlet
      | s -> Tident s }
  | '"' 
    { reset_string_buffer();
      handle_lexical_error string lexbuf;
      Tstring(get_stored_string()) }
  | "'" [^ '\\'] "'" 
    { Tchar(Char.code(Lexing.lexeme_char lexbuf 1)) }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'" 
    { Tchar(Char.code(char_for_backslash (Lexing.lexeme_char lexbuf 2))) }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'" 
    { Tchar(Char.code(char_for_decimal_code lexbuf 2)) }
  | '{' 
    { let n1 = Lexing.lexeme_end lexbuf
      and l1 = !line_num
      and s1 = !line_start_pos in
      brace_depth := 1;
      let n2 = handle_lexical_error action lexbuf in
      Taction({start_pos = n1; end_pos = n2;
               start_line = l1; start_col = n1 - s1}) }
  | '='  { Tequal }
  | '|'  { Tor }
  | '_'  { Tunderscore }
  | '['  { Tlbracket }
  | ']'  { Trbracket }
  | '*'  { Tstar }
  | '?'  { Tmaybe }
  | '+'  { Tplus }
  | '('  { Tlparen }
  | ')'  { Trparen }
  | '^'  { Tcaret }
  | '-'  { Tdash }
  | eof  { Tend }
  | _
    { raise(Lexical_error
             ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf),
              !line_num, Lexing.lexeme_start lexbuf - !line_start_pos)) }

and action = parse
    '{' 
    { incr brace_depth;
      action lexbuf }
  | '}' 
    { decr brace_depth;
      if !brace_depth = 0 then Lexing.lexeme_start lexbuf else action lexbuf }
  | '"' 
    { reset_string_buffer();
      string lexbuf;
      reset_string_buffer();
      action lexbuf }
  | "'" [^ '\\'] "'" 
    { action lexbuf }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'" 
    { action lexbuf }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'" 
    { action lexbuf }
  | "(*" 
    { comment_depth := 1;
      comment lexbuf;
      action lexbuf }
  | eof 
    { raise (Lexical_error("unterminated action", 0, 0)) }
  | '\010'
    { line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      action lexbuf }
  | _ 
    { action lexbuf }
      
and string = parse
    '"' 
    { () }
  | '\\' [' ' '\013' '\009' '\012'] * '\010' [' ' '\013' '\009' '\012'] *
    { line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r'] 
    { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
      string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] 
    { store_string_char(char_for_decimal_code lexbuf 1);
      string lexbuf }
  | eof 
    { raise(Lexical_error("unterminated string", 0, 0)) }
  | '\010'
    { store_string_char '\010';
      line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      string lexbuf }
  | _ 
    { store_string_char(Lexing.lexeme_char lexbuf 0);
      string lexbuf }

and comment = parse
    "(*" 
    { incr comment_depth; comment lexbuf }
  | "*)" 
    { decr comment_depth;
      if !comment_depth = 0 then () else comment lexbuf }
  | '"' 
    { reset_string_buffer();
      string lexbuf;
      reset_string_buffer();
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
    { raise(Lexical_error("unterminated comment", 0, 0)) }
  | '\010'
    { line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      comment lexbuf }
  | _ 
    { comment lexbuf }
