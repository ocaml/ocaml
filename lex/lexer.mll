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

exception Lexical_error of string

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

}

rule main = parse
    [' ' '\010' '\013' '\009' '\012' ] + 
    { main lexbuf }
  | "(*" 
    { comment_depth := 1;
      comment lexbuf;
      main lexbuf }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9'] *
    { match Lexing.lexeme lexbuf with
        "rule" -> Trule
      | "parse" -> Tparse
      | "and" -> Tand
      | "eof" -> Teof
      | s -> Tident s }
  | '"' 
    { reset_string_buffer();
      string lexbuf;
      Tstring(get_stored_string()) }
  | "'" [^ '\\'] "'" 
    { Tchar(Lexing.lexeme_char lexbuf 1) }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'" 
    { Tchar(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'" 
    { Tchar(char_for_decimal_code lexbuf 2) }
  | '{' 
    { let n1 = Lexing.lexeme_end lexbuf in
        brace_depth := 1;
        let n2 = action lexbuf in
          Taction(Location(n1, n2)) }
  | '='  { Tequal }
  | '|'  { Tor }
  | '_'  { Tunderscore }
  | "eof"  { Teof }
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
             ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf))) }

and action = parse
    '{' 
    { incr brace_depth;
      action lexbuf }
  | '}' 
    { decr brace_depth;
      if !brace_depth == 0 then Lexing.lexeme_start lexbuf else action lexbuf }
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
    { raise (Lexical_error "unterminated action") }
  | _ 
    { action lexbuf }
      
and string = parse
    '"' 
    { () }
  | '\\' [' ' '\010' '\013' '\009' '\026' '\012'] +
    { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r'] 
    { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
      string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] 
    { store_string_char(char_for_decimal_code lexbuf 1);
      string lexbuf }
  | eof 
    { raise(Lexical_error "unterminated string") }
  | _ 
    { store_string_char(Lexing.lexeme_char lexbuf 0);
      string lexbuf }

and comment = parse
    "(*" 
    { incr comment_depth; comment lexbuf }
  | "*)" 
    { decr comment_depth;
      if !comment_depth == 0 then () else comment lexbuf }
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
    { raise(Lexical_error "unterminated comment") }
  | _ 
    { comment lexbuf }
