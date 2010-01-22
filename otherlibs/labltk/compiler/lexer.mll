(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of Objective Caml            *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file ../LICENSE.                                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

{
open StdLabels
open Lexing
open Parser
open Support

exception Lexical_error of string
let current_line = ref 1


(* The table of keywords *)

let keyword_table = (Hashtbl.create 149 : (string, token) Hashtbl.t)

let _ = List.iter
  ~f:(fun (str,tok) -> Hashtbl.add keyword_table str tok)
  [
  "int", TYINT;
  "float", TYFLOAT;
  "bool", TYBOOL;
  "char", TYCHAR;
  "string", TYSTRING;
  "list", LIST;
  "as", AS;
  "variant", VARIANT;
  "widget", WIDGET;
  "option", OPTION;
  "type", TYPE;
  "subtype", SUBTYPE;
  "function", FUNCTION;
  "module", MODULE;
  "external", EXTERNAL;
  "sequence", SEQUENCE;
  "unsafe", UNSAFE
]


(* To buffer string literals *)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0;
  ()

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit ~src:(!string_buff) ~src_pos:0 ~dst:new_buff ~dst_pos:0
                  ~len:(String.length (!string_buff));
      string_buff := new_buff
  end;
  String.set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) ~pos:0 ~len:(!string_index) in
    string_buff := initial_string_buffer;
    s
(* To translate escape sequences *)

let char_for_backslash = function
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  Char.chr(100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
               10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                    (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48))

let saved_string_start = ref 0

}

rule main = parse
    '\010' { incr current_line; main lexbuf }
  | [' ' '\013' '\009' '\026' '\012'] +
      { main lexbuf }
  | ['A'-'Z' 'a'-'z' '\192'-'\214' '\216'-'\246' '\248'-'\255' ]
    ( '_' ? ['A'-'Z' 'a'-'z' '\192'-'\214' '\216'-'\246' '\248'-'\255' (*'*) '0'-'9' ] ) *
      { let s = Lexing.lexeme lexbuf in
          try
            Hashtbl.find keyword_table s
          with Not_found ->
            IDENT s }

  | "\""
      { reset_string_buffer();
        (* Start of token is start of string. *)
        saved_string_start := lexbuf.lex_start_pos;
        string lexbuf;
        lexbuf.lex_start_pos <- !saved_string_start;
        STRING (get_stored_string()) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | ":" {COLON}
  | "?" {QUESTION}
  | "/" {SLASH}
  | "%" { comment lexbuf; main lexbuf }
  | "##line" { line lexbuf; main lexbuf }
  | eof { EOF }
  | _
      { raise (Lexical_error("illegal character")) }


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
      { raise (Lexical_error("string not terminated")) }
  | '\010'
      { incr current_line;
        store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and comment = parse
   '\010' { incr current_line }
 | eof  { () }
 | _ { comment lexbuf }

and linenum = parse
 | ['0'-'9']+ {
            let next_line = int_of_string (Lexing.lexeme lexbuf) in
            current_line := next_line - 1
          }
 | _ { raise (Lexical_error("illegal ##line directive: no line number"))}

and line = parse
 | [' ' '\t']* { linenum lexbuf }
