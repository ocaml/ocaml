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

(* The lexical analyzer for lexer definitions. Bootstrapped! *)

{
open Syntax
open Parser

(* Auxiliaries for the lexical analyzer *)

exception Lexical_error of string * string * int * int

let string_buff = Buffer.create 256

let reset_string_buffer () = Buffer.clear string_buff

let store_string_char c = Buffer.add_char string_buff c
let store_string_uchar u = Buffer.add_utf_8_uchar string_buff u
let store_string_chars s = Buffer.add_string string_buff s

let get_stored_string () = Buffer.contents string_buff

let char_for_backslash = function
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let raise_lexical_error lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  raise (Lexical_error (msg,
                        p.Lexing.pos_fname,
                        p.Lexing.pos_lnum,
                        p.Lexing.pos_cnum - p.Lexing.pos_bol + 1))

let handle_lexical_error fn arg lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line = p.Lexing.pos_lnum
  and column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1
  and file = p.Lexing.pos_fname
  in
  try
    fn arg lexbuf
  with Lexical_error (msg, "", 0, 0) ->
    raise(Lexical_error(msg, file, line, column))

let warning lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  Printf.eprintf "ocamllex warning:\nFile \"%s\", line %d, character %d: %s.\n"
    p.Lexing.pos_fname p.Lexing.pos_lnum
    (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1) msg;
  flush stderr

let hex_digit_value d =
  let d = Char.code d in
  if d >= 97 then d - 87 else
  if d >= 65 then d - 55 else
  d - 48

let decimal_code c d u =
  100 * (Char.code c - 48) + 10 * (Char.code d - 48) + (Char.code u - 48)

let hexadecimal_code s =
  let rec loop acc i =
    if i < String.length s then
      let value = hex_digit_value s.[i] in
      loop (16 * acc + value) (i + 1)
    else acc in
  loop 0 0

let char_for_octal_code c d u =
  let c = 64 * (Char.code c - 48) +
           8 * (Char.code d - 48) +
               (Char.code u - 48) in
  Char.chr c

let char_for_hexadecimal_code d u =
  Char.chr (16 * (hex_digit_value d) + (hex_digit_value u))

let incr_loc lexbuf delta =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum - delta;
  }

let update_loc lexbuf opt_file line =
  let pos = lexbuf.Lexing.lex_curr_p in
  let new_file = match opt_file with
                 | None -> pos.Lexing.pos_fname
                 | Some f -> f
  in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_fname = new_file;
    Lexing.pos_lnum = line;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }

type string_context = Pattern | Action | Comment
}

let identstart =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255']
let identbody =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let backslash_escapes =
  ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

let lowercase = ['a'-'z' '_']
let ident = identstart identbody*
let extattrident = ident ('.' ident)*
let blank = [' ' '\009' '\012']

let uppercase = ['A'-'Z']
let ocaml_identstart = lowercase | uppercase
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let utf8 = ['\192'-'\255'] ['\128'-'\191']*
let identstart_ext = ocaml_identstart | utf8
let identchar_ext = identchar | utf8
let ocaml_ident = identstart_ext identchar_ext*


rule main = parse
    [' ' '\013' '\009' '\012' ] +
    { main lexbuf }
  | '\010'
    { incr_loc lexbuf 0;
      main lexbuf }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
    ('\"' ([^ '\010' '\013' '\"']* as name) '\"')?
    [^ '\010' '\013']* '\013'* '\010'
    { update_loc lexbuf name (int_of_string num);
      main lexbuf
    }
  | "(*"
    { handle_lexical_error comment 0 lexbuf;
      main lexbuf }
  | '_' { Tunderscore }
  | ident
    { match Lexing.lexeme lexbuf with
        "rule" -> Trule
      | "parse" -> Tparse
      | "shortest" -> Tparse_shortest
      | "and" -> Tand
      | "eof" -> Teof
      | "let" -> Tlet
      | "as"  -> Tas
      | "refill" -> Trefill
      | s -> Tident s }
  | '"'
    { reset_string_buffer();
      handle_lexical_error string Pattern lexbuf;
      Tstring(get_stored_string()) }
(* note: ''' is a valid character literal (by contrast with the compiler) *)
  | "'" [^ '\\'] "'"
    { Tchar(Char.code(Lexing.lexeme_char lexbuf 1)) }
  | "'" '\\' backslash_escapes "'"
    { Tchar(Char.code(char_for_backslash (Lexing.lexeme_char lexbuf 2))) }
  | "'" '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9'] as u)"'"
    { let v = decimal_code c d u in
      if v > 255 then
        raise_lexical_error lexbuf
          (Printf.sprintf "illegal escape sequence \\%c%c%c" c d u)
      else
        Tchar v }
  | "'" '\\' 'o' (['0'-'3'] as c) (['0'-'7'] as d) (['0'-'7'] as u) "'"
    { Tchar(Char.code(char_for_octal_code c d u)) }
  | "'" '\\' 'x'
       (['0'-'9' 'a'-'f' 'A'-'F'] as d) (['0'-'9' 'a'-'f' 'A'-'F'] as u) "'"
       { Tchar(Char.code(char_for_hexadecimal_code d u)) }
  | "'" '\\' (_ as c)
    { raise_lexical_error lexbuf
        (Printf.sprintf "illegal escape sequence \\%c" c)
    }
  | '{'
    { let p = Lexing.lexeme_end_p lexbuf in
      let f = p.Lexing.pos_fname in
      let n1 = p.Lexing.pos_cnum
      and l1 = p.Lexing.pos_lnum
      and s1 = p.Lexing.pos_bol in
      let n2 = handle_lexical_error action [] lexbuf in
      Taction({loc_file = f; start_pos = n1; end_pos = n2;
               start_line = l1; start_col = n1 - s1}) }
  | '='  { Tequal }
  | '|'  { Tor }
  | '['  { Tlbracket }
  | ']'  { Trbracket }
  | '*'  { Tstar }
  | '?'  { Tmaybe }
  | '+'  { Tplus }
  | '('  { Tlparen }
  | ')'  { Trparen }
  | '^'  { Tcaret }
  | '-'  { Tdash }
  | '#'  { Thash }
  | eof  { Tend }
  | _
    { raise_lexical_error lexbuf
        ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf))
    }


(* String parsing comes from the compiler lexer *)
and string in_pattern = parse
    '"'
    { () }
  | '\\' ('\013'* '\010') ([' ' '\009'] * as spaces)
    { incr_loc lexbuf (String.length spaces);
      string in_pattern lexbuf }
  | '\\' (backslash_escapes as c)
    { store_string_char(char_for_backslash c);
      string in_pattern lexbuf }
  | '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9']  as u)
    { let v = decimal_code c d u in
      if in_pattern = Pattern then
        if v > 255 then
          raise_lexical_error lexbuf
            (Printf.sprintf
              "illegal backslash escape in string: '\\%c%c%c'" c d u)
        else
          store_string_char (Char.chr v);
      string in_pattern lexbuf }
  | '\\' 'o' (['0'-'3'] as c) (['0'-'7'] as d) (['0'-'7'] as u)
    { store_string_char (char_for_octal_code c d u);
      string in_pattern lexbuf }
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as d) (['0'-'9' 'a'-'f' 'A'-'F'] as u)
    { store_string_char (char_for_hexadecimal_code d u) ;
      string in_pattern lexbuf }
  | '\\' 'u' '{' (['0'-'9' 'a'-'f' 'A'-'F'] + as s) '}'
    { let v = hexadecimal_code s in
      if in_pattern = Pattern then
        if not (Uchar.is_valid v) then
          raise_lexical_error lexbuf
            (Printf.sprintf
              "illegal uchar escape in string: '\\u{%s}'" s)
        else
          store_string_uchar (Uchar.unsafe_of_int v);
      string in_pattern lexbuf }
  | '\\' (_ as c)
    { if in_pattern = Pattern then
        warning lexbuf
          (Printf.sprintf "illegal backslash escape in string: '\\%c'" c) ;
      store_string_char '\\' ;
      store_string_char c ;
      string in_pattern lexbuf }
  | eof
    { raise(Lexical_error("unterminated string", "", 0, 0)) }
  | '\013'* '\010' as s
    { if in_pattern <> Comment then
        warning lexbuf (Printf.sprintf "unescaped newline in string") ;
      store_string_chars s;
      incr_loc lexbuf 0;
      string in_pattern lexbuf }
  | _ as c
    { store_string_char c;
      string in_pattern lexbuf }

and quoted_string delim = parse
  | '\013'* '\010'
    { incr_loc lexbuf 0;
      quoted_string delim lexbuf }
  | eof
    { raise (Lexical_error ("unterminated string", "", 0, 0)) }
  | '|' (lowercase* as delim') '}'
    { if delim <> delim' then
      quoted_string delim lexbuf }
  | _
    { quoted_string delim lexbuf }

(*
   Lexers comment and action are quite similar.
   They should lex strings, quoted strings and characters,
   in order not to be confused by what is inside them.
*)

and comment depth = parse
    "(*" { comment (depth + 1) lexbuf }
  | "*)" { if depth > 0 then comment (depth - 1) lexbuf }
  | '"'
    { reset_string_buffer();
      string Comment lexbuf;
      reset_string_buffer();
      comment depth lexbuf }
  | '{' ('%' '%'? extattrident blank*)? (lowercase* as delim) "|"
    { quoted_string delim lexbuf;
      comment depth lexbuf }
  | "'"
    { skip_char lexbuf ;
      comment depth lexbuf }
  | eof
    { raise(Lexical_error("unterminated comment", "", 0, 0)) }
  | '\010'
    { incr_loc lexbuf 0;
      comment depth lexbuf }
  | ocaml_ident
    { comment depth lexbuf }
  | _
    { comment depth lexbuf }

and action stk = parse
  | '(' { action ('(' :: stk) lexbuf }
  | '{' { action ('{' :: stk) lexbuf }
  | ')'
    { match stk with
      | '(' :: stk' -> action stk' lexbuf
      | _ -> raise_lexical_error lexbuf "Unmatched ) in action" }
  | '}'
    { match stk with
      | [] -> Lexing.lexeme_start lexbuf
      | '{' :: stk' -> action stk' lexbuf
      | _ -> raise_lexical_error lexbuf "Unmatched } in action" }
  | '"'
    { reset_string_buffer();
      handle_lexical_error string Action lexbuf;
      reset_string_buffer();
      action stk lexbuf }
  | '{' ('%' '%'? extattrident blank*)? (lowercase* as delim) "|"
    { quoted_string delim lexbuf;
      action stk lexbuf }
  | "'"
    { skip_char lexbuf ;
      action stk lexbuf }
  | "(*"
    { comment 0 lexbuf;
      action stk lexbuf }
  | eof
    { raise (Lexical_error("unterminated action", "", 0, 0)) }
  | '\010'
    { incr_loc lexbuf 0;
      action stk lexbuf }
  | ocaml_ident
    { action stk lexbuf }
  | _
    { action stk lexbuf }

and skip_char = parse
  | '\\'? ('\013'* '\010') "'"
     { incr_loc lexbuf 1;
     }
  | [^ '\\' '\'' '\010' '\013'] "'" (* regular character *)
(* one character and numeric escape sequences *)
  | '\\' _ "'"
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
  | '\\' 'o' ['0'-'7'] ['0'-'7'] ['0'-'7'] "'"
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
     {()}
(* Perilous *)
  | "" {()}
