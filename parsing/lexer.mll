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
open Parser

type error =
  | Illegal_character of char
  | Illegal_escape of string * string option
  | Reserved_sequence of string * string option
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Empty_character_literal
  | Keyword_as_label of string
  | Invalid_literal of string
  | Invalid_directive of string * string option

exception Error of error * Location.t

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
    "nonrec", NONREC;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
(*  "parser", PARSER; *)
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

    "lor", INFIXOP3("lor"); (* Should be INFIXOP2 *)
    "lxor", INFIXOP3("lxor"); (* Should be INFIXOP2 *)
    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr")
]

(* To buffer string literals *)

let string_buffer = Buffer.create 256
let reset_string_buffer () = Buffer.reset string_buffer
let get_stored_string () = Buffer.contents string_buffer

let store_string_char c = Buffer.add_char string_buffer c
let store_string_utf_8_uchar u = Buffer.add_utf_8_uchar string_buffer u
let store_string s = Buffer.add_string string_buffer s
let store_lexeme lexbuf = store_string (Lexing.lexeme lexbuf)

(* To store the position of the beginning of a string and comment *)
let string_start_loc = ref Location.none
let comment_start_loc = ref []
let in_comment () = !comment_start_loc <> []
let is_in_string = ref false
let in_string () = !is_in_string
let print_warnings = ref true

(* Escaped chars are interpreted in strings unless they are in comments. *)
let store_escaped_char lexbuf c =
  if in_comment () then store_lexeme lexbuf else store_string_char c

let store_escaped_uchar lexbuf u =
  if in_comment () then store_lexeme lexbuf else store_string_utf_8_uchar u

let compute_quoted_string_idloc {Location.loc_start = orig_loc } shift id =
  let id_start_pos = orig_loc.Lexing.pos_cnum + shift in
  let loc_start =
    Lexing.{orig_loc with pos_cnum = id_start_pos }
  in
  let loc_end =
    Lexing.{orig_loc with pos_cnum = id_start_pos + String.length id}
  in
  {Location. loc_start ; loc_end ; loc_ghost = false }

let wrap_string_lexer f lexbuf =
  let loc_start = lexbuf.lex_curr_p in
  reset_string_buffer();
  is_in_string := true;
  let string_start = lexbuf.lex_start_p in
  string_start_loc := Location.curr lexbuf;
  let loc_end = f lexbuf in
  is_in_string := false;
  lexbuf.lex_start_p <- string_start;
  let loc = Location.{loc_ghost= false; loc_start; loc_end} in
  get_stored_string (), loc

let wrap_comment_lexer comment lexbuf =
  let start_loc = Location.curr lexbuf  in
  comment_start_loc := [start_loc];
  reset_string_buffer ();
  let end_loc = comment lexbuf in
  let s = get_stored_string () in
  reset_string_buffer ();
  s,
  { start_loc with Location.loc_end = end_loc.Location.loc_end }

let error lexbuf e = raise (Error(e, Location.curr lexbuf))
let error_loc loc e = raise (Error(e, loc))

(* to translate escape sequences *)

let digit_value c =
  match c with
  | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> assert false

let num_value lexbuf ~base ~first ~last =
  let c = ref 0 in
  for i = first to last do
    let v = digit_value (Lexing.lexeme_char lexbuf i) in
    assert(v < base);
    c := (base * !c) + v
  done;
  !c

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let illegal_escape lexbuf reason =
  let error = Illegal_escape (Lexing.lexeme lexbuf, Some reason) in
  raise (Error (error, Location.curr lexbuf))

let char_for_decimal_code lexbuf i =
  let c = num_value lexbuf ~base:10 ~first:i ~last:(i+2) in
  if (c < 0 || c > 255) then
    if in_comment ()
    then 'x'
    else
      illegal_escape lexbuf
        (Printf.sprintf
          "%d is outside the range of legal characters (0-255)." c)
  else Char.chr c

let char_for_octal_code lexbuf i =
  let c = num_value lexbuf ~base:8 ~first:i ~last:(i+2) in
  if (c < 0 || c > 255) then
    if in_comment ()
    then 'x'
    else
      illegal_escape lexbuf
        (Printf.sprintf
          "o%o (=%d) is outside the range of legal characters (0-255)." c c)
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  Char.chr (num_value lexbuf ~base:16 ~first:i ~last:(i+1))

let uchar_for_uchar_escape lexbuf =
  let len = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
  let first = 3 (* skip opening \u{ *) in
  let last = len - 2 (* skip closing } *) in
  let digit_count = last - first + 1 in
  match digit_count > 6 with
  | true ->
      illegal_escape lexbuf
        "too many digits, expected 1 to 6 hexadecimal digits"
  | false ->
      let cp = num_value lexbuf ~base:16 ~first ~last in
      if Uchar.is_valid cp then Uchar.unsafe_of_int cp else
      illegal_escape lexbuf
        (Printf.sprintf "%X is not a Unicode scalar value" cp)

let is_keyword name = Hashtbl.mem keyword_table name

let check_label_name lexbuf name =
  if is_keyword name then error lexbuf (Keyword_as_label name)

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

let preprocessor = ref None

let escaped_newlines = ref false

(* Warn about Latin-1 characters used in idents *)

let warn_latin1 lexbuf =
  Location.deprecated
    (Location.curr lexbuf)
    "ISO-Latin1 characters in identifiers"

let handle_docstrings = ref true
let comment_list = ref []

let add_comment com =
  comment_list := com :: !comment_list

let add_docstring_comment ds =
  let com =
    ("*" ^ Docstrings.docstring_body ds, Docstrings.docstring_loc ds)
  in
    add_comment com

let comments () = List.rev !comment_list

(* Error report *)

open Format

let prepare_error loc = function
  | Illegal_character c ->
      Location.errorf ~loc "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape (s, explanation) ->
      Location.errorf ~loc
        "Illegal backslash escape in string or character (%s)%t" s
        (fun ppf -> match explanation with
           | None -> ()
           | Some expl -> fprintf ppf ": %s" expl)
  | Reserved_sequence (s, explanation) ->
      Location.errorf ~loc
        "Reserved character sequence: %s%t" s
        (fun ppf -> match explanation with
           | None -> ()
           | Some expl -> fprintf ppf " %s" expl)
  | Unterminated_comment _ ->
      Location.errorf ~loc "Comment not terminated"
  | Unterminated_string ->
      Location.errorf ~loc "String literal not terminated"
  | Unterminated_string_in_comment (_, literal_loc) ->
      Location.errorf ~loc
        "This comment contains an unterminated string literal"
        ~sub:[Location.msg ~loc:literal_loc "String literal begins here"]
  | Empty_character_literal ->
      let msg = "Illegal empty character literal ''" in
      let sub =
        [Location.msg
           "Hint: Did you mean ' ' or a type variable 'a?"] in
      Location.error ~loc ~sub msg
  | Keyword_as_label kwd ->
      Location.errorf ~loc
        "`%s' is a keyword, it cannot be used as label name" kwd
  | Invalid_literal s ->
      Location.errorf ~loc "Invalid literal %s" s
  | Invalid_directive (dir, explanation) ->
      Location.errorf ~loc "Invalid lexer directive %S%t" dir
        (fun ppf -> match explanation with
           | None -> ()
           | Some expl -> fprintf ppf ": %s" expl)

let () =
  Location.register_error_of_exn
    (function
      | Error (err, loc) ->
          Some (prepare_error loc err)
      | _ ->
          None
    )

}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let lowercase_latin1 = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase_latin1 = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar_latin1 =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
(* This should be kept in sync with the [is_identchar] function in [env.ml] *)

let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let dotsymbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '/' ':' '=' '>' '?' '@' '^' '|']
let symbolchar_or_hash =
  symbolchar | '#'
let kwdopchar =
  ['$' '&' '*' '+' '-' '/' '<' '=' '>' '@' '^' '|']

let ident = (lowercase | uppercase) identchar*
let extattrident = ident ('.' ident)*

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let literal_modifier = ['G'-'Z' 'g'-'z']

rule token = parse
  | ('\\' as bs) newline {
      if not !escaped_newlines then error lexbuf (Illegal_character bs);
      update_loc lexbuf None 1 false 0;
      token lexbuf }
  | newline
      { update_loc lexbuf None 1 false 0;
        EOL }
  | blank +
      { token lexbuf }
  | "_"
      { UNDERSCORE }
  | "~"
      { TILDE }
  | ".~"
      { error lexbuf
          (Reserved_sequence (".~", Some "is reserved for use in MetaOCaml")) }
  | "~" (lowercase identchar * as name) ':'
      { check_label_name lexbuf name;
        LABEL name }
  | "~" (lowercase_latin1 identchar_latin1 * as name) ':'
      { warn_latin1 lexbuf;
        LABEL name }
  | "?"
      { QUESTION }
  | "?" (lowercase identchar * as name) ':'
      { check_label_name lexbuf name;
        OPTLABEL name }
  | "?" (lowercase_latin1 identchar_latin1 * as name) ':'
      { warn_latin1 lexbuf;
        OPTLABEL name }
  | lowercase identchar * as name
      { try Hashtbl.find keyword_table name
        with Not_found -> LIDENT name }
  | lowercase_latin1 identchar_latin1 * as name
      { warn_latin1 lexbuf; LIDENT name }
  | uppercase identchar * as name
      { UIDENT name } (* No capitalized keywords *)
  | uppercase_latin1 identchar_latin1 * as name
      { warn_latin1 lexbuf; UIDENT name }
  | int_literal as lit { INT (lit, None) }
  | (int_literal as lit) (literal_modifier as modif)
      { INT (lit, Some modif) }
  | float_literal | hex_float_literal as lit
      { FLOAT (lit, None) }
  | (float_literal | hex_float_literal as lit) (literal_modifier as modif)
      { FLOAT (lit, Some modif) }
  | (float_literal | hex_float_literal | int_literal) identchar+ as invalid
      { error lexbuf (Invalid_literal invalid) }
  | "\""
      { let s, loc = wrap_string_lexer string lexbuf in
        STRING (s, loc, None) }
  | "{" (lowercase* as delim) "|"
      { let s, loc = wrap_string_lexer (quoted_string delim) lexbuf in
        STRING (s, loc, Some delim) }
  | "{%" (extattrident as id) "|"
      { let orig_loc = Location.curr lexbuf in
        let s, loc = wrap_string_lexer (quoted_string "") lexbuf in
        let idloc = compute_quoted_string_idloc orig_loc 2 id in
        QUOTED_STRING_EXPR (id, idloc, s, loc, Some "") }
  | "{%" (extattrident as id) blank+ (lowercase* as delim) "|"
      { let orig_loc = Location.curr lexbuf in
        let s, loc = wrap_string_lexer (quoted_string delim) lexbuf in
        let idloc = compute_quoted_string_idloc orig_loc 2 id in
        QUOTED_STRING_EXPR (id, idloc, s, loc, Some delim) }
  | "{%%" (extattrident as id) "|"
      { let orig_loc = Location.curr lexbuf in
        let s, loc = wrap_string_lexer (quoted_string "") lexbuf in
        let idloc = compute_quoted_string_idloc orig_loc 3 id in
        QUOTED_STRING_ITEM (id, idloc, s, loc, Some "") }
  | "{%%" (extattrident as id) blank+ (lowercase* as delim) "|"
      { let orig_loc = Location.curr lexbuf in
        let s, loc = wrap_string_lexer (quoted_string delim) lexbuf in
        let idloc = compute_quoted_string_idloc orig_loc 3 id in
        QUOTED_STRING_ITEM (id, idloc, s, loc, Some delim) }
  | "\'" newline "\'"
      { update_loc lexbuf None 1 false 1;
        (* newline is ('\013'* '\010') *)
        CHAR '\n' }
  | "\'" ([^ '\\' '\'' '\010' '\013'] as c) "\'"
      { CHAR c }
  | "\'\\" (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as c) "\'"
      { CHAR (char_for_backslash c) }
  | "\'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "\'"
      { CHAR(char_for_decimal_code lexbuf 2) }
  | "\'\\" 'o' ['0'-'7'] ['0'-'7'] ['0'-'7'] "\'"
      { CHAR(char_for_octal_code lexbuf 3) }
  | "\'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "\'"
      { CHAR(char_for_hexadecimal_code lexbuf 3) }
  | "\'" ("\\" _ as esc)
      { error lexbuf (Illegal_escape (esc, None)) }
  | "\'\'"
      { error lexbuf Empty_character_literal }
  | "(*"
      { let s, loc = wrap_comment_lexer comment lexbuf in
        COMMENT (s, loc) }
  | "(**"
      { let s, loc = wrap_comment_lexer comment lexbuf in
        if !handle_docstrings then
          DOCSTRING (Docstrings.docstring s loc)
        else
          COMMENT ("*" ^ s, loc)
      }
  | "(**" (('*'+) as stars)
      { let s, loc =
          wrap_comment_lexer
            (fun lexbuf ->
               store_string ("*" ^ stars);
               comment lexbuf)
            lexbuf
        in
        COMMENT (s, loc) }
  | "(*)"
      { if !print_warnings then
          Location.prerr_warning (Location.curr lexbuf) Warnings.Comment_start;
        let s, loc = wrap_comment_lexer comment lexbuf in
        COMMENT (s, loc) }
  | "(*" (('*'*) as stars) "*)"
      { if !handle_docstrings && stars="" then
         (* (**) is an empty docstring *)
          DOCSTRING(Docstrings.docstring "" (Location.curr lexbuf))
        else
          COMMENT (stars, Location.curr lexbuf) }
  | "*)"
      { let loc = Location.curr lexbuf in
        Location.prerr_warning loc Warnings.Comment_not_end;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
        STAR
      }
  | "#"
      { let at_beginning_of_line pos = (pos.pos_cnum = pos.pos_bol) in
        if not (at_beginning_of_line lexbuf.lex_start_p)
        then HASH
        else try directive lexbuf with Failure _ -> HASH
      }
  | "&"  { AMPERSAND }
  | "&&" { AMPERAMPER }
  | "`"  { BACKQUOTE }
  | "\'" { QUOTE }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "*"  { STAR }
  | ","  { COMMA }
  | "->" { MINUSGREATER }
  | "."  { DOT }
  | ".." { DOTDOT }
  | "." (dotsymbolchar symbolchar* as op) { DOTOP op }
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
  | "[>" { LBRACKETGREATER }
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
  | "[@" { LBRACKETAT }
  | "[@@"  { LBRACKETATAT }
  | "[@@@" { LBRACKETATATAT }
  | "[%"   { LBRACKETPERCENT }
  | "[%%"  { LBRACKETPERCENTPERCENT }
  | "!"  { BANG }
  | "!=" { INFIXOP0 "!=" }
  | "+"  { PLUS }
  | "+." { PLUSDOT }
  | "+=" { PLUSEQ }
  | "-"  { MINUS }
  | "-." { MINUSDOT }

  | "!" symbolchar_or_hash + as op
            { PREFIXOP op }
  | ['~' '?'] symbolchar_or_hash + as op
            { PREFIXOP op }
  | ['=' '<' '>' '|' '&' '$'] symbolchar * as op
            { INFIXOP0 op }
  | ['@' '^'] symbolchar * as op
            { INFIXOP1 op }
  | ['+' '-'] symbolchar * as op
            { INFIXOP2 op }
  | "**" symbolchar * as op
            { INFIXOP4 op }
  | '%'     { PERCENT }
  | ['*' '/' '%'] symbolchar * as op
            { INFIXOP3 op }
  | '#' symbolchar_or_hash + as op
            { HASHOP op }
  | "let" kwdopchar dotsymbolchar * as op
            { LETOP op }
  | "and" kwdopchar dotsymbolchar * as op
            { ANDOP op }
  | eof { EOF }
  | (_ as illegal_char)
      { error lexbuf (Illegal_character illegal_char) }

and directive = parse
  | ([' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '\"' ] * as name) "\"") as directive)
        [^ '\010' '\013'] *
      {
        match int_of_string num with
        | exception _ ->
            (* PR#7165 *)
            let explanation = "line number out of range" in
            error lexbuf (Invalid_directive ("#" ^ directive, Some explanation))
        | line_num ->
           (* Documentation says that the line number should be
              positive, but we have never guarded against this and it
              might have useful hackish uses. *)
            update_loc lexbuf (Some name) (line_num - 1) true 0;
            token lexbuf
      }
and comment = parse
    "(*"
      { comment_start_loc := (Location.curr lexbuf) :: !comment_start_loc;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | "*)"
      { match !comment_start_loc with
        | [] -> assert false
        | [_] -> comment_start_loc := []; Location.curr lexbuf
        | _ :: l -> comment_start_loc := l;
                  store_lexeme lexbuf;
                  comment lexbuf
       }
  | "\""
      {
        string_start_loc := Location.curr lexbuf;
        store_string_char '\"';
        is_in_string := true;
        let _loc = try string lexbuf
        with Error (Unterminated_string, str_start) ->
          match !comment_start_loc with
          | [] -> assert false
          | loc :: _ ->
            let start = List.hd (List.rev !comment_start_loc) in
            comment_start_loc := [];
            error_loc loc (Unterminated_string_in_comment (start, str_start))
        in
        is_in_string := false;
        store_string_char '\"';
        comment lexbuf }
  | "{" ('%' '%'? extattrident blank*)? (lowercase* as delim) "|"
      {
        string_start_loc := Location.curr lexbuf;
        store_lexeme lexbuf;
        is_in_string := true;
        let _loc = try quoted_string delim lexbuf
        with Error (Unterminated_string, str_start) ->
          match !comment_start_loc with
          | [] -> assert false
          | loc :: _ ->
            let start = List.hd (List.rev !comment_start_loc) in
            comment_start_loc := [];
            error_loc loc (Unterminated_string_in_comment (start, str_start))
        in
        is_in_string := false;
        store_string_char '|';
        store_string delim;
        store_string_char '}';
        comment lexbuf }
  | "\'\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'" newline "\'"
      { update_loc lexbuf None 1 false 1;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | "\'" [^ '\\' '\'' '\010' '\013' ] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'\\" ['\\' '\"' '\'' 'n' 't' 'b' 'r' ' '] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'\\" 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | eof
      { match !comment_start_loc with
        | [] -> assert false
        | loc :: _ ->
          let start = List.hd (List.rev !comment_start_loc) in
          comment_start_loc := [];
          error_loc loc (Unterminated_comment start)
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | ident
      { store_lexeme lexbuf; comment lexbuf }
  | _
      { store_lexeme lexbuf; comment lexbuf }

and string = parse
    '\"'
      { lexbuf.lex_start_p }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        if in_comment () then store_lexeme lexbuf;
        string lexbuf
      }
  | '\\' (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as c)
      { store_escaped_char lexbuf (char_for_backslash c);
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_escaped_char lexbuf (char_for_decimal_code lexbuf 1);
         string lexbuf }
  | '\\' 'o' ['0'-'7'] ['0'-'7'] ['0'-'7']
      { store_escaped_char lexbuf (char_for_octal_code lexbuf 2);
         string lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { store_escaped_char lexbuf (char_for_hexadecimal_code lexbuf 2);
         string lexbuf }
  | '\\' 'u' '{' hex_digit+ '}'
        { store_escaped_uchar lexbuf (uchar_for_uchar_escape lexbuf);
          string lexbuf }
  | '\\' _
      { if not (in_comment ()) then begin
(*  Should be an error, but we are very lax.
          error lexbuf (Illegal_escape (Lexing.lexeme lexbuf, None))
*)
          let loc = Location.curr lexbuf in
          Location.prerr_warning loc Warnings.Illegal_backslash;
        end;
        store_lexeme lexbuf;
        string lexbuf
      }
  | newline
      { if not (in_comment ()) then
          Location.prerr_warning (Location.curr lexbuf) Warnings.Eol_in_string;
        update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        string lexbuf
      }
  | eof
      { is_in_string := false;
        error_loc !string_start_loc Unterminated_string }
  | (_ as c)
      { store_string_char c;
        string lexbuf }

and quoted_string delim = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        quoted_string delim lexbuf
      }
  | eof
      { is_in_string := false;
        error_loc !string_start_loc Unterminated_string }
  | "|" (lowercase* as edelim) "}"
      {
        if delim = edelim then lexbuf.lex_start_p
        else (store_lexeme lexbuf; quoted_string delim lexbuf)
      }
  | (_ as c)
      { store_string_char c;
        quoted_string delim lexbuf }

and skip_hash_bang = parse
  | "#!" [^ '\n']* '\n' [^ '\n']* "\n!#\n"
      { update_loc lexbuf None 3 false 0 }
  | "#!" [^ '\n']* '\n'
      { update_loc lexbuf None 1 false 0 }
  | "" { () }

{

  let token_with_comments lexbuf =
    match !preprocessor with
    | None -> token lexbuf
    | Some (_init, preprocess) -> preprocess token lexbuf

  type newline_state =
    | NoLine (* There have been no blank lines yet. *)
    | NewLine
        (* There have been no blank lines, and the previous
           token was a newline. *)
    | BlankLine (* There have been blank lines. *)

  type doc_state =
    | Initial  (* There have been no docstrings yet *)
    | After of docstring list
        (* There have been docstrings, none of which were
           preceded by a blank line *)
    | Before of docstring list * docstring list * docstring list
        (* There have been docstrings, some of which were
           preceded by a blank line *)

  and docstring = Docstrings.docstring

  let token lexbuf =
    let post_pos = lexeme_end_p lexbuf in
    let attach lines docs pre_pos =
      let open Docstrings in
        match docs, lines with
        | Initial, _ -> ()
        | After a, (NoLine | NewLine) ->
            set_post_docstrings post_pos (List.rev a);
            set_pre_docstrings pre_pos a;
        | After a, BlankLine ->
            set_post_docstrings post_pos (List.rev a);
            set_pre_extra_docstrings pre_pos (List.rev a)
        | Before(a, f, b), (NoLine | NewLine) ->
            set_post_docstrings post_pos (List.rev a);
            set_post_extra_docstrings post_pos
              (List.rev_append f (List.rev b));
            set_floating_docstrings pre_pos (List.rev f);
            set_pre_extra_docstrings pre_pos (List.rev a);
            set_pre_docstrings pre_pos b
        | Before(a, f, b), BlankLine ->
            set_post_docstrings post_pos (List.rev a);
            set_post_extra_docstrings post_pos
              (List.rev_append f (List.rev b));
            set_floating_docstrings pre_pos
              (List.rev_append f (List.rev b));
            set_pre_extra_docstrings pre_pos (List.rev a)
    in
    let rec loop lines docs lexbuf =
      match token_with_comments lexbuf with
      | COMMENT (s, loc) ->
          add_comment (s, loc);
          let lines' =
            match lines with
            | NoLine -> NoLine
            | NewLine -> NoLine
            | BlankLine -> BlankLine
          in
          loop lines' docs lexbuf
      | EOL ->
          let lines' =
            match lines with
            | NoLine -> NewLine
            | NewLine -> BlankLine
            | BlankLine -> BlankLine
          in
          loop lines' docs lexbuf
      | DOCSTRING doc ->
          Docstrings.register doc;
          add_docstring_comment doc;
          let docs' =
            if Docstrings.docstring_body doc = "/*" then
              match docs with
              | Initial -> Before([], [doc], [])
              | After a -> Before (a, [doc], [])
              | Before(a, f, b) -> Before(a, doc :: b @ f, [])
            else
              match docs, lines with
              | Initial, (NoLine | NewLine) -> After [doc]
              | Initial, BlankLine -> Before([], [], [doc])
              | After a, (NoLine | NewLine) -> After (doc :: a)
              | After a, BlankLine -> Before (a, [], [doc])
              | Before(a, f, b), (NoLine | NewLine) -> Before(a, f, doc :: b)
              | Before(a, f, b), BlankLine -> Before(a, b @ f, [doc])
          in
          loop NoLine docs' lexbuf
      | tok ->
          attach lines docs (lexeme_start_p lexbuf);
          tok
    in
      loop NoLine Initial lexbuf

  let init () =
    is_in_string := false;
    comment_start_loc := [];
    comment_list := [];
    match !preprocessor with
    | None -> ()
    | Some (init, _preprocess) -> init ()

  let set_preprocessor init preprocess =
    escaped_newlines := true;
    preprocessor := Some (init, preprocess)

}
