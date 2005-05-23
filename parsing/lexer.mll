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

(* The lexer definition *)

{
open Lexing
open Misc
open Parser

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_string_in_comment
  | Keyword_as_label of string
  | Literal_overflow of string
  | Illegal_ncname of int
;;

let ext = Location.ext

exception Error of error * Location.t;;

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

    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lor", INFIXOP3("lor");
    "lxor", INFIXOP3("lxor");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr")
]

let join_keyword_table =
  let h = Hashtbl.copy keyword_table in
  Hashtbl.add h "def" DEF;
  Hashtbl.add h "loc" LOC;
  Hashtbl.add h "spawn" SPAWN;
  Hashtbl.add h "reply" REPLY;
  Hashtbl.add h "nullp" NULLP;
  h

let keywords () =
  if !Clflags.join then join_keyword_table else keyword_table

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

(* To buffer Unicode string literals *)

let unicode_buffer = Buffer.create 1024

module U = Cduce_types.Encodings.Utf8
let store_uchar = U.store unicode_buffer
let store_ucharc c = U.store unicode_buffer (Char.code c)

let reset_unicode_buffer () = Buffer.clear unicode_buffer
let get_stored_unicode () = 
  let s = Buffer.contents unicode_buffer in
  Buffer.clear unicode_buffer;
  U.mk s

let rec int_of_hex_string s i accu =
  if (i = String.length s) then accu
  else
    let d = Char.code s.[i] in
    let digit = if d >= 97 then d - 87 else if d >= 65 then d - 55 else d - 48
    in int_of_hex_string s (succ i) (accu * 16 + digit)

let int_of_hex_string s = int_of_hex_string s 0 0


(* To store the position of the beginning of a string and comment *)
let string_start_loc = ref Location.none;;
let comment_start_loc = ref [];;
let in_comment () = !comment_start_loc <> [];;

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
  if (c < 0 || c > 255) && not (in_comment ())
  then raise (Error(Illegal_escape (Lexing.lexeme lexbuf),
                    Location.curr lexbuf))
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

(* Remove underscores from float literals *)

let remove_underscores s =
  let l = String.length s in
  let rec remove src dst =
    if src >= l then
      if dst >= l then s else String.sub s 0 dst
    else
      match s.[src] with
        '_' -> remove (src + 1) dst
      |  c  -> s.[dst] <- c; remove (src + 1) (dst + 1)
  in remove 0 0

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
;;

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment ->
      fprintf ppf "Comment not terminated"
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment ->
      fprintf ppf "This comment contains an unterminated string literal"
  | Keyword_as_label kwd ->
      fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
  | Literal_overflow ty ->
      fprintf ppf "Integer literal exceeds the range of representable integers of type %s" ty
  | Illegal_ncname i ->
      fprintf ppf "Illegal character in NCNAME (codepoint %i)" i

;;

let xml_base_char =
  [ 0x0041,0x005A; 0x0061,0x007A; 0x00C0,0x00D6; 0x00D8,0x00F6; 
    0x00F8,0x00FF; 0x0100,0x0131; 0x0134,0x013E; 0x0141,0x0148; 
    0x014A,0x017E; 0x0180,0x01C3; 0x01CD,0x01F0; 0x01F4,0x01F5; 
    0x01FA,0x0217; 0x0250,0x02A8; 0x02BB,0x02C1; 0x0386,0x0386;
    0x0388,0x038A; 0x038C,0x038C; 0x038E,0x03A1; 0x03A3,0x03CE; 
    0x03D0,0x03D6; 0x03DA,0x03DA; 0x03DC,0x03DC; 0x03DE,0x03DE; 
    0x03E0,0x03E0; 0x03E2,0x03F3; 
    0x0401,0x040C; 0x040E,0x044F; 0x0451,0x045C; 0x045E,0x0481; 
    0x0490,0x04C4; 0x04C7,0x04C8; 0x04CB,0x04CC; 0x04D0,0x04EB; 
    0x04EE,0x04F5; 0x04F8,0x04F9; 0x0531,0x0556; 0x0559,0x0559;
    0x0561,0x0586; 0x05D0,0x05EA; 0x05F0,0x05F2; 0x0621,0x063A; 
    0x0641,0x064A; 0x0671,0x06B7; 0x06BA,0x06BE; 0x06C0,0x06CE; 
    0x06D0,0x06D3; 0x06D5,0x06D5; 0x06E5,0x06E6; 0x0905,0x0939; 
    0x093D,0x093D;
    0x0958,0x0961; 0x0985,0x098C; 0x098F,0x0990; 0x0993,0x09A8; 
    0x09AA,0x09B0; 0x09B2,0x09B2; 0x09B6,0x09B9; 0x09DC,0x09DD; 
    0x09DF,0x09E1; 0x09F0,0x09F1; 0x0A05,0x0A0A; 0x0A0F,0x0A10; 
    0x0A13,0x0A28; 0x0A2A,0x0A30; 0x0A32,0x0A33; 0x0A35,0x0A36; 
    0x0A38,0x0A39; 0x0A59,0x0A5C; 0x0A5E,0x0A5E; 0x0A72,0x0A74; 
    0x0A85,0x0A8B; 0x0A8D,0x0A8D; 0x0A8F,0x0A91; 0x0A93,0x0AA8; 
    0x0AAA,0x0AB0; 0x0AB2,0x0AB3; 0x0AB5,0x0AB9; 0x0ABD,0x0ABD; 
    0x0AE0,0x0AE0;
    0x0B05,0x0B0C; 0x0B0F,0x0B10; 0x0B13,0x0B28; 0x0B2A,0x0B30; 
    0x0B32,0x0B33; 0x0B36,0x0B39; 0x0B3D,0x0B3D; 0x0B5C,0x0B5D; 
    0x0B5F,0x0B61; 0x0B85,0x0B8A; 0x0B8E,0x0B90; 0x0B92,0x0B95; 
    0x0B99,0x0B9A; 0x0B9C,0x0B9C; 0x0B9E,0x0B9F; 0x0BA3,0x0BA4; 
    0x0BA8,0x0BAA; 0x0BAE,0x0BB5; 0x0BB7,0x0BB9; 0x0C05,0x0C0C; 
    0x0C0E,0x0C10; 0x0C12,0x0C28; 0x0C2A,0x0C33; 0x0C35,0x0C39; 
    0x0C60,0x0C61; 0x0C85,0x0C8C; 0x0C8E,0x0C90; 0x0C92,0x0CA8; 
    0x0CAA,0x0CB3; 0x0CB5,0x0CB9; 0x0CDE,0x0CDE; 0x0CE0,0x0CE1; 
    0x0D05,0x0D0C; 0x0D0E,0x0D10; 0x0D12,0x0D28; 0x0D2A,0x0D39; 
    0x0D60,0x0D61; 0x0E01,0x0E2E; 0x0E30,0x0E30; 0x0E32,0x0E33; 
    0x0E40,0x0E45; 0x0E81,0x0E82; 0x0E84,0x0E84; 0x0E87,0x0E88; 
    0x0E8A,0x0E8A;
    0x0E8D,0x0E8D; 0x0E94,0x0E97; 0x0E99,0x0E9F; 0x0EA1,0x0EA3; 
    0x0EA5,0x0EA5;
    0x0EA7,0x0EA7; 0x0EAA,0x0EAB; 0x0EAD,0x0EAE; 0x0EB0,0x0EB0; 
    0x0EB2,0x0EB3;
    0x0EBD,0x0EBD; 0x0EC0,0x0EC4; 0x0F40,0x0F47; 0x0F49,0x0F69; 
    0x10A0,0x10C5; 0x10D0,0x10F6; 0x1100,0x1100; 0x1102,0x1103; 
    0x1105,0x1107; 0x1109,0x1109; 0x110B,0x110C; 0x110E,0x1112; 
    0x113C,0x113C;
    0x113E,0x113E; 0x1140,0x1140; 0x114C,0x114C; 0x114E,0x114E; 
    0x1150,0x1150; 0x1154,0x1155; 0x1159,0x1159;
    0x115F,0x1161; 0x1163,0x1163; 0x1165,0x1165; 0x1167,0x1167; 
    0x1169,0x1169; 0x116D,0x116E; 
    0x1172,0x1173; 0x1175,0x1175; 0x119E,0x119E; 0x11A8,0x11A8; 
    0x11AB,0x11AB; 0x11AE,0x11AF; 
    0x11B7,0x11B8; 0x11BA,0x11BA; 0x11BC,0x11C2; 0x11EB,0x11EB; 
    0x11F0,0x11F0; 0x11F9,0x11F9;
    0x1E00,0x1E9B; 0x1EA0,0x1EF9; 0x1F00,0x1F15; 0x1F18,0x1F1D; 
    0x1F20,0x1F45; 0x1F48,0x1F4D; 0x1F50,0x1F57; 0x1F59,0x1F59; 
    0x1F5B,0x1F5B;
    0x1F5D,0x1F5D; 0x1F5F,0x1F7D; 0x1F80,0x1FB4; 0x1FB6,0x1FBC; 
    0x1FBE,0x1FBE;
    0x1FC2,0x1FC4; 0x1FC6,0x1FCC; 0x1FD0,0x1FD3; 0x1FD6,0x1FDB; 
    0x1FE0,0x1FEC; 0x1FF2,0x1FF4; 0x1FF6,0x1FFC; 0x2126,0x2126;
    0x212A,0x212B; 0x212E,0x212E; 0x2180,0x2182; 0x3041,0x3094; 
    0x30A1,0x30FA; 0x3105,0x312C; 0xAC00,0xD7A3 ]
  
let xml_ideographic =
  [ 0x3007,0x3007; 0x3021,0x3029; 0x4E00,0x9FA5 ]

let xml_combining_char =
  [ 0x0300,0x0345; 0x0360,0x0361; 0x0483,0x0486; 0x0591,0x05A1;
    0x05A3,0x05B9; 0x05BB,0x05BD; 0x05BF,0x05BF; 0x05C1,0x05C2;
    0x05C4,0x05C4; 0x064B,0x0652; 0x0670,0x0670; 0x06D6,0x06DC;
    0x06DD,0x06DF; 0x06E0,0x06E4; 0x06E7,0x06E8; 0x06EA,0x06ED;
    0x0901,0x0903; 0x093C,0x093C; 0x093E,0x094C; 0x094D,0x094D;
    0x0951,0x0954; 0x0962,0x0963; 0x0981,0x0983; 0x09BC,0x09BC;
    0x09BE,0x09BE; 0x09BF,0x09BF; 0x09C0,0x09C4; 0x09C7,0x09C8;
    0x09CB,0x09CD; 0x09D7,0x09D7; 0x09E2,0x09E3; 0x0A02,0x0A02;
    0x0A3C,0x0A3C; 0x0A3E,0x0A3E; 0x0A3F,0x0A3F; 0x0A40,0x0A42;
    0x0A47,0x0A48; 0x0A4B,0x0A4D; 0x0A70,0x0A71; 0x0A81,0x0A83;
    0x0ABC,0x0ABC; 0x0ABE,0x0AC5; 0x0AC7,0x0AC9; 0x0ACB,0x0ACD;
    0x0B01,0x0B03; 0x0B3C,0x0B3C; 0x0B3E,0x0B43; 0x0B47,0x0B48;
    0x0B4B,0x0B4D; 0x0B56,0x0B57; 0x0B82,0x0B83; 0x0BBE,0x0BC2;
    0x0BC6,0x0BC8; 0x0BCA,0x0BCD; 0x0BD7,0x0BD7; 0x0C01,0x0C03;
    0x0C3E,0x0C44; 0x0C46,0x0C48; 0x0C4A,0x0C4D; 0x0C55,0x0C56;
    0x0C82,0x0C83; 0x0CBE,0x0CC4; 0x0CC6,0x0CC8; 0x0CCA,0x0CCD;
    0x0CD5,0x0CD6; 0x0D02,0x0D03; 0x0D3E,0x0D43; 0x0D46,0x0D48;
    0x0D4A,0x0D4D; 0x0D57,0x0D57; 0x0E31,0x0E31; 0x0E34,0x0E3A;
    0x0E47,0x0E4E; 0x0EB1,0x0EB1; 0x0EB4,0x0EB9; 0x0EBB,0x0EBC;
    0x0EC8,0x0ECD; 0x0F18,0x0F19; 0x0F35,0x0F35; 0x0F37,0x0F37;
    0x0F39,0x0F39; 0x0F3E,0x0F3E; 0x0F3F,0x0F3F; 0x0F71,0x0F84;
    0x0F86,0x0F8B; 0x0F90,0x0F95; 0x0F97,0x0F97; 0x0F99,0x0FAD;
    0x0FB1,0x0FB7; 0x0FB9,0x0FB9; 0x20D0,0x20DC; 0x20E1,0x20E1;
    0x302A,0x302F; 0x3099,0x3099; 0x309A,0x309A ]

let xml_digit =
  [ 0x0030,0x0039;
    0x0660,0x0669; 0x06F0,0x06F9; 0x0966,0x096F; 0x09E6,0x09EF;
    0x0A66,0x0A6F; 0x0AE6,0x0AEF; 0x0B66,0x0B6F; 0x0BE7,0x0BEF;
    0x0C66,0x0C6F; 0x0CE6,0x0CEF; 0x0D66,0x0D6F; 0x0E50,0x0E59;
    0x0ED0,0x0ED9; 0x0F20,0x0F29 ]

let xml_extender =
  [ 0x00B7,0x00B7; 0x02D0,0x02D1; 0x0387,0x0387; 0x0640,0x0640;
    0x0E46,0x0E46; 0x0EC6,0x0EC6; 0x3005,0x3005; 0x3031,0x3035;
    0x309D,0x309E; 0x30FC,0x30FE ]

let mk_disp l = 
  let good = Cduce_types.Chars.mk_classes  l in
  let bad = Cduce_types.Chars.diff Cduce_types.Chars.any good in
  let m = Cduce_types.Chars.mk_map [ good,true; bad,false ] in
  fun i -> Cduce_types.Chars.get_map (Cduce_types.Chars.V.mk_int i) m

let namechar = 
  mk_disp
    (xml_base_char @ xml_ideographic @ xml_digit @ xml_combining_char @ 
       xml_extender @ [ 45,46; 95,95 ])

let namechar_first =
  mk_disp (xml_base_char @ xml_ideographic @ [ 95,95 ])

let parse_ncname lexbuf =
  let s = Lexing.lexeme lexbuf in
  reset_unicode_buffer ();
  let rec aux1 first i =
    if i = String.length s then get_stored_unicode ()
    else match s.[i] with
      | '\\' -> aux2 first 0 (succ i)
      | c -> store_ucharc c; aux1 false (succ i)
  and aux2 first accu i =
    match s.[i] with
      | '.' -> store_ucharc '.'; aux1 false (succ i)
      | '0'..'9' as c -> aux2 first (accu*10 + (Char.code c - 48)) (succ i)
      | ';' -> 
	  if not ((if first then namechar_first else namechar) accu) 
	  then raise (Error(Illegal_ncname accu, Location.curr lexbuf));
	  store_uchar accu; aux1 false (succ i)
      | _ -> assert false
  in
  aux1 true 0
}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
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
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

let xml_letter =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' ]
let xml_uchar =
  '\\' ['0'-'9']+ ';'
let xml_ncname =
  (xml_uchar | xml_letter | '_') 
  (xml_uchar | xml_letter | "\\." | ['0'-'9' '-' '_' '\xb7' ])*
let xml_uncname =
  uppercase
  (xml_uchar | xml_letter | "\\." | ['0'-'9' '-' '_' '\xb7' ])*


rule token = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        token lexbuf
      }
  | blank +
      { token lexbuf }
  | "_"
      { UNDERSCORE }
  | "~"  { TILDE }
  | "~" lowercase identchar * ':'
      { let s = Lexing.lexeme lexbuf in
        let name = String.sub s 1 (String.length s - 2) in
        if Hashtbl.mem (keywords ()) name then
          raise (Error(Keyword_as_label name, Location.curr lexbuf));
        LABEL name }
  | "?"  { QUESTION }
  | "??" { QUESTIONQUESTION }
  | "?" lowercase identchar * ':'
      { let s = Lexing.lexeme lexbuf in
        let name = String.sub s 1 (String.length s - 2) in
        if Hashtbl.mem (keywords ()) name then
          raise (Error(Keyword_as_label name, Location.curr lexbuf));
        OPTLABEL name }
  | lowercase identchar *
      { let s = Lexing.lexeme lexbuf in
          try
            Hashtbl.find (keywords ()) s
          with Not_found ->
            LIDENT s }
  | uppercase identchar *
      { UIDENT(Lexing.lexeme lexbuf) }       (* No capitalized keywords *)
  | int_literal
      { try
          INT (int_of_string(Lexing.lexeme lexbuf))
        with Failure _ ->
          raise (Error(Literal_overflow "int", Location.curr lexbuf))
      }
  | float_literal
      { FLOAT (remove_underscores(Lexing.lexeme lexbuf)) }
  | int_literal "l"
      { let s = Lexing.lexeme lexbuf in
        try
          INT32 (Int32.of_string(String.sub s 0 (String.length s - 1)))
        with Failure _ ->
          raise (Error(Literal_overflow "int32", Location.curr lexbuf)) }
  | int_literal "L"
      { let s = Lexing.lexeme lexbuf in
        try
          INT64 (Int64.of_string(String.sub s 0 (String.length s - 1)))
        with Failure _ ->
          raise (Error(Literal_overflow "int64", Location.curr lexbuf)) }
  | int_literal "n"
      { let s = Lexing.lexeme lexbuf in
        try
          NATIVEINT 
            (Nativeint.of_string(String.sub s 0 (String.length s - 1)))
        with Failure _ ->
          raise (Error(Literal_overflow "nativeint", Location.curr lexbuf)) }
  | "\""
      { reset_string_buffer();
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        string lexbuf;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string()) }
  | "'" newline "'"
      { update_loc lexbuf None 1 false 1;
        CHAR (Lexing.lexeme_char lexbuf 1) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
      { CHAR(Lexing.lexeme_char lexbuf 1) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r'] "'"
      { CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHAR(char_for_decimal_code lexbuf 2) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { CHAR(char_for_hexadecimal_code lexbuf 3) }
  | "'\\" _
      { let l = Lexing.lexeme lexbuf in
        let esc = String.sub l 1 (String.length l - 1) in
        raise (Error(Illegal_escape esc, Location.curr lexbuf))
      }
  | "(*"
      { comment_start_loc := [Location.curr lexbuf];
        comment lexbuf;
        token lexbuf }
  | "(*)"
      { let loc = Location.curr lexbuf in
        Location.prerr_warning loc (Warnings.Comment "the start of a comment");
        comment_start_loc := [Location.curr lexbuf];
        comment lexbuf;
        token lexbuf
      }
  | "*)"
      { let loc = Location.curr lexbuf in
        let warn = Warnings.Comment "not the end of a comment" in
        Location.prerr_warning loc warn;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
        STAR
      }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
      { update_loc lexbuf name (int_of_string num) true 0;
        token lexbuf
      }
  | "#"  { SHARP }
  | "&"  { AMPERSAND }
  | "&&" { AMPERAMPER }
  | "`"  { BACKQUOTE }
  | "'"  { QUOTE }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "*"  { STAR }
  | ","  { COMMA }
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
  | "[>" { LBRACKETGREATER }
  | "]"  { RBRACKET }
  | "{"  { LBRACE }
  | "{<" { LBRACELESS }
  | "{{" { assert (not !ext); ext := true; start_lbracebrace lexbuf }
  | "{:" { assert (not !ext); ext := true; LBRACECOLON }
  | ":}" { assert (not !ext); ext := true; RCOLONBRACE }
  | "|"  { BAR }
  | "||" { BARBAR }
  | "|]" { BARRBRACKET }
  | ">"  { GREATER }
  | ">]" { GREATERRBRACKET }
  | "}"  { RBRACE }
  | ">}" { GREATERRBRACE }

  | "!=" { INFIXOP0 "!=" }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "-." { MINUSDOT }

  | "!" symbolchar *
            { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['~' '?'] symbolchar +
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
      { raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     Location.curr lexbuf))
      }

and comment = parse
    "(*"
      { comment_start_loc := (Location.curr lexbuf) :: !comment_start_loc;
        comment lexbuf;
      }
  | "*)"
      { match !comment_start_loc with
        | [] -> assert false
        | [x] -> comment_start_loc := [];
        | _ :: l -> comment_start_loc := l;
                    comment lexbuf;
       }
  | "\""
      { reset_string_buffer();
        string_start_loc := Location.curr lexbuf;
        begin try string lexbuf
        with Error (Unterminated_string, _) ->
          match !comment_start_loc with
          | [] -> assert false
          | loc :: _ -> comment_start_loc := [];
                        raise (Error (Unterminated_string_in_comment, loc))
        end;
        reset_string_buffer ();
        comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'" newline "'"
      { update_loc lexbuf None 1 false 1;
        comment lexbuf
      }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
      { comment lexbuf }
  | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r'] "'"
      { comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { comment lexbuf }
  | eof
      { match !comment_start_loc with
        | [] -> assert false
        | loc :: _ -> comment_start_loc := [];
                      raise (Error (Unterminated_comment, loc))
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        comment lexbuf
      }
  | _
      { comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        string lexbuf
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { store_string_char(char_for_hexadecimal_code lexbuf 2);
         string lexbuf }
  | '\\' _
      { if in_comment ()
        then string lexbuf
        else begin
(*  Should be an error, but we are very lax.
          raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
                        Location.curr lexbuf))
*)
          let loc = Location.curr lexbuf in
          let warn = Warnings.Other "Illegal backslash escape in string" in
          Location.prerr_warning loc warn;
          store_string_char (Lexing.lexeme_char lexbuf 0);
          store_string_char (Lexing.lexeme_char lexbuf 1);
          string lexbuf
        end
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        let s = Lexing.lexeme lexbuf in
        for i = 0 to String.length s - 1 do
          store_string_char s.[i];
        done;
        string lexbuf
      }
  | eof
      { raise (Error (Unterminated_string, !string_start_loc)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and xstring double = parse
    '"'
      { if double then () else (store_ucharc '"'; xstring double lexbuf) }
  | "'" 
      { if double then (store_ucharc '\"'; xstring double lexbuf) else () }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        xstring double lexbuf
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r']
      { store_ucharc (char_for_backslash(Lexing.lexeme_char lexbuf 1));
        xstring double lexbuf }
  | '\\' ['0'-'9']+ ';'
      { let s = Lexing.lexeme lexbuf in
        let s = String.sub s 1 (String.length s - 2) in
        store_uchar (int_of_string s);
        xstring double lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F']+ ';'
      { let s = Lexing.lexeme lexbuf in
        let s = String.sub s 1 (String.length s - 2) in
        store_uchar (int_of_hex_string s);
        xstring double lexbuf }
  | '\\' _
      { if in_comment ()
        then xstring double lexbuf
        else
          raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
                        Location.curr lexbuf))
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        let s = Lexing.lexeme lexbuf in
        for i = 0 to String.length s - 1 do
          store_ucharc s.[i];
        done;
        xstring double lexbuf
      }
  | eof
      { raise (Error (Unterminated_string, !string_start_loc)) }
  | _
      { store_ucharc (Lexing.lexeme_char lexbuf 0);
        xstring double lexbuf }

and skip_sharp_bang = parse
  | "#!" [^ '\n']* '\n' [^ '\n']* "\n!#\n"
       { update_loc lexbuf None 3 false 0 }
  | "#!" [^ '\n']* '\n'
       { update_loc lexbuf None 1 false 0 }
  | "" { () }

and xtoken = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        xtoken lexbuf
      }
  | blank +
      { xtoken lexbuf }
  | decimal_literal
      { 
	let s = Lexing.lexeme lexbuf in
	try XINT (Big_int.big_int_of_string s)
        with Failure _ ->
          raise (Error(Literal_overflow "big_int", Location.curr lexbuf))
      }
  | "}" { RBRACE }
  | "}}" { ext := false; RBRACEBRACE }
  | ":}" { ext := false; RCOLONBRACE }
  | "{{" { ext := false; LBRACEBRACE }
  | "{:" { ext := false; LBRACECOLON }
  | '"'
      { reset_unicode_buffer ();
	let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        xstring true lexbuf;
        lexbuf.lex_start_p <- string_start;
        XSTRING2 (get_stored_unicode()) }
  | "'"
      { reset_unicode_buffer ();
	let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        xstring false lexbuf;
        lexbuf.lex_start_p <- string_start;
        XSTRING1 (get_stored_unicode()) }
  | '`' { BACKQUOTE }
  | ':' { COLON }
  | '+' { PLUS }
  | "++" { PLUSPLUS }
  | "*?" { STARQUESTION }
  | "+?" { PLUSQUESTION }
  | "??" { QUESTIONQUESTION }
  | '-' { MINUS }
  | "--" { DASHDASH }
  | '*' { STAR }
  | "**" { STARSTAR }
  | "div" { DIV }
  | "mod" { MOD }
  | "@" { CONCAT }
  | ":?" { COLONQUESTION }
  | "." { DOT }
  | "-." { MINUSDOT }
  | "match" { MATCH }
  | "map" { MAP }
  | "with" { WITH }
  | "let" { LET }
  | "in" { IN }
  | "namespace" { NAMESPACE }
  | "else" { ELSE }
  | "=?" { EQUALQUESTION }
  | "->" { MINUSGREATER }
  | "<-" { LESSMINUS }
  | "_" { UNDERSCORE }
  | "<" { LESS }
  | ">" { GREATER }
  | ".." { DOTDOT }
  | "|" { BAR }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "=" { EQUAL }
  | "!" { BANG }
  | ";" { SEMI } 
  | "&" { AMPERSAND }
  | ":=" { COLONEQUAL }
  | "?" { QUESTION }
  | "::" { COLONCOLON }
  | "/" { SLASH }
  | xml_uncname { UNCNAME (parse_ncname lexbuf) }
  | xml_ncname { NCNAME (parse_ncname lexbuf) }
  | eof { EOF }
  | "(*"
      { comment_start_loc := [Location.curr lexbuf];
        comment lexbuf;
        xtoken lexbuf }
  | "(*)"
      { let loc = Location.curr lexbuf in
        Location.prerr_warning loc (Warnings.Comment "the start of a comment");
        comment_start_loc := [Location.curr lexbuf];
        comment lexbuf;
        xtoken lexbuf
      }
  | "*)"
      { let loc = Location.curr lexbuf in
        let warn = Warnings.Comment "not the end of a comment" in
        Location.prerr_warning loc warn;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
        STAR
      }
  | _
      { raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     Location.curr lexbuf))
      }

and start_lbracebrace = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        start_lbracebrace lexbuf
      }
  | blank +
      { start_lbracebrace lexbuf }
  | "(*"
      { comment_start_loc := [Location.curr lexbuf];
        comment lexbuf;
        start_lbracebrace lexbuf }
  | "namespace" { LBRACEBRACENAMESPACE }
  | "" { LBRACEBRACE }


{
  let token lexbuf = if !ext then xtoken lexbuf else token lexbuf
  let init () = ext := false
}
