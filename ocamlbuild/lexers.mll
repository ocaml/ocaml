(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
{
exception Error of (string * Lexing.position)

let error lexbuf fmt = Printf.ksprintf (fun s -> raise (Error (s,Lexing.lexeme_start_p lexbuf))) fmt

open Glob_ast

type conf_values =
  { plus_tags   : string list;
    minus_tags  : string list }

type conf = (Glob.globber * conf_values) list

let empty = { plus_tags = []; minus_tags = [] }
}

let newline = ('\n' | '\r' | "\r\n")
let space = [' ' '\t' '\012']
let space_or_esc_nl = (space | '\\' newline)
let blank = newline | space
let not_blank = [^' ' '\t' '\012' '\n' '\r']
let not_space_nor_comma = [^' ' '\t' '\012' ',']
let not_newline = [^ '\n' '\r' ]
let not_newline_nor_colon = [^ '\n' '\r' ':' ]
let normal_flag_value = [^ '(' ')' '\n' '\r']
let normal = [^ ':' ',' '(' ')' ''' ' ' '\n' '\r']
let tag = normal+ | ( normal+ ':' normal+ ) | normal+ '(' [^ ')' ]* ')'
let variable = [ 'a'-'z' 'A'-'Z' '_' '-' '0'-'9' ]*
let pattern = ([^ '(' ')' '\\' ] | '\\' [ '(' ')' ])*

rule ocamldep_output = parse
  | ([^ ':' '\n' '\r' ]+ as k) ':' { let x = (k, space_sep_strings_nl lexbuf) in x :: ocamldep_output lexbuf }
  | eof { [] }
  | _ { error lexbuf "Expecting colon followed by space-separated module name list" }

and space_sep_strings_nl = parse
  | space* (not_blank+ as word) { word :: space_sep_strings_nl lexbuf }
  | space* newline { Lexing.new_line lexbuf; [] }
  | _ { error lexbuf "Expecting space-separated strings terminated with newline" }

and space_sep_strings = parse
  | space* (not_blank+ as word) { word :: space_sep_strings lexbuf }
  | space* newline? eof { [] }
  | _ { error lexbuf "Expecting space-separated strings" }

and blank_sep_strings = parse
  | blank* '#' not_newline* newline { blank_sep_strings lexbuf }
  | blank* '#' not_newline* eof { [] }
  | blank* (not_blank+ as word) { word :: blank_sep_strings lexbuf }
  | blank* eof { [] }
  | _ { error lexbuf "Expecting blank-separated strings" }

and comma_sep_strings = parse
  | space* (not_space_nor_comma+ as word) space* eof { [word] }
  | space* (not_space_nor_comma+ as word) { word :: comma_sep_strings_aux lexbuf }
  | space* eof { [] }
  | _ { error lexbuf "Expecting comma-separated strings (1)" }
and comma_sep_strings_aux = parse
  | space* ',' space* (not_space_nor_comma+ as word) { word :: comma_sep_strings_aux lexbuf }
  | space* eof { [] }
  | _ { error lexbuf "Expecting comma-separated strings (2)" }

and comma_or_blank_sep_strings = parse
  | space* (not_space_nor_comma+ as word) space* eof { [word] }
  | space* (not_space_nor_comma+ as word) { word :: comma_or_blank_sep_strings_aux lexbuf }
  | space* eof { [] }
  | _ { error lexbuf "Expecting (comma|blank)-separated strings (1)" }
and comma_or_blank_sep_strings_aux = parse
  | space* ',' space* (not_space_nor_comma+ as word) { word :: comma_or_blank_sep_strings_aux lexbuf }
  | space* (not_space_nor_comma+ as word) { word :: comma_or_blank_sep_strings_aux lexbuf }
  | space* eof { [] }
  | _ { error lexbuf "Expecting (comma|blank)-separated strings (2)" }

and parse_environment_path_w = parse
  | ([^ ';']* as word) { word :: parse_environment_path_aux_w lexbuf }
  | ';' ([^ ';']* as word) { "" :: word :: parse_environment_path_aux_w lexbuf }
  | eof { [] }
and parse_environment_path_aux_w = parse
  | ';' ([^ ';']* as word) { word :: parse_environment_path_aux_w lexbuf }
  | eof { [] }
  | _ { error lexbuf "Impossible: expecting colon-separated strings" }

and parse_environment_path = parse
  | ([^ ':']* as word) { word :: parse_environment_path_aux lexbuf }
  | ':' ([^ ':']* as word) { "" :: word :: parse_environment_path_aux lexbuf }
  | eof { [] }
and parse_environment_path_aux = parse
  | ':' ([^ ':']* as word) { word :: parse_environment_path_aux lexbuf }
  | eof { [] }
  | _ { error lexbuf "Impossible: expecting colon-separated strings" }

and conf_lines dir = parse
  | space* '#' not_newline* newline { Lexing.new_line lexbuf; conf_lines dir lexbuf }
  | space* '#' not_newline* eof { [] }
  | space* newline { Lexing.new_line lexbuf; conf_lines dir lexbuf }
  | space* eof { [] }
  | space* (not_newline_nor_colon+ as k) space* ':' space*
      {
        let bexpr =
          try Glob.parse ?dir k
          with exn -> error lexbuf "Invalid globbing pattern %S" k (Printexc.to_string exn)
        in
        let v1 = conf_value empty lexbuf in
        let v2 = conf_values v1 lexbuf in
        Lexing.new_line lexbuf; (* FIXME values may have escaped newlines *)
        let rest = conf_lines dir lexbuf in (bexpr,v2) :: rest
      }
  | _ { error lexbuf "Invalid line syntax" }

and conf_value x = parse
  | '-'  (tag as tag) { { (x) with minus_tags = tag :: x.minus_tags } }
  | '+'? (tag as tag) { { (x) with plus_tags = tag :: x.plus_tags } }
  | (_ | eof) { error lexbuf "Invalid tag modifier only '+ or '-' are allowed as prefix for tag" }

and conf_values x = parse
  | space_or_esc_nl* ',' space_or_esc_nl* { conf_values (conf_value x lexbuf) lexbuf }
  | (newline | eof) { x }
  | (_ | eof) { error lexbuf "Only ',' separated tags are alllowed" }

and path_scheme patt_allowed = parse
  | ([^ '%' ]+ as prefix)
      { `Word prefix :: path_scheme patt_allowed lexbuf }
  | "%(" (variable as var) ')'
      { `Var (var, Bool.True) :: path_scheme patt_allowed lexbuf }
  | "%(" (variable as var) ':' (pattern as patt) ')'
      { if patt_allowed then
          let patt = My_std.String.implode (unescape (Lexing.from_string patt)) in
          `Var (var, Glob.parse patt) :: path_scheme patt_allowed lexbuf
        else
          error lexbuf "Patterns are not allowed in this pathname (%%(%s:%s) only in ~prod)" var patt }
  | '%'
      { `Var ("", Bool.True) :: path_scheme patt_allowed lexbuf }
  | eof
      { [] }
  | _ { error lexbuf "Bad pathanme scheme" }

and unescape = parse
  | '\\' (['(' ')'] as c)        { c :: unescape lexbuf }
  | _ as c                       { c :: unescape lexbuf }
  | eof                          { [] }

and ocamlfind_query = parse
  | newline*
    "package:" space* (not_newline* as n) newline+
    "description:" space* (not_newline* as d) newline+
    "version:" space* (not_newline* as v) newline+
    "archive(s):" space* (not_newline* as a) newline+
    "linkopts:" space* (not_newline* as lo) newline+
    "location:" space* (not_newline* as l) newline+
    { n, d, v, a, lo, l }
  | _ { error lexbuf "Bad ocamlfind query" }

and trim_blanks = parse
  | blank* (not_blank* as word) blank* { word }
  | _ { error lexbuf "Bad input for trim_blanks" }

and tag_gen = parse
  | (normal+ as name) ('(' ([^')']* as param) ')')? { name, param }
