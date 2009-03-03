(***********************************************************************)
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
exception Error of string
open Glob_ast

type conf_values =
  { plus_tags   : string list;
    minus_tags  : string list;
    plus_flags  : (string * string) list;
    minus_flags : (string * string) list }

type conf = (Glob.globber * conf_values) list

let empty = { plus_flags = []; minus_flags = []; plus_tags = []; minus_tags = [] }
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
let tag = normal+ | ( normal+ ':' normal+ )
let flag_name = normal+
let flag_value = normal_flag_value+
let variable = [ 'a'-'z' 'A'-'Z' '_' '-' '0'-'9' ]*
let pattern = ([^ '(' ')' '\\' ] | '\\' [ '(' ')' ])*

rule ocamldep_output = parse
  | ([^ ':' '\n' '\r' ]+ as k) ':' { let x = (k, space_sep_strings_nl lexbuf) in x :: ocamldep_output lexbuf }
  | eof { [] }
  | _ { raise (Error "Expecting colon followed by space-separated module name list") }

and space_sep_strings_nl = parse
  | space* (not_blank+ as word) { word :: space_sep_strings_nl lexbuf }
  | space* newline { [] }
  | _ { raise (Error "Expecting space-separated strings terminated with newline") }

and space_sep_strings = parse
  | space* (not_blank+ as word) { word :: space_sep_strings lexbuf }
  | space* newline? eof { [] }
  | _ { raise (Error "Expecting space-separated strings") }

and blank_sep_strings = parse
  | blank* '#' not_newline* newline { blank_sep_strings lexbuf }
  | blank* '#' not_newline* eof { [] }
  | blank* (not_blank+ as word) { word :: blank_sep_strings lexbuf }
  | blank* eof { [] }
  | _ { raise (Error "Expecting blank-separated strings") }

and comma_sep_strings = parse
  | space* (not_space_nor_comma+ as word) space* eof { [word] }
  | space* (not_space_nor_comma+ as word) { word :: comma_sep_strings_aux lexbuf }
  | space* eof { [] }
  | _ { raise (Error "Expecting comma-separated strings (1)") }
and comma_sep_strings_aux = parse
  | space* ',' space* (not_space_nor_comma+ as word) { word :: comma_sep_strings_aux lexbuf }
  | space* eof { [] }
  | _ { raise (Error "Expecting comma-separated strings (2)") }

and comma_or_blank_sep_strings = parse
  | space* (not_space_nor_comma+ as word) space* eof { [word] }
  | space* (not_space_nor_comma+ as word) { word :: comma_or_blank_sep_strings_aux lexbuf }
  | space* eof { [] }
  | _ { raise (Error "Expecting (comma|blank)-separated strings (1)") }
and comma_or_blank_sep_strings_aux = parse
  | space* ',' space* (not_space_nor_comma+ as word) { word :: comma_or_blank_sep_strings_aux lexbuf }
  | space* (not_space_nor_comma+ as word) { word :: comma_or_blank_sep_strings_aux lexbuf }
  | space* eof { [] }
  | _ { raise (Error "Expecting (comma|blank)-separated strings (2)") }

and colon_sep_strings = parse
  | ([^ ':']+ as word) eof { [word] }
  | ([^ ':']+ as word) { word :: colon_sep_strings_aux lexbuf }
  | eof { [] }
  | _ { raise (Error "Expecting colon-separated strings (1)") }
and colon_sep_strings_aux = parse
  | ':'+ ([^ ':']+ as word) { word :: colon_sep_strings_aux lexbuf }
  | eof { [] }
  | _ { raise (Error "Expecting colon-separated strings (2)") }

and conf_lines dir pos err = parse
  | space* '#' not_newline* newline { conf_lines dir (pos + 1) err lexbuf }
  | space* '#' not_newline* eof { [] }
  | space* newline { conf_lines dir (pos + 1) err lexbuf }
  | space* eof { [] }
  | space* (not_newline_nor_colon+ as k) space* ':' space*
      {
        let bexpr = Glob.parse ?dir k in
        let v1 = conf_value pos err empty lexbuf in
        let v2 = conf_values pos err v1 lexbuf in
        let rest = conf_lines dir (pos + 1) err lexbuf in (bexpr, v2) :: rest
      }
  | _ { raise (Error(Printf.sprintf "Bad key in configuration line at line %d (from %s)" pos err)) }

and conf_value pos err x = parse
  | '-'  (flag_name as t1) '(' (flag_value as t2) ')' { { (x) with minus_flags = (t1, t2) :: x.minus_flags } }
  | '+'? (flag_name as t1) '(' (flag_value as t2) ')' { { (x) with plus_flags = (t1, t2) :: x.plus_flags } }
  | '-'  (tag as tag) { { (x) with minus_tags = tag :: x.minus_tags } }
  | '+'? (tag as tag) { { (x) with plus_tags = tag :: x.plus_tags } }
  | (_ | eof) { raise (Error(Printf.sprintf "Bad value in configuration line at line %d (from %s)" pos err)) }

and conf_values pos err x = parse
  | space_or_esc_nl* ',' space_or_esc_nl* { conf_values pos err (conf_value pos err x lexbuf) lexbuf }
  | (newline | eof) { x }
  | (_ | eof) { raise (Error(Printf.sprintf "Bad values in configuration line at line %d (from %s)" pos err)) }

and path_scheme patt_allowed = parse
  | ([^ '%' ]+ as prefix)
      { `Word prefix :: path_scheme patt_allowed lexbuf }
  | "%(" (variable as var) ')'
      { `Var (var, Bool.True) :: path_scheme patt_allowed lexbuf }
  | "%(" (variable as var) ':' (pattern as patt) ')'
      { if patt_allowed then
          let patt = My_std.String.implode (unescape (Lexing.from_string patt)) in
          `Var (var, Glob.parse patt) :: path_scheme patt_allowed lexbuf
        else raise (Error(
          Printf.sprintf "Patterns are not allowed in this pathname (%%(%s:%s) only in ~prod)"
            var patt)) }
  | '%'
      { `Var ("", Bool.True) :: path_scheme patt_allowed lexbuf }
  | eof
      { [] }
  | _ { raise (Error("Bad pathanme scheme")) }

and unescape = parse
  | '\\' (['(' ')'] as c)        { c :: unescape lexbuf }
  | _ as c                       { c :: unescape lexbuf }
  | eof                          { [] }
