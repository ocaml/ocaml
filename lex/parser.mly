/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* The grammar for lexer definitions */

%{
open Syntax

(* Auxiliaries for the parser. *)

let named_regexps =
  (Hashtbl.create 13 : (string, regular_expression) Hashtbl.t)

let regexp_for_string s =
  let rec re_string n =
    if n >= String.length s then Epsilon
    else if succ n = String.length s then Characters([Char.code (s.[n])])
    else Sequence(Characters([Char.code (s.[n])]), re_string (succ n))
  in re_string 0

let char_class c1 c2 =
  let rec cl n =
    if n > c2 then [] else n :: cl(succ n)
  in cl c1

let all_chars = char_class 0 255

let rec subtract l1 l2 =
  match l1 with
    [] -> []
  | a::r -> if List.mem a l2 then subtract r l2 else a :: subtract r l2
%}

%token <string> Tident
%token <int> Tchar
%token <string> Tstring
%token <Syntax.location> Taction
%token Trule Tparse Tand Tequal Tend Tor Tunderscore Teof Tlbracket Trbracket
%token Tstar Tmaybe Tplus Tlparen Trparen Tcaret Tdash Tlet

%left Tor
%left CONCAT
%nonassoc Tmaybe
%left Tstar
%left Tplus

%start lexer_definition
%type <Syntax.lexer_definition> lexer_definition

%%

lexer_definition:
    header named_regexps Trule definition other_definitions header Tend
        { {header = $1;
           entrypoints = $4 :: List.rev $5;
           trailer = $6} }
;
header:
    Taction
        { $1 }
  | /*epsilon*/
        { { start_pos = 0; end_pos = 0; start_line = 1; start_col = 0 } }
;
named_regexps:
    named_regexps Tlet Tident Tequal regexp
        { Hashtbl.add named_regexps $3 $5 }
  | /*epsilon*/
        { () }
;
other_definitions:
    other_definitions Tand definition
        { $3::$1 }
  | /*epsilon*/
        { [] }
;
definition:
    Tident Tequal entry
        { ($1,$3) }
;
entry:
    Tparse case rest_of_entry
        { $2::List.rev $3 }
  | Tparse rest_of_entry
        { List.rev $2 }
;
rest_of_entry:
    rest_of_entry Tor case
        { $3::$1 }
  |
        { [] }
;
case:
    regexp Taction
        { ($1,$2) }
;
regexp:
    Tunderscore
        { Characters all_chars }
  | Teof
        { Characters [256] }
  | Tchar
        { Characters [$1] }
  | Tstring
        { regexp_for_string $1 }
  | Tlbracket char_class Trbracket
        { Characters $2 }
  | regexp Tstar
        { Repetition $1 }
  | regexp Tmaybe
        { Alternative($1, Epsilon) }
  | regexp Tplus
        { Sequence($1, Repetition $1) }
  | regexp Tor regexp
        { Alternative($1,$3) }
  | regexp regexp %prec CONCAT
        { Sequence($1,$2) }
  | Tlparen regexp Trparen
        { $2 }
  | Tident
        { try
            Hashtbl.find named_regexps $1
          with Not_found ->
            prerr_string "Reference to unbound regexp name `";
            prerr_string $1;
            prerr_string "' at char ";
            prerr_int (Parsing.symbol_start());
            prerr_newline();
            exit 2 }
;
char_class:
    Tcaret char_class1
        { subtract all_chars $2 }
  | char_class1
        { $1 }
;
char_class1:
    Tchar Tdash Tchar
        { char_class $1 $3 }
  | Tchar
        { [$1] }
  | char_class1 char_class1 %prec CONCAT
        { $1 @ $2 }
;

%%

