/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        */
/*          Objective Caml port by John Malecki and Xavier Leroy       */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

%{

open Primitives
open Input_handling
open Parser_aux

%}

%token <string>	ARGUMENT
%token <string>	IDENTIFIER
%token <int>	INTEGER
%token		STAR			/* *  */
%token		MINUS			/* -  */
%token		UNDERUNDER		/* __ */
%token		SHARP			/* #  */
%token		AT			/* @  */
%token		COLONCOLON		/* :: */
%token		COMMA			/* ,  */
%token		UNDERSCORE		/* _  */
%token		LPAREN			/* (  */
%token		RPAREN			/* )  */
%token		LBRACKET		/* [  */
%token		RBRACKET		/* ]  */
%token		LBRACE			/* {  */
%token		RBRACE			/* }  */
%token		SEMI			/* ;  */
%token		EQUAL			/* =  */
%token		SUPERIOR		/* >  */
%token		PREFIX			/* prefix */
%token <string>	OPERATOR          	/* infix/prefix symbols */
%token		EOL

%right COMMA
%right sharp
%right COLONCOLON

%start argument_list_eol
%type <string list> argument_list_eol

%start argument_eol
%type <string> argument_eol

%start integer_list_eol
%type <int list> integer_list_eol

%start integer_eol
%type <int> integer_eol

%start integer
%type <int> integer

%start opt_integer_eol
%type <int option> opt_integer_eol

%start opt_signed_integer_eol
%type <int option> opt_signed_integer_eol

%start identifier
%type <string> identifier

%start identifier_eol
%type <string> identifier_eol

%start identifier_or_eol
%type <string option> identifier_or_eol

%start opt_identifier_eol
%type <string option> opt_identifier_eol

%start variable_list_eol
%type <string list> variable_list_eol

%start break_argument_eol
%type <Parser_aux.break_arg> break_argument_eol

%start match_arguments_eol
%type <string * Parser_aux.pattern> match_arguments_eol

%start list_arguments_eol
%type <string option * int option * int option> list_arguments_eol

%start end_of_line
%type <unit> end_of_line

%%

/* Raw arguments */

argument_list_eol :
    ARGUMENT argument_list_eol
      { $1::$2 }
  | end_of_line
      { [] };

argument_eol :
    ARGUMENT end_of_line
      { $1 };

/* Integer */

integer_list_eol :
    INTEGER integer_list_eol
      { $1::$2 }
  | end_of_line
      { [] };

integer_eol :
    INTEGER end_of_line
      { $1 };

integer :
    INTEGER
      { $1 };

opt_integer_eol :
    INTEGER end_of_line
      { Some $1 }
  | end_of_line
      { None };

opt_signed_integer_eol :
    MINUS integer_eol
      { Some (- $2) }
  | opt_integer_eol
      { $1 };

/* Identifier */

identifier :
    IDENTIFIER
      { $1 };

identifier_eol :
    IDENTIFIER end_of_line
      { $1 };

identifier_or_eol :
    IDENTIFIER
      { Some $1 }
  | end_of_line
      { None };

opt_identifier :
    IDENTIFIER
      { Some $1 }
  |
      { None };

opt_identifier_eol :
    IDENTIFIER end_of_line
      { Some $1 }
  | end_of_line
      { None };

/* Variables list */

variable_list_eol :
    variable variable_list_eol
      { $1::$2 }
  | end_of_line
      { [] };

variable_eol :
  variable end_of_line
    { $1 };

local_name :
    IDENTIFIER
      { $1 }
  | PREFIX STAR
      { "*" };
  | PREFIX MINUS
      { "-" }
  | PREFIX AT
      { "@" }
  | PREFIX EQUAL
      { "=" }
  | PREFIX SUPERIOR
      { ">" }
  | PREFIX OPERATOR
      { $2 };

variable :
    local_name
      { $1 }
  | IDENTIFIER UNDERUNDER local_name
      { $1 ^ "." ^ $3 }
  | STAR
      { "" };


/* Arguments for breakpoint */

break_argument_eol :
    end_of_line
      { BA_none }
  | integer_eol
      { BA_pc $1 }
  | variable_eol
      { BA_function $1 }
  | AT opt_identifier INTEGER opt_integer_eol
      { BA_pos1 ($2, $3, $4) }
  | AT opt_identifier SHARP integer_eol
      { BA_pos2 ($2, $4) };

/* Arguments for list */

list_arguments_eol :
    opt_identifier integer opt_integer_eol
      { ($1, Some $2, $3) }
  | opt_identifier_eol
      { ($1, None, None) };

/* Pattern */

match_arguments_eol :
    variable pattern end_of_line
      { ($1, $2) }

pattern_sm_list :
    pattern SEMI pattern_sm_list
      { $1::$3 }
  | pattern
      { [$1] }
;

pattern_label_list :
    pattern_label SEMI pattern_label_list
      { $1::$3 }
  | pattern_label
      { [$1] }
;

pattern_label :
    variable EQUAL pattern
      { ($1, $3) }
;

pattern_comma_list :
        pattern_comma_list COMMA pattern
          { $3 :: $1 }
      | pattern COMMA pattern
          { [$3; $1] }
;
  
pattern :
    simple_pattern
      { $1 }
  | pattern COLONCOLON pattern
      { P_concat ($1, $3) }
  | pattern_comma_list
      { P_tuple (List.rev $1) }
  | variable simple_pattern
      { P_constr ($1, $2) }
  | SUPERIOR simple_pattern
      { P_constr ("", $2) }
;

simple_pattern :
    UNDERSCORE
      { P_dummy }
  | identifier
      { P_variable $1 }
  | LBRACKET RBRACKET
      { P_list [] }
  | LBRACKET pattern_sm_list RBRACKET
      { P_list $2 }
  | LBRACE pattern_label_list RBRACE
      { P_record $2 }
  | LPAREN pattern RPAREN
      { $2 }
  | SHARP INTEGER pattern %prec sharp
      { P_nth ($2, $3) }
;

/* End of line */

end_of_line :
    EOL
      { stop_user_input () };
