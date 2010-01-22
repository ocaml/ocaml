/***********************************************************************/
/*                                                                     */
/*                 MLTk, Tcl/Tk interface of Objective Caml            */
/*                                                                     */
/*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    */
/*               projet Cristal, INRIA Rocquencourt                    */
/*            Jacques Garrigue, Kyoto University RIMS                  */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique and Kyoto University.  All rights reserved.         */
/*  This file is distributed under the terms of the GNU Library        */
/*  General Public License, with the special exception on linking      */
/*  described in file ../LICENSE.                                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

%{

open Tables

%}

/* Tokens */
%token <string> IDENT
%token <string> STRING
%token EOF

%token LPAREN           /* "(" */
%token RPAREN           /* ")" */
%token COMMA            /* "," */
%token SEMICOLON        /* ";" */
%token COLON            /* ":" */
%token QUESTION         /* "?" */
%token LBRACKET         /* "[" */
%token RBRACKET         /* "]" */
%token LBRACE           /* "{" */
%token RBRACE           /* "}" */
%token SLASH            /* "/" */

%token TYINT            /* "int" */
%token TYFLOAT          /* "float" */
%token TYBOOL           /* "bool" */
%token TYCHAR           /* "char" */
%token TYSTRING         /* "string" */
%token LIST             /* "list" */

%token AS               /* "as" */
%token VARIANT          /* "variant" */
%token WIDGET           /* "widget" */
%token OPTION           /* "option" */
%token TYPE             /* "type" */
%token SEQUENCE         /* "sequence" */
%token SUBTYPE          /* "subtype" */
%token FUNCTION         /* "function" */
%token MODULE           /* "module" */
%token EXTERNAL         /* "external" */
%token UNSAFE           /* "unsafe" */
/* Entry points */
%start entry
%type <unit> entry

%%
TypeName:
   IDENT { String.uncapitalize $1 }
 | WIDGET { "widget" }
;

/* Atomic types */
Type0 :
    TYINT
      { Int }
  | TYFLOAT
      { Float }
  | TYBOOL
      { Bool }
  | TYCHAR
      { Char }
  | TYSTRING
      { String }
  | TypeName
      { UserDefined $1 }
;

/* Camltk/Labltk types */
Type0_5:
  | Type0 SLASH Type0 { if !Flags.camltk then $1 else $3 }
  | Type0 { $1 }
;

/* with subtypes */
Type1 :
    Type0_5
      { $1 }
  | TypeName LPAREN IDENT RPAREN
     { Subtype ($1, $3) }
  | WIDGET LPAREN IDENT RPAREN
     { Subtype ("widget", $3) }
  | OPTION LPAREN IDENT RPAREN
     { Subtype ("options", $3) }
  | Type1 AS STRING
     { As ($1, $3) }
  | LBRACE Type_list RBRACE
      { Product $2 }
;

/* with list constructors */
Type2 :
    Type1
     { $1 }
  | Type2 LIST
     { List $1 }
;

Labeled_type2 :
    Type2
      { "", $1 }
  | IDENT COLON Type2
      { $1, $3 }
;

/* products */
Type_list :
    Type2 COMMA Type_list
      { $1 :: $3 }
  | Type2
      { [$1] }
;

/* records */
Type_record :
    Labeled_type2 COMMA Type_record
      { $1 :: $3 }
  | Labeled_type2
      { [$1] }
;

/* callback arguments or function results*/
FType :
    LPAREN RPAREN
      { Unit }
  | LPAREN Type2 RPAREN
      { $2 }
  | LPAREN Type_record RPAREN
      { Record $2 }
;

Type :
    Type2
      { $1 }
  | FUNCTION FType
      { Function $2 }
;



SimpleArg:
    STRING
      {StringArg $1}
  | Type
      {TypeArg ("", $1) }
;

Arg:
    STRING
      {StringArg $1}
  | Type
      {TypeArg ("", $1) }
  | IDENT COLON Type
      {TypeArg ($1, $3)}
  | QUESTION IDENT COLON LBRACKET SimpleArgList RBRACKET DefaultList
      {OptionalArgs ( $2, $5, $7 )}
  | QUESTION WIDGET COLON LBRACKET SimpleArgList RBRACKET DefaultList
      {OptionalArgs ( "widget", $5, $7 )}
  | QUESTION IDENT COLON LBRACKET SimpleArgList RBRACKET
      {OptionalArgs ( $2, $5, [] )}
  | QUESTION WIDGET COLON LBRACKET SimpleArgList RBRACKET
      {OptionalArgs ( "widget", $5, [] )}
  | WIDGET COLON Type
      {TypeArg ("widget", $3)}
  | Template
      { $1 }
;

SimpleArgList:
    SimpleArg SEMICOLON SimpleArgList
       { $1 :: $3}
  | SimpleArg
      { [$1] }
;

ArgList:
    Arg SEMICOLON ArgList
       { $1 :: $3}
  | Arg
      { [$1] }
;

/* DefaultList Only one TypeArg in ArgList and it must be unlabeled */
DefaultList :
    LBRACKET LBRACE ArgList RBRACE RBRACKET
      {$3}

/* Template */
Template :
    LBRACKET ArgList RBRACKET
      { ListArg $2 }
;


/* Constructors for type declarations */
Constructor :
    IDENT Template
      {{ component = Constructor;
         ml_name = $1;
         var_name = getvarname $1 $2;
         template = $2;
         result = Unit;
         safe = true }}
  | IDENT LPAREN IDENT RPAREN Template
      {{ component = Constructor;
         ml_name = $1;
         var_name = $3;
         template = $5;
         result = Unit;
         safe = true }}
;

AbbrevConstructor :
    Constructor
      { Full $1 }
 |  IDENT
      { Abbrev $1 }
;

Constructors :
  Constructor Constructors
   { $1 :: $2 }
| Constructor
   { [$1] }
;

AbbrevConstructors :
  AbbrevConstructor AbbrevConstructors
   { $1 :: $2 }
| AbbrevConstructor
   { [$1] }
;

Safe:
   /* */
  { true }
 | UNSAFE
  { false }

Command :
   Safe FUNCTION FType IDENT Template
     {{component = Command; ml_name = $4; var_name = "";
       template = $5; result = $3; safe = $1 }}
;

External :
  Safe EXTERNAL IDENT STRING
     {{component = External; ml_name = $3; var_name = "";
       template = StringArg $4; result = Unit; safe = $1}}
;

Option :
   OPTION IDENT Template
     {{component = Constructor; ml_name = $2; var_name = getvarname $2 $3;
       template = $3; result = Unit; safe = true }}
   /* Abbreviated */
|   OPTION IDENT LPAREN IDENT RPAREN Template
     {{component = Constructor; ml_name = $2; var_name = $4;
       template = $6; result = Unit; safe = true }}
   /* Abbreviated */
|  OPTION IDENT
     { retrieve_option $2 }
;

WidgetComponents :
  /* */
  { [] }
 | Command WidgetComponents
  { $1 :: $2 }
 | Option WidgetComponents
  { $1 :: $2 }
 | External WidgetComponents
  { $1 :: $2 }
;

ModuleComponents :
  /* */
  { [] }
 | Command ModuleComponents
  { $1 :: $2 }
 | External ModuleComponents
  { $1 :: $2 }
;

ParserArity :
  /* */
  { OneToken }
 | SEQUENCE
  { MultipleToken }
;



entry :
  TYPE ParserArity TypeName LBRACE Constructors RBRACE
    { enter_type $3 $2 $5 }
| VARIANT TYPE ParserArity TypeName LBRACE Constructors RBRACE
    { enter_type $4 $3 $6 ~variant: true }
| TYPE ParserArity TypeName EXTERNAL
    { enter_external_type $3 $2 }
| SUBTYPE ParserArity OPTION LPAREN IDENT RPAREN LBRACE AbbrevConstructors RBRACE
    { enter_subtype "options" $2 $5 $8 }
| SUBTYPE ParserArity TypeName LPAREN IDENT RPAREN LBRACE AbbrevConstructors RBRACE
    { enter_subtype $3 $2 $5 $8 }
| Command
    { enter_function $1 }
| WIDGET IDENT LBRACE WidgetComponents RBRACE
    { enter_widget $2 $4 }
| MODULE IDENT LBRACE ModuleComponents RBRACE
    { enter_module (String.uncapitalize $2) $4 }
| EOF
    { raise End_of_file }
;
