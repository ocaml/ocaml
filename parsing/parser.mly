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

/* The parser definition */

%{
open Location
open Asttypes
open Longident
open Parsetree

let mktyp d =
  { ptyp_desc = d; ptyp_loc = symbol_loc() }
let mkpat d =
  { ppat_desc = d; ppat_loc = symbol_loc() }
let mkexp d =
  { pexp_desc = d; pexp_loc = symbol_loc() }
let mkmty d =
  { pmty_desc = d; pmty_loc = symbol_loc() }
let mksig d =
  { psig_desc = d; psig_loc = symbol_loc() }
let mkmod d =
  { pmod_desc = d; pmod_loc = symbol_loc() }
let mkstr d =
  { pstr_desc = d; pstr_loc = symbol_loc() }
let mkfield d =
  { pfield_desc = d; pfield_loc = symbol_loc() }

let mkoperator name pos =
  { pexp_desc = Pexp_ident(Lident name); pexp_loc = rhs_loc pos }

let mkassert e =
  let {loc_start = st; loc_end = en} = symbol_loc () in
  let triple = mkexp (Pexp_tuple
                       [mkexp (Pexp_constant (Const_string !input_name));
                        mkexp (Pexp_constant (Const_int st));
                        mkexp (Pexp_constant (Const_int en))]) in
  let ex = Ldot (Lident "Pervasives", "Assert_failure") in
  let bucket = mkexp (Pexp_construct (ex, Some triple, false)) in
  let ra = Ldot (Lident "Pervasives", "raise") in
  let raiser = mkexp (Pexp_apply (mkexp (Pexp_ident ra), [bucket])) in
  let un = mkexp (Pexp_construct (Lident "()", None, false)) in
  match e with
  | {pexp_desc = Pexp_construct (Lident "false", None, false) } -> raiser
  | _ -> if !Clflags.noassert
         then un
         else mkexp (Pexp_ifthenelse (e, un, Some raiser))
;;

let mklazy e =
  let void_pat = mkpat (Ppat_construct (Lident "()", None, false)) in
  let f = mkexp (Pexp_function ([void_pat, e])) in
  let delayed = Ldot (Lident "Lazy", "Delayed") in
  let df = mkexp (Pexp_construct (delayed, Some f, false)) in
  let r = mkexp (Pexp_ident (Ldot (Lident "Pervasives", "ref"))) in
  mkexp (Pexp_apply (r, [df]))
;;

let mkinfix arg1 name arg2 =
  mkexp(Pexp_apply(mkoperator name 2, [arg1; arg2]))

let mkuminus name arg =
  match arg.pexp_desc with
    Pexp_constant(Const_int n) ->
      mkexp(Pexp_constant(Const_int(-n)))
  | Pexp_constant(Const_float f) ->
      mkexp(Pexp_constant(Const_float("-" ^ f)))
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, [arg]))

let rec mklistexp = function
    [] ->
      mkexp(Pexp_construct(Lident "[]", None, false))
  | e1 :: el ->
      mkexp(Pexp_construct(Lident "::",
                           Some(mkexp(Pexp_tuple[e1; mklistexp el])),
                           false))
let rec mklistpat = function
    [] ->
      mkpat(Ppat_construct(Lident "[]", None, false))
  | p1 :: pl ->
      mkpat(Ppat_construct(Lident "::",
                           Some(mkpat(Ppat_tuple[p1; mklistpat pl])),
                           false))

let mkstrexp e =
  { pstr_desc = Pstr_eval e; pstr_loc = e.pexp_loc }

let array_function str name =
  Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name))

let rec mkrangepat c1 c2 =
  if c1 > c2 then mkrangepat c2 c1 else
  if c1 = c2 then mkpat(Ppat_constant(Const_char c1)) else
  mkpat(Ppat_or(mkpat(Ppat_constant(Const_char c1)),
                mkrangepat (Char.chr(Char.code c1 + 1)) c2))

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_num closing_name closing_num =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(rhs_loc opening_num, opening_name,
                                           rhs_loc closing_num, closing_name)))

%}

/* Tokens */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token CLOSED
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token <int> INT
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token METHOD
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token NEW
%token OF
%token OPEN
%token OR
%token PARSER
%token <string> PREFIXOP
%token PRIVATE
%token PROTECTED
%token QUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token SIG
%token STAR
%token <string> STRING
%token STRUCT
%token <string> SUBTRACTIVE
%token THEN
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH

/* Precedences and associativities. Lower precedences come first. */

%right prec_let                         /* let ... in ... */
%right prec_type_def                    /* = in type definitions */
%right SEMI                             /* e1; e2 (sequence) */
%right prec_fun prec_match prec_try     /* match ... with ... */
%right prec_list                        /* e1; e2 (list, array, record) */
%right prec_if                          /* if ... then ... else ... */
%right COLONEQUAL LESSMINUS             /* assignments */
%left  AS                               /* as in patterns */
%left  BAR                              /* | in patterns */
%left  COMMA                            /* , in expressions, patterns, types */
%right prec_type_arrow                  /* -> in type expressions */
%right OR BARBAR                        /* or */
%right AMPERSAND AMPERAMPER             /* & */
%left  INFIXOP0 EQUAL LESS GREATER      /* = < > etc */
%right INFIXOP1                         /* @ ^ etc */
%right COLONCOLON                       /* :: */
%left  INFIXOP2 SUBTRACTIVE             /* + - */
%left  INFIXOP3 STAR                    /* * / */
%right INFIXOP4                         /* ** */
%right prec_unary_minus                 /* - unary */
%left  prec_appl                        /* function application */
%right prec_constr_appl                 /* constructor application */
%left  SHARP                            /* method call */
%left  DOT                              /* record access, array access */
%right PREFIXOP                         /* ! */

/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation
%start interface                        /* for interface files */
%type <Parsetree.signature> interface
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.toplevel_phrase list> use_file

%%

/* Entry points */

implementation:
    structure EOF                        { $1 }
;
interface:
    signature EOF                        { List.rev $1 }
;
toplevel_phrase:
    top_structure SEMISEMI               { Ptop_def $1 }
  | seq_expr SEMISEMI                    { Ptop_def[mkstrexp $1] }
  | toplevel_directive SEMISEMI          { $1 }
  | EOF                                  { raise End_of_file }
;
top_structure:
    structure_item                       { [$1] }
  | structure_item top_structure         { $1 :: $2 }
;
use_file:
    use_file_tail                        { $1 }
  | seq_expr use_file_tail               { Ptop_def[mkstrexp $1] :: $2 }
;
use_file_tail:
    EOF                                         { [] }
  | SEMISEMI EOF                                { [] }
  | SEMISEMI seq_expr use_file_tail             { Ptop_def[mkstrexp $2] :: $3 }
  | SEMISEMI structure_item use_file_tail       { Ptop_def[$2] :: $3 }
  | SEMISEMI toplevel_directive use_file_tail   { $2 :: $3 }
  | structure_item use_file_tail                { Ptop_def[$1] :: $2 }
  | toplevel_directive use_file_tail            { $1 :: $2 }
;

/* Module expressions */

module_expr:
    mod_longident
      { mkmod(Pmod_ident $1) }
  | STRUCT structure END
      { mkmod(Pmod_structure($2)) }
  | STRUCT structure error
      { unclosed "struct" 1 "end" 3 }
  | FUNCTOR LPAREN UIDENT COLON module_type RPAREN MINUSGREATER module_expr
    %prec prec_fun
      { mkmod(Pmod_functor($3, $5, $8)) }
  | module_expr LPAREN module_expr RPAREN
      { mkmod(Pmod_apply($1, $3)) }
  | module_expr LPAREN module_expr error
      { unclosed "(" 2 ")" 4 }
  | LPAREN module_expr COLON module_type RPAREN
      { mkmod(Pmod_constraint($2, $4)) }
  | LPAREN module_expr COLON module_type error
      { unclosed "(" 1 ")" 5 }
  | LPAREN module_expr RPAREN
      { $2 }
  | LPAREN module_expr error
      { unclosed "(" 1 ")" 3 }
;
structure:
    structure_tail                              { $1 }
  | seq_expr structure_tail                     { mkstrexp $1 :: $2 }
;
structure_tail:
    /* empty */                                 { [] }
  | SEMISEMI                                    { [] }
  | SEMISEMI seq_expr structure_tail            { mkstrexp $2 :: $3 }
  | SEMISEMI structure_item structure_tail      { $2 :: $3 }
  | structure_item structure_tail               { $1 :: $2 }
;
structure_item:
    LET rec_flag let_bindings
      { match $3 with
          [{ppat_desc = Ppat_any}, exp] -> mkstr(Pstr_eval exp)
        | _ -> mkstr(Pstr_value($2, List.rev $3)) }
  | EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
      { mkstr(Pstr_primitive($2, {pval_type = $4; pval_prim = $6})) }
  | TYPE type_declarations
      { mkstr(Pstr_type(List.rev $2)) }
  | EXCEPTION UIDENT constructor_arguments
      { mkstr(Pstr_exception($2, $3)) }
  | MODULE UIDENT module_binding
      { mkstr(Pstr_module($2, $3)) }
  | MODULE TYPE ident EQUAL module_type
      { mkstr(Pstr_modtype($3, $5)) }
  | OPEN mod_longident
      { mkstr(Pstr_open $2) }
  | CLASS class_list END
      { mkstr(Pstr_class (List.rev $2)) }
  | CLASS class_list error
      { unclosed "class" 1 "end" 3 }
;
module_binding:
    EQUAL module_expr
      { $2 }
  | COLON module_type EQUAL module_expr
      { mkmod(Pmod_constraint($4, $2)) }
  | LPAREN UIDENT COLON module_type RPAREN module_binding
      { mkmod(Pmod_functor($2, $4, $6)) }
;

/* Module types */

module_type:
    mty_longident
      { mkmty(Pmty_ident $1) }
  | SIG signature END
      { mkmty(Pmty_signature(List.rev $2)) }
  | SIG signature error
      { unclosed "sig" 1 "end" 3 }
  | FUNCTOR LPAREN UIDENT COLON module_type RPAREN MINUSGREATER module_type
    %prec prec_fun
      { mkmty(Pmty_functor($3, $5, $8)) }
  | module_type WITH with_constraints
      { mkmty(Pmty_with($1, List.rev $3)) }
  | LPAREN module_type RPAREN
      { $2 }
  | LPAREN module_type error
      { unclosed "(" 1 ")" 3 }
;
signature:
    /* empty */                                 { [] }
  | signature signature_item                    { $2 :: $1 }
  | signature signature_item SEMISEMI           { $2 :: $1 }
;
signature_item:
    VAL val_ident COLON core_type
      { mksig(Psig_value($2, {pval_type = $4; pval_prim = []})) }
  | EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
      { mksig(Psig_value($2, {pval_type = $4; pval_prim = $6})) }
  | TYPE type_declarations
      { mksig(Psig_type(List.rev $2)) }
  | EXCEPTION UIDENT constructor_arguments
      { mksig(Psig_exception($2, $3)) }
  | MODULE UIDENT module_declaration
      { mksig(Psig_module($2, $3)) }
  | MODULE TYPE ident
      { mksig(Psig_modtype($3, Pmodtype_abstract)) }
  | MODULE TYPE ident EQUAL module_type
      { mksig(Psig_modtype($3, Pmodtype_manifest $5)) }
  | OPEN mod_longident
      { mksig(Psig_open $2) }
  | INCLUDE module_type
      { mksig(Psig_include $2) }
  | CLASS class_type_list END
      { mksig(Psig_class (List.rev $2)) }
;

module_declaration:
    COLON module_type
      { $2 }
  | LPAREN UIDENT COLON module_type RPAREN module_declaration
      { mkmty(Pmty_functor($2, $4, $6)) }
;

/* Core expressions */

seq_expr:
  | expr                          { $1 }
  | expr SEMI                     { $1 }
  | expr SEMI seq_expr            { mkexp(Pexp_sequence($1, $3)) }
;
expr:
    simple_expr
      { $1 }
  | simple_expr simple_expr_list %prec prec_appl
      { mkexp(Pexp_apply($1, List.rev $2)) }
  | LET rec_flag let_bindings IN seq_expr %prec prec_let
      { mkexp(Pexp_let($2, List.rev $3, $5)) }
  | LET rec_flag let_bindings IN error %prec prec_let
      { syntax_error() }
  | LET rec_flag let_bindings error %prec prec_let
      { unclosed "let" 1 "in" 4 }
  | PARSER opt_pat opt_bar parser_cases %prec prec_fun
      { Pstream.cparser ($2, List.rev $4) }
  | FUNCTION opt_bar match_cases %prec prec_fun
      { mkexp(Pexp_function(List.rev $3)) }
  | FUN simple_pattern fun_def %prec prec_fun
      { mkexp(Pexp_function([$2, $3])) }
  | MATCH seq_expr WITH opt_bar match_cases %prec prec_match
      { mkexp(Pexp_match($2, List.rev $5)) }
  | MATCH seq_expr WITH PARSER opt_pat opt_bar parser_cases %prec prec_match
      { mkexp(Pexp_apply(Pstream.cparser ($5, List.rev $7), [$2])) }
  | TRY seq_expr WITH opt_bar match_cases %prec prec_try
      { mkexp(Pexp_try($2, List.rev $5)) }
  | TRY seq_expr WITH error %prec prec_try
      { syntax_error() }
  | TRY seq_expr error %prec prec_try
      { unclosed "try" 1 "with" 3 }
  | expr_comma_list
      { mkexp(Pexp_tuple(List.rev $1)) }
  | constr_longident simple_expr %prec prec_constr_appl
      { mkexp(Pexp_construct($1, Some $2, false)) }
  | IF seq_expr THEN expr ELSE expr %prec prec_if
      { mkexp(Pexp_ifthenelse($2, $4, Some $6)) }
  | IF seq_expr THEN expr %prec prec_if
      { mkexp(Pexp_ifthenelse($2, $4, None)) }
  | WHILE seq_expr DO seq_expr DONE
      { mkexp(Pexp_while($2, $4)) }
  | WHILE seq_expr DO seq_expr error
      { unclosed "while" 1 "done" 5 }
  | FOR val_ident EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
      { mkexp(Pexp_for($2, $4, $6, $5, $8)) }
  | FOR val_ident EQUAL seq_expr direction_flag seq_expr DO seq_expr error
      { unclosed "for" 1 "done" 9 }
  | expr COLONCOLON expr
      { mkexp(Pexp_construct(Lident "::", Some(mkexp(Pexp_tuple[$1;$3])), false)) }
  | expr INFIXOP0 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP1 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP2 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP3 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP4 expr
      { mkinfix $1 $2 $3 }
  | expr SUBTRACTIVE expr
      { mkinfix $1 $2 $3 } 
  | expr STAR expr
      { mkinfix $1 "*" $3 } 
  | expr EQUAL expr
      { mkinfix $1 "=" $3 } 
  | expr LESS expr
      { mkinfix $1 "<" $3 } 
  | expr GREATER expr
      { mkinfix $1 ">" $3 } 
  | expr OR expr
      { mkinfix $1 "or" $3 }
  | expr BARBAR expr
      { mkinfix $1 "||" $3 }
  | expr AMPERSAND expr
      { mkinfix $1 "&" $3 }
  | expr AMPERAMPER expr
      { mkinfix $1 "&&" $3 }
  | expr COLONEQUAL expr
      { mkinfix $1 ":=" $3 }
  | SUBTRACTIVE expr %prec prec_unary_minus
      { mkuminus $1 $2 }
  | simple_expr DOT label_longident LESSMINUS expr
      { mkexp(Pexp_setfield($1, $3, $5)) }
  | simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS expr
      { mkexp(Pexp_apply(mkexp(Pexp_ident(array_function "Array" "set")),
                         [$1; $4; $7])) }
  | simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS expr
      { mkexp(Pexp_apply(mkexp(Pexp_ident(array_function "String" "set")),
                         [$1; $4; $7])) }
  | label LESSMINUS expr
      { mkexp(Pexp_setinstvar($1, $3)) }
  | ASSERT simple_expr %prec prec_appl
      { mkassert $2 }
  | LAZY simple_expr %prec prec_appl
      { mklazy $2 }
;
simple_expr:
    val_longident
      { mkexp(Pexp_ident $1) }
  | constant
      { mkexp(Pexp_constant $1) }
  | constr_longident
      { mkexp(Pexp_construct($1, None, false)) }
  | LPAREN seq_expr RPAREN
      { $2 }
  | LPAREN seq_expr error
      { unclosed "(" 1 ")" 3 }
  | BEGIN seq_expr END
      { $2 }
  | BEGIN seq_expr error
      { unclosed "begin" 1 "end" 3 }
  | LPAREN seq_expr type_constraint RPAREN
      { let (t, t') = $3 in mkexp(Pexp_constraint($2, t, t')) }
  | simple_expr DOT label_longident
      { mkexp(Pexp_field($1, $3)) }
  | simple_expr DOT LPAREN seq_expr RPAREN
      { mkexp(Pexp_apply(mkexp(Pexp_ident(array_function "Array" "get")),
                         [$1; $4])) }
  | simple_expr DOT LPAREN seq_expr error
      { unclosed "(" 3 ")" 5 }
  | simple_expr DOT LBRACKET seq_expr RBRACKET
      { mkexp(Pexp_apply(mkexp(Pexp_ident(array_function "String" "get")),
                         [$1; $4])) }
  | simple_expr DOT LBRACKET seq_expr error
      { unclosed "[" 3 "]" 5 }
  | LBRACE lbl_expr_list opt_semi RBRACE
      { mkexp(Pexp_record(List.rev $2)) }
  | LBRACE lbl_expr_list opt_semi error
      { unclosed "{" 1 "}" 4 }
  | LBRACKETLESS stream_expr opt_semi GREATERRBRACKET
      { Pstream.cstream (List.rev $2) }
  | LBRACKETLESS stream_expr opt_semi error
      { unclosed "[<" 1 ">]" 4 }
  | LBRACKETLESS GREATERRBRACKET
      { Pstream.cstream [] }
  | LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { mkexp(Pexp_array(List.rev $2)) }
  | LBRACKETBAR expr_semi_list opt_semi error
      { unclosed "[|" 1 "|]" 4 }
  | LBRACKETBAR BARRBRACKET
      { mkexp(Pexp_array []) }
  | LBRACKET expr_semi_list opt_semi RBRACKET
      { mklistexp(List.rev $2) }
  | LBRACKET expr_semi_list opt_semi error
      { unclosed "[" 1 "]" 4 }
  | PREFIXOP simple_expr
      { mkexp(Pexp_apply(mkoperator $1 1, [$2])) }
  | simple_expr SHARP label
      { mkexp(Pexp_send($1, $3)) }
  | NEW class_longident
      { mkexp(Pexp_new($2)) }
  | LBRACELESS label_expr_list opt_semi GREATERRBRACE
      { mkexp(Pexp_override(List.rev $2)) }
  | LBRACELESS label_expr_list opt_semi error
      { unclosed "{<" 1 ">}" 4 }
  | LBRACELESS GREATERRBRACE
      { mkexp(Pexp_override []) }
  | LPAREN SHARP label RPAREN
      { mkexp(Pexp_function [mkpat(Ppat_var "x"),
                mkexp(Pexp_send(mkexp(Pexp_ident (Lident"x")), $3))]) }
;
simple_expr_list:
    simple_expr
      { [$1] }
  | simple_expr_list simple_expr
      { $2 :: $1 }
;
let_bindings:
    let_binding                                 { [$1] }
  | let_bindings AND let_binding                { $3 :: $1 }
;
let_binding:
    val_ident fun_binding
      { ({ppat_desc = Ppat_var $1; ppat_loc = rhs_loc 1}, $2) }
  | pattern EQUAL seq_expr %prec prec_let
      { ($1, $3) }
;
fun_binding:
    EQUAL seq_expr %prec prec_let
      { $2 }
  | type_constraint EQUAL seq_expr %prec prec_let
      { let (t, t') = $1 in mkexp(Pexp_constraint($3, t, t')) }
  | simple_pattern fun_binding
      { mkexp(Pexp_function[$1,$2]) }
;
parser_cases:
    parser_case                                 { [$1] }
  | parser_cases BAR parser_case                { $3 :: $1 }
;
parser_case:
    LBRACKETLESS stream_pattern opt_semi GREATERRBRACKET opt_pat
    MINUSGREATER seq_expr
      { (List.rev $2, $5, $7) }
  | LBRACKETLESS stream_pattern opt_semi error
      { unclosed "[<" 1 ">]" 4 }
  | LBRACKETLESS GREATERRBRACKET opt_pat MINUSGREATER seq_expr
      { ([], $3, $5) }
;
stream_pattern:
    stream_pattern_component opt_err                     { [($1, $2)] }
  | stream_pattern SEMI stream_pattern_component opt_err { ($3, $4) :: $1 }
;
stream_pattern_component:
    QUOTE pattern
      { Pstream.Spat_term ($2, None) }
  | QUOTE pattern WHEN expr %prec prec_list
      { Pstream.Spat_term ($2, Some $4) }
  | pattern EQUAL expr
      { Pstream.Spat_nterm ($1, $3) }
  | pattern
      { Pstream.Spat_sterm $1 }
;
opt_pat:
    /* empty */                                 { None }
  | simple_pattern                              { Some $1 }
;
opt_err:
    /* empty */                                 { None }
  | QUESTION expr %prec prec_list               { Some $2 }
;
stream_expr:
    stream_expr_component                       { [$1] }
  | stream_expr SEMI stream_expr_component      { $3 :: $1 }
;
stream_expr_component:
    QUOTE expr %prec prec_list                  { Pstream.Sexp_term $2 }
  | expr %prec prec_list                        { Pstream.Sexp_nterm $1 }
;
match_cases:
    pattern match_action                        { [$1, $2] }
  | match_cases BAR pattern match_action        { ($3, $4) :: $1 }
;
fun_def:
    match_action                                { $1 }
  | simple_pattern fun_def                      { mkexp(Pexp_function[$1,$2]) }
;
match_action:
    MINUSGREATER seq_expr                       { $2 }
  | WHEN seq_expr MINUSGREATER seq_expr         { mkexp(Pexp_when($2, $4)) }
;
expr_comma_list:
    expr_comma_list COMMA expr                  { $3 :: $1 }
  | expr COMMA expr                             { [$3; $1] }
;
lbl_expr_list:
    label_longident EQUAL expr %prec prec_list
      { [$1,$3] }
  | lbl_expr_list SEMI label_longident EQUAL expr %prec prec_list
      { ($3, $5) :: $1 }
;
label_expr_list:
    label EQUAL expr %prec prec_list
      { [$1,$3] }
  | label_expr_list SEMI label EQUAL expr %prec prec_list
      { ($3, $5) :: $1 }
;
expr_semi_list:
    expr %prec prec_list                        { [$1] }
  | expr_semi_list SEMI expr %prec prec_list    { $3 :: $1 }
;
type_constraint:
    COLON core_type                             { (Some $2, None) }
  | COLON core_type COLONGREATER core_type      { (Some $2, Some $4) }
  | COLONGREATER core_type                      { (None, Some $2) }
  | COLON error                                 { syntax_error() }
  | COLONGREATER error                          { syntax_error() }
;

/* Patterns */

pattern:
    simple_pattern
      { $1 }
  | pattern AS val_ident
      { mkpat(Ppat_alias($1, $3)) }
  | pattern_comma_list
      { mkpat(Ppat_tuple(List.rev $1)) }
  | constr_longident pattern %prec prec_constr_appl
      { mkpat(Ppat_construct($1, Some $2, false)) }
  | pattern COLONCOLON pattern
      { mkpat(Ppat_construct(Lident "::", Some(mkpat(Ppat_tuple[$1;$3])),
                             false)) }
  | pattern BAR pattern
      { mkpat(Ppat_or($1, $3)) }
;
simple_pattern:
    val_ident
      { mkpat(Ppat_var $1) }
  | UNDERSCORE
      { mkpat(Ppat_any) }
  | signed_constant
      { mkpat(Ppat_constant $1) }
  | CHAR DOTDOT CHAR
      { mkrangepat $1 $3 }
  | constr_longident
      { mkpat(Ppat_construct($1, None, false)) }
  | LBRACE lbl_pattern_list opt_semi RBRACE
      { mkpat(Ppat_record(List.rev $2)) }
  | LBRACE lbl_pattern_list opt_semi error
      { unclosed "{" 1 "}" 4 }
  | LBRACKET pattern_semi_list opt_semi RBRACKET
      { mklistpat(List.rev $2) }
  | LBRACKET pattern_semi_list opt_semi error
      { unclosed "{" 1 "}" 4 }
  | LPAREN pattern RPAREN
      { $2 }
  | LPAREN pattern error
      { unclosed "(" 1 ")" 3 }
  | LPAREN pattern COLON core_type RPAREN
      { mkpat(Ppat_constraint($2, $4)) }
  | LPAREN pattern COLON core_type error
      { unclosed "(" 1 ")" 5 }
;

pattern_comma_list:
    pattern_comma_list COMMA pattern            { $3 :: $1 }
  | pattern COMMA pattern                       { [$3; $1] }
;
pattern_semi_list:
    pattern                                     { [$1] }
  | pattern_semi_list SEMI pattern              { $3 :: $1 }
;
lbl_pattern_list:
    label_longident EQUAL pattern               { [($1, $3)] }
  | lbl_pattern_list SEMI label_longident EQUAL pattern { ($3, $5) :: $1 }
;

/* Primitive declarations */

primitive_declaration:
    STRING                                      { [$1] }
  | STRING primitive_declaration                { $1 :: $2 }
;

/* Class definitions */

class_list:
        class_list AND class_def                { $3 :: $1 }
      | class_def                               { [$1] }
;
class_def:
        virtual_flag closed_flag
        class_type_parameters LIDENT simple_pattern_list self self_type EQUAL
        constraints class_fields
          { { pcl_name = $4; pcl_param = $3; pcl_args = List.rev $5;
              pcl_self = $6; pcl_self_ty = $7; pcl_cstr = List.rev $9;
              pcl_field = List.rev $10;
              pcl_kind = $1; pcl_closed = $2;
              pcl_loc = symbol_loc () } }
;
class_type_parameters:
        type_parameters                         { $1, symbol_loc () }
;
simple_pattern_list:
        simple_pattern                          { [$1] }
      | simple_pattern_list simple_pattern      { $2::$1 }
;
self:
        AS LIDENT                               { Some $2 }
      | /* empty */                             { None }
;
class_fields:
        /* empty */                             { [] }
      | class_fields INHERIT ancestor           { Pcf_inher $3 :: $1 }
      | class_fields VAL value                  { Pcf_val $3 :: $1 }
      | class_fields virtual_method             { Pcf_virt $2 :: $1 }
      | class_fields method_def                 { Pcf_meth $2 :: $1 }
;
ancestor:
        LPAREN core_type_comma_list RPAREN class_longident simple_expr_list
        self
          { $4, List.rev $2, List.rev $5, $6, symbol_loc () }
      | LPAREN core_type RPAREN class_longident simple_expr_list self
          { $4, [$2], List.rev $5, $6, symbol_loc () }
      | class_longident simple_expr_list self
          { $1, [], List.rev $2, $3, symbol_loc () }
;
value:
        private_flag mutable_flag label EQUAL seq_expr
          { $3, $1, $2, Some $5, symbol_loc () }
      | private_flag mutable_flag label
          { $3, $1, $2, None, symbol_loc () }
;
virtual_method:
        VIRTUAL protected_flag label COLON core_type
          { $3, $2, $5, symbol_loc () }

method_def :
        METHOD protected_flag label fun_binding
          { $3, $2, $4, symbol_loc () }
;

/* Class type definitions */

class_type_list:
        class_type_list AND class_type          { $3 :: $1 }
      | class_type                              { [$1] }
;
class_type:
        virtual_flag closed_flag class_type_parameters LIDENT type_list
        self_type
        EQUAL
        constraints class_type_fields
          { { pcty_name = $4; pcty_param = $3; pcty_args = $5;
              pcty_self = $6; pcty_cstr = List.rev $8;
              pcty_field = List.rev $9;
              pcty_kind = $1; pcty_closed = $2;
              pcty_loc = symbol_loc () } }
;
type_list:
        LPAREN core_type RPAREN type_list       { $2 :: $4 }
      | LPAREN core_type RPAREN                 { [$2] }
;
self_type:
        COLON type_parameter                    { Some $2 }
      | /* empty */                             { None }
;
constraints:
        constraints CONSTRAINT constrain        { $3 :: $1 }
      | /* empty */                             { [] }
;
constrain:
        type_parameter EQUAL core_type          { $1, $3, symbol_loc () }
;
class_type_fields:
        /* empty */                             { [] }
      | class_type_fields INHERIT ancestor_type { Pctf_inher $3 :: $1 }
      | class_type_fields VAL value_type        { Pctf_val $3 :: $1 }
      | class_type_fields virtual_method        { Pctf_virt $2 :: $1 }
      | class_type_fields method_type           { Pctf_meth $2 :: $1 }
;
ancestor_type:
        LPAREN core_type_comma_list RPAREN class_longident
          { $4, List.rev $2, symbol_loc () }
      | LPAREN core_type RPAREN class_longident
          { $4, [$2], symbol_loc () }
      | class_longident
          { $1, [], symbol_loc () }
;
value_type :
        private_flag mutable_flag label COLON core_type
          { $3, $1, $2, Some $5, symbol_loc () }
      | private_flag mutable_flag label
          { $3, $1, $2, None, symbol_loc () }
;
method_type :
        METHOD protected_flag label COLON core_type
          { $3, $2, $5, symbol_loc () }
      | METHOD protected_flag label
          { $3, $2, mktyp(Ptyp_any), symbol_loc () }
;

/* Type declarations */

type_declarations:
    type_declaration                            { [$1] }
  | type_declarations AND type_declaration      { $3 :: $1 }
;
type_declaration:
    type_parameters LIDENT type_kind constraints
      { let (kind, manifest) = $3 in
        ($2, {ptype_params = $1;
              ptype_cstrs = List.rev $4;
              ptype_kind = kind;
              ptype_manifest = manifest;
              ptype_loc = symbol_loc()}) }
;
type_kind:
    /*empty*/
      { (Ptype_abstract, None) }
  | EQUAL core_type %prec prec_type_def
      { (Ptype_abstract, Some $2) }
  | EQUAL constructor_declarations
      { (Ptype_variant(List.rev $2), None) }
  | EQUAL BAR constructor_declarations
      { (Ptype_variant(List.rev $3), None) }
  | EQUAL LBRACE label_declarations opt_semi RBRACE
      { (Ptype_record(List.rev $3), None) }
  | EQUAL core_type EQUAL opt_bar constructor_declarations %prec prec_type_def
      { (Ptype_variant(List.rev $5), Some $2) }
  | EQUAL core_type EQUAL LBRACE label_declarations opt_semi RBRACE
    %prec prec_type_def
      { (Ptype_record(List.rev $5), Some $2) }
;
type_parameters:
    /*empty*/                                   { [] }
  | type_parameter                              { [$1] }
  | LPAREN type_parameter_list RPAREN           { List.rev $2 }
;
type_parameter:
    QUOTE ident                                 { $2 }
;
type_parameter_list:
    type_parameter                              { [$1] }
  | type_parameter_list COMMA type_parameter    { $3 :: $1 }
;
constructor_declarations:
    constructor_declaration                     { [$1] }
  | constructor_declarations BAR constructor_declaration { $3 :: $1 }
;
constructor_declaration:
    constr_ident constructor_arguments          { ($1, $2) }
;
constructor_arguments:
    /*empty*/                                   { [] }
  | OF core_type_list                           { List.rev $2 }
;
label_declarations:
    label_declaration                           { [$1] }
  | label_declarations SEMI label_declaration   { $3 :: $1 }
;
label_declaration:
    mutable_flag LIDENT COLON core_type         { ($2, $1, $4) }
;

/* "with" constraints (additional type equations over signature components) */

with_constraints:
    with_constraint                             { [$1] }
  | with_constraints AND with_constraint        { $3 :: $1 }
;
with_constraint:
    TYPE type_parameters label_longident EQUAL core_type constraints
      { ($3, Pwith_type {ptype_params = $2;
                         ptype_cstrs = List.rev $6;
                         ptype_kind = Ptype_abstract;
                         ptype_manifest = Some $5;
                         ptype_loc = symbol_loc()}) }
    /* used label_longident instead of type_longident to disallow
       functor applications in type path */
  | MODULE mod_longident EQUAL mod_ext_longident
      { ($2, Pwith_module $4) }
;

/* Core types */

core_type:
    simple_core_type
      { $1 }
  | core_type MINUSGREATER core_type %prec prec_type_arrow
      { mktyp(Ptyp_arrow($1, $3)) }
  | core_type_tuple
      { mktyp(Ptyp_tuple(List.rev $1)) }
  | core_type AS type_parameter
      { mktyp(Ptyp_alias($1, $3)) }
;

simple_core_type:
    QUOTE ident
      { mktyp(Ptyp_var $2) }
  | UNDERSCORE
      { mktyp(Ptyp_any) }
  | type_longident
      { mktyp(Ptyp_constr($1, [])) }
  | simple_core_type type_longident %prec prec_constr_appl
      { mktyp(Ptyp_constr($2, [$1])) }
  | LPAREN core_type_comma_list RPAREN type_longident
      %prec prec_constr_appl
      { mktyp(Ptyp_constr($4, List.rev $2)) }
  | LPAREN core_type RPAREN
      { $2 }
  | LESS meth_list GREATER
      { mktyp(Ptyp_object $2) }
  | LESS GREATER
      { mktyp(Ptyp_object []) }
  | SHARP class_longident
      { mktyp(Ptyp_class($2, [])) }
  | simple_core_type SHARP class_longident %prec prec_constr_appl
      { mktyp(Ptyp_class($3, [$1])) }
  | LPAREN core_type_comma_list RPAREN SHARP class_longident
      %prec prec_constr_appl
      { mktyp(Ptyp_class($5, List.rev $2)) }
;
core_type_tuple:
    simple_core_type STAR simple_core_type      { [$3; $1] }
  | core_type_tuple STAR simple_core_type       { $3 :: $1 }
;
core_type_comma_list:
    core_type COMMA core_type                   { [$3; $1] }
  | core_type_comma_list COMMA core_type        { $3 :: $1 }
;
core_type_list:
    simple_core_type                            { [$1] }
  | core_type_list STAR simple_core_type        { $3 :: $1 }
;
meth_list:
    field SEMI meth_list                        { $1 :: $3 }
  | field opt_semi                              { [$1] }
  | DOTDOT                                      { [mkfield Pfield_var] }
;
field:
    label COLON core_type                       { mkfield(Pfield($1, $3)) }
;
label:
    LIDENT                                      { $1 }
;

/* Constants */

constant:
    INT                                         { Const_int $1 }
  | CHAR                                        { Const_char $1 }
  | STRING                                      { Const_string $1 }
  | FLOAT                                       { Const_float $1 }
;
signed_constant:
    constant                                    { $1 }
  | SUBTRACTIVE INT                             { Const_int(- $2) }
  | SUBTRACTIVE FLOAT                           { Const_float("-" ^ $2) }
;
/* Identifiers and long identifiers */

ident:
    UIDENT                                      { $1 }
  | LIDENT                                      { $1 }
;
val_ident:
    LIDENT                                      { $1 }
  | LPAREN operator RPAREN                      { $2 }
;
operator:
    PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | SUBTRACTIVE                                 { $1 }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | BARBAR                                      { "||" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }
;
constr_ident:
    UIDENT                                      { $1 }
  | LBRACKET RBRACKET                           { "[]" }
  | LPAREN RPAREN                               { "()" }
  | COLONCOLON                                  { "::" }
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;
    
val_longident:
    val_ident                                   { Lident $1 }
  | mod_longident DOT val_ident                 { Ldot($1, $3) }
;
constr_longident:
    mod_longident                               { $1 }
  | LBRACKET RBRACKET                           { Lident "[]" }
  | LPAREN RPAREN                               { Lident "()" }
  | FALSE                                       { Lident "false" }
  | TRUE                                        { Lident "true" }
;
label_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;
type_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
mod_longident:
    UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }
;
mod_ext_longident:
    UIDENT                                      { Lident $1 }
  | mod_ext_longident DOT UIDENT                { Ldot($1, $3) }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN { Lapply($1, $3) }
;
mty_longident:
    ident                                       { Lident $1 }
  | mod_ext_longident DOT ident                 { Ldot($1, $3) }
;
class_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;

/* Toplevel directives */

toplevel_directive:
    SHARP ident                 { Ptop_dir($2, Pdir_none) }
  | SHARP ident STRING          { Ptop_dir($2, Pdir_string $3) }
  | SHARP ident INT             { Ptop_dir($2, Pdir_int $3) }
  | SHARP ident val_longident   { Ptop_dir($2, Pdir_ident $3) }
;

/* Miscellaneous */

rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
private_flag:
    /* empty */                                 { Public }
  | PRIVATE                                     { Private }
;
protected_flag:
    /* empty */                                 { Public }
  | PROTECTED                                   { Private }
;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;
virtual_flag:
    /* empty */                                 { Concrete }
  | VIRTUAL                                     { Virtual }
;
closed_flag:
    /* empty */                                 { Open }
  | CLOSED                                      { Closed }
;
opt_bar:
    /* empty */                                 { () }
  | BAR                                         { () }
;
opt_semi:
  | /* empty */                                 { () }
  | SEMI                                        { () }
;
%%
