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
let mkmod d =
  { pmod_desc = d; pmod_loc = symbol_loc() }

let mkoperator name pos =
  { pexp_desc = Pexp_ident(Lident name); pexp_loc = rhs_loc pos }

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
      mkexp(Pexp_construct(Lident "[]", None))
  | e1 :: el ->
      mkexp(Pexp_construct(Lident "::",
                           Some(mkexp(Pexp_tuple[e1; mklistexp el]))))
let rec mklistpat = function
    [] ->
      mkpat(Ppat_construct(Lident "[]", None))
  | p1 :: pl ->
      mkpat(Ppat_construct(Lident "::",
                           Some(mkpat(Ppat_tuple[p1; mklistpat pl]))))

let array_function str name =
  Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name))

let rec mkrangepat c1 c2 =
  if c1 > c2 then mkrangepat c2 c1 else
  if c1 = c2 then mkpat(Ppat_constant(Const_char c1)) else
  mkpat(Ppat_or(mkpat(Ppat_constant(Const_char c1)),
                mkrangepat (Char.chr(Char.code c1 + 1)) c2))
%}

/* Tokens */

%token AMPERSAND
%token AND
%token AS
%token BAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COMMA
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOTLBRACKET
%token DOTLPAREN
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
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token <int> INT
%token LBRACE
%token LBRACKET
%token LBRACKETBAR
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token OF
%token OPEN
%token OR
%token <string> PREFIXOP
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
%token WHEN
%token WHILE
%token WITH

/* Precedences and associativities. Lower precedences come first. */

%right prec_let                         /* let ... in ... */
%right SEMI                             /* e1; e2 (sequence) */
%right prec_fun prec_match prec_try     /* match ... with ... */
%right prec_list                        /* e1; e2 (list, array, record) */
%right prec_if                          /* if ... then ... else ... */
%right COLONEQUAL LESSMINUS             /* assignments */
%left  AS                               /* as in patterns */
%left  BAR                              /* | in patterns */
%left  COMMA                            /* , in expressions, patterns, types */
%right prec_type_arrow                  /* -> in type expressions */
%right OR                               /* or */
%right AMPERSAND                        /* & */
%left  INFIXOP1 EQUAL                   /* = < > etc */
%right COLONCOLON                       /* :: */
%left  INFIXOP2 SUBTRACTIVE             /* + - */
%left  INFIXOP3 STAR                    /* * / */
%right INFIXOP4                         /* ** */
%right prec_unary_minus                 /* - unary */
%left  prec_appl                        /* function application */
%right prec_constr_appl                 /* constructor application */
%left  DOT DOTLPAREN DOTLBRACKET        /* record access, array access */
%right PREFIXOP                         /* ! */

/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation
%start interface                        /* for interface files */
%type <Parsetree.signature> interface
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase

%%

/* Entry points */

implementation:
    structure EOF                        { List.rev $1 }
;
interface:
    signature EOF                        { List.rev $1 }
;
toplevel_phrase:
    structure_item SEMISEMI              { Ptop_def[$1] }
  | expr SEMISEMI                        { Ptop_def[Pstr_eval($1)] }
  | SHARP ident SEMISEMI                 { Ptop_dir($2, Pdir_none) }
  | SHARP ident STRING SEMISEMI          { Ptop_dir($2, Pdir_string $3) }
  | SHARP ident INT SEMISEMI             { Ptop_dir($2, Pdir_int $3) }
  | SHARP ident val_longident SEMISEMI   { Ptop_dir($2, Pdir_ident $3) }
  | EOF                                  { raise End_of_file }
;

/* Module expressions */

module_expr:
    mod_longident
      { mkmod(Pmod_ident $1) }
  | STRUCT structure END
      { mkmod(Pmod_structure(List.rev $2)) }
  | FUNCTOR LPAREN UIDENT COLON module_type RPAREN MINUSGREATER module_expr
    %prec prec_fun
      { mkmod(Pmod_functor($3, $5, $8)) }
  | module_expr module_expr %prec prec_appl
      { mkmod(Pmod_apply($1, $2)) }
  | LPAREN module_expr COLON module_type RPAREN
      { mkmod(Pmod_constraint($2, $4)) }
  | LPAREN module_expr RPAREN
      { $2 }
;
structure:
    /* empty */                                 { [] }
  | structure structure_item                    { $2 :: $1 }
;
structure_item:
    LET UNDERSCORE EQUAL expr
      { Pstr_eval $4 }
  | LET rec_flag let_bindings
      { Pstr_value($2, List.rev $3) }
  | EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
      { Pstr_primitive($2, {pval_type = $4; pval_prim = $6}) }
  | TYPE type_declarations
      { Pstr_type(List.rev $2) }
  | EXCEPTION UIDENT constructor_arguments
      { Pstr_exception($2, $3) }
  | MODULE UIDENT module_binding
      { Pstr_module($2, $3) }
  | MODULE TYPE ident EQUAL module_type
      { Pstr_modtype($3, $5) }
  | OPEN mod_longident
      { Pstr_open($2, rhs_loc 2) }
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
  | FUNCTOR LPAREN UIDENT COLON module_type RPAREN MINUSGREATER module_type
    %prec FUNCTOR
      { mkmty(Pmty_functor($3, $5, $8)) }
  | module_type WITH type_declarations
      { mkmty(Pmty_with($1, List.rev $3)) }
  | LPAREN module_type RPAREN
      { $2 }
;
signature:
    /* empty */                                 { [] }
  | signature signature_item                    { $2 :: $1 }
;
signature_item:
    VAL val_ident COLON core_type
      { Psig_value($2, {pval_type = $4; pval_prim = []}) }
  | EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
      { Psig_value($2, {pval_type = $4; pval_prim = $6}) }
  | TYPE type_declarations
      { Psig_type(List.rev $2) }
  | EXCEPTION UIDENT constructor_arguments
      { Psig_exception($2, $3) }
  | MODULE UIDENT module_declaration
      { Psig_module($2, $3) }
  | MODULE TYPE ident
      { Psig_modtype($3, Pmodtype_abstract) }
  | MODULE TYPE ident EQUAL module_type
      { Psig_modtype($3, Pmodtype_manifest $5) }
  | OPEN mod_longident
      { Psig_open($2, rhs_loc 2) }
  | INCLUDE module_type
      { Psig_include $2 }
;

module_declaration:
    COLON module_type
      { $2 }
  | LPAREN UIDENT COLON module_type RPAREN module_declaration
      { mkmty(Pmty_functor($2, $4, $6)) }
;

/* Core expressions */

expr:
    simple_expr
      { $1 }
  | simple_expr simple_expr_list %prec prec_appl
      { mkexp(Pexp_apply($1, List.rev $2)) }
  | LET rec_flag let_bindings IN expr %prec prec_let
      { mkexp(Pexp_let($2, List.rev $3, $5)) }
  | FUNCTION match_cases %prec prec_fun
      { mkexp(Pexp_function(List.rev $2)) }
  | FUN pattern fun_def %prec prec_fun
      { mkexp(Pexp_function([$2, $3])) }
  | MATCH expr WITH match_cases %prec prec_match
      { mkexp(Pexp_match($2, List.rev $4)) }
  | TRY expr WITH match_cases %prec prec_try
      { mkexp(Pexp_try($2, List.rev $4)) }
  | expr_comma_list
      { mkexp(Pexp_tuple(List.rev $1)) }
  | constr_longident simple_expr %prec prec_constr_appl
      { mkexp(Pexp_construct($1, Some $2)) }
  | IF expr THEN expr ELSE expr %prec prec_if
      { mkexp(Pexp_ifthenelse($2, $4, Some $6)) }
  | IF expr THEN expr %prec prec_if
      { mkexp(Pexp_ifthenelse($2, $4, None)) }
  | expr SEMI expr
      { mkexp(Pexp_sequence($1, $3)) }
  | WHILE expr DO expr DONE
      { mkexp(Pexp_while($2, $4)) }
  | FOR val_ident EQUAL expr direction_flag expr DO expr DONE
      { mkexp(Pexp_for($2, $4, $6, $5, $8)) }
  | expr COLONCOLON expr
      { mkexp(Pexp_construct(Lident "::", Some(mkexp(Pexp_tuple[$1;$3])))) }
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
  | expr OR expr
      { mkinfix $1 "or" $3 }
  | expr AMPERSAND expr
      { mkinfix $1 "&" $3 }
  | expr COLONEQUAL expr
      { mkinfix $1 ":=" $3 }
  | SUBTRACTIVE expr %prec prec_unary_minus
      { mkuminus $1 $2 }
  | simple_expr DOT label_longident LESSMINUS expr
      { mkexp(Pexp_setfield($1, $3, $5)) }
  | simple_expr DOTLPAREN expr RPAREN LESSMINUS expr
      { mkexp(Pexp_apply(mkexp(Pexp_ident(array_function "Array" "set")),
                         [$1; $3; $6])) }
  | simple_expr DOTLBRACKET expr RBRACKET LESSMINUS expr
      { mkexp(Pexp_apply(mkexp(Pexp_ident(array_function "String" "set")),
                         [$1; $3; $6])) }
;
simple_expr:
    val_longident
      { mkexp(Pexp_ident $1) }
  | constant
      { mkexp(Pexp_constant $1) }
  | constr_longident
      { mkexp(Pexp_construct($1, None)) }
  | LPAREN expr RPAREN
      { $2 }
  | BEGIN expr END
      { $2 }
  | LPAREN expr COLON core_type RPAREN
      { mkexp(Pexp_constraint($2, $4)) }
  | simple_expr DOT label_longident
      { mkexp(Pexp_field($1, $3)) }
  | simple_expr DOTLPAREN expr RPAREN
      { mkexp(Pexp_apply(mkexp(Pexp_ident(array_function "Array" "get")),
                         [$1; $3])) }
  | simple_expr DOTLBRACKET expr RBRACKET
      { mkexp(Pexp_apply(mkexp(Pexp_ident(array_function "String" "get")),
                         [$1; $3])) }
  | LBRACE lbl_expr_list RBRACE
      { mkexp(Pexp_record(List.rev $2)) }
  | LBRACKETBAR expr_semi_list BARRBRACKET
      { mkexp(Pexp_array(List.rev $2)) }
  | LBRACKETBAR BARRBRACKET
      { mkexp(Pexp_array []) }
  | LBRACKET expr_semi_list RBRACKET
      { mklistexp(List.rev $2) }
  | PREFIXOP simple_expr
      { mkexp(Pexp_apply(mkoperator $1 1, [$2])) }
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
  | let_pattern EQUAL expr
      { ($1, $3) }
;
fun_binding:
    EQUAL expr %prec prec_let
      { $2 }
  | COLON core_type EQUAL expr %prec prec_let
      { mkexp(Pexp_constraint($4,$2)) }
  | pattern fun_binding
      { mkexp(Pexp_function[$1,$2]) }
;
match_cases:
    pattern match_action                        { [$1, $2] }
  | match_cases BAR pattern match_action        { ($3, $4) :: $1 }
;
fun_def:
    match_action                                { $1 }
  | pattern fun_def                             { mkexp(Pexp_function[$1,$2]) }
;
match_action:
    MINUSGREATER expr                           { $2 }
  | WHEN expr MINUSGREATER expr                 { mkexp(Pexp_when($2, $4)) }
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
expr_semi_list:
    expr %prec prec_list                        { [$1] }
  | expr_semi_list SEMI expr %prec prec_list    { $3 :: $1 }
;

/* Patterns */

pattern:
    val_ident
      { mkpat(Ppat_var $1) }
  | UNDERSCORE
      { mkpat(Ppat_any) }
  | pattern AS val_ident
      { mkpat(Ppat_alias($1, $3)) }
  | signed_constant
      { mkpat(Ppat_constant $1) }
  | CHAR DOTDOT CHAR
      { mkrangepat $1 $3 }
  | pattern_comma_list
      { mkpat(Ppat_tuple(List.rev $1)) }
  | constr_longident
      { mkpat(Ppat_construct($1, None)) }
  | constr_longident pattern %prec prec_constr_appl
      { mkpat(Ppat_construct($1, Some $2)) }
  | pattern COLONCOLON pattern
      { mkpat(Ppat_construct(Lident "::", Some(mkpat(Ppat_tuple[$1;$3])))) }
  | LBRACE lbl_pattern_list RBRACE
      { mkpat(Ppat_record(List.rev $2)) }
  | LBRACKET pattern_semi_list RBRACKET
      { mklistpat(List.rev $2) }
  | pattern BAR pattern
      { mkpat(Ppat_or($1, $3)) }
  | LPAREN pattern RPAREN
      { $2 }
  | LPAREN pattern COLON core_type RPAREN
      { mkpat(Ppat_constraint($2, $4)) }
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
let_pattern:
    constr_longident
      { mkpat(Ppat_construct($1, None)) }
  | constr_longident pattern %prec prec_constr_appl
      { mkpat(Ppat_construct($1, Some $2)) }
  | LBRACE lbl_pattern_list RBRACE
      { mkpat(Ppat_record(List.rev $2)) }
  | LBRACKET pattern_semi_list RBRACKET
      { mklistpat(List.rev $2) }
  | LPAREN pattern RPAREN
      { $2 }
  | LPAREN pattern COLON core_type RPAREN
      { mkpat(Ppat_constraint($2, $4)) }
;

/* Primitive declarations */

primitive_declaration:
    STRING                                      { [$1] }
  | STRING primitive_declaration                { $1 :: $2 }
;

/* Type declarations */

type_declarations:
    type_declaration                            { [$1] }
  | type_declarations AND type_declaration      { $3 :: $1 }
;
type_declaration:
    type_parameters LIDENT type_kind
      { ($2, {ptype_params = $1; ptype_kind = $3; ptype_loc = symbol_loc()}) }
;
type_kind:
    /*empty*/
      { Ptype_abstract }
  | EQUAL core_type
      { Ptype_manifest $2 }
  | EQUAL constructor_declarations
      { Ptype_variant(List.rev $2) }
  | EQUAL LBRACE label_declarations RBRACE
      { Ptype_record(List.rev $3) }
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

/* Core types */

core_type:
    simple_core_type
      { $1 }
  | core_type MINUSGREATER core_type %prec prec_type_arrow
      { mktyp(Ptyp_arrow($1, $3)) }
  | core_type_tuple
      { mktyp(Ptyp_tuple(List.rev $1)) }
;
simple_core_type:
    QUOTE ident
      { mktyp(Ptyp_var $2) }
  | type_longident
      { mktyp(Ptyp_constr($1, [])) }
  | simple_core_type type_longident %prec prec_constr_appl
      { mktyp(Ptyp_constr($2, [$1])) }
  | LPAREN core_type_comma_list RPAREN type_longident %prec prec_constr_appl
      { mktyp(Ptyp_constr($4, List.rev $2)) }
  | LPAREN core_type RPAREN
      { $2 }
;

core_type_tuple:
    simple_core_type STAR simple_core_type
      { [$3; $1] }
  | core_type_tuple STAR simple_core_type
      { $3 :: $1 }
;
core_type_comma_list:
    core_type COMMA core_type                   { [$3; $1] }
  | core_type_comma_list COMMA core_type        { $3 :: $1 }
;
core_type_list:
    simple_core_type                            { [$1] }
  | core_type_list STAR simple_core_type        { $3 :: $1 }
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
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | SUBTRACTIVE                                 { $1 }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | OR                                          { "or" }
  | AMPERSAND                                   { "&" }
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
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;
mod_longident:
    UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }
;
mty_longident:
    ident                                       { Lident $1 }
  | mod_longident DOT ident                     { Ldot($1, $3) }
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
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;

%%
