/* A simple parser for C-- */

%{
open Cmm
open Parsecmmaux

let rec make_letdef def body =
  match def with
    [] -> body
  | (id, def) :: rem ->
      unbind_ident id;
      Clet(id, def, make_letdef rem body)

let make_switch n selector caselist =
  let index = Array.new n 0 in
  let casev = Array.of_list caselist in
  let actv = Array.new (Array.length casev) Cexit in
  for i = 0 to Array.length casev - 1 do
    let (posl, e) = casev.(i) in
    List.iter (fun pos -> index.(pos) <- i) posl;
    actv.(i) <- e
  done;
  Cswitch(selector, index, actv)

let access_array base numelt size =
  match numelt with
    Cconst_int 0 -> base
  | Cconst_int n -> Cop(Cadda, [base; Cconst_int(n * size)])
  | _ -> Cop(Cadda, [base;
                     Cop(Clsl, [numelt; Cconst_int(Misc.log2 size)])])

%}

%token ADDA
%token ADDF
%token ADDI
%token ADDR
%token ALIGN
%token ALLOC
%token AND
%token APPLY
%token ASR
%token ASSIGN
%token BYTE
%token CASE
%token CATCH
%token CHECKBOUND
%token COLON
%token DIVF
%token DIVI
%token EOF
%token EQA
%token EQF
%token EQI
%token EXIT
%token EXTCALL
%token FLOAT
%token <string> FLOATCONST
%token FLOATOFINT
%token FUNCTION
%token GEA
%token GEF
%token GEI
%token GTA
%token GTF
%token GTI
%token HALF
%token <string> IDENT
%token IF
%token INT
%token <int> INTCONST
%token INTOFFLOAT
%token KSTRING
%token LBRACKET
%token LEA
%token LEF
%token LEI
%token LET
%token LOAD
%token LPAREN
%token LSL
%token LSR
%token LTA
%token LTF
%token LTI
%token MODI
%token MODIFY
%token MULF
%token MULI
%token NEA
%token NEF
%token NEI
%token OR
%token <int> POINTER
%token PROJ
%token RAISE
%token RBRACKET
%token RPAREN
%token SEQ
%token SIGNED
%token SKIP
%token STAR
%token STORE
%token <string> STRING
%token SUBA
%token SUBF
%token SUBI
%token SWITCH
%token TRY
%token UNIT
%token UNSIGNED
%token WHILE
%token WITH
%token XOR
%token ADDRAREF
%token INTAREF
%token FLOATAREF
%token ADDRASET
%token INTASET
%token FLOATASET

%start phrase
%type <Cmm.phrase> phrase

%%

phrase:
    fundecl     { Cfunction $1 }
  | datadecl    { Cdata $1 }
  | EOF         { raise End_of_file }
;
fundecl:
    LPAREN FUNCTION STRING LPAREN params RPAREN sequence RPAREN
      { List.iter (fun (id, ty) -> unbind_ident id) $5;
        {fun_name = $3; fun_args = $5; fun_body = $7; fun_fast = true} }
;
params:
    oneparam params     { $1 :: $2 }
  | /**/                { [] }
;
oneparam:
    IDENT COLON machtype { (bind_ident $1, $3) }
;
machtype:
    UNIT                        { [||] }
  | componentlist               { Array.of_list(List.rev $1) }
;
component:
    ADDR                        { Addr }
  | INT                         { Int }
  | FLOAT                       { Float }
;
componentlist:
    component                    { [$1] }
  | componentlist STAR component { $3 :: $1 }
;
expr:
    INTCONST    { Cconst_int $1 }
  | FLOATCONST  { Cconst_float $1 }
  | STRING      { Cconst_symbol $1 }
  | POINTER     { Cconst_pointer $1 }
  | IDENT       { Cvar(find_ident $1) }
  | LPAREN LET letdef sequence RPAREN { make_letdef $3 $4 }
  | LPAREN ASSIGN IDENT expr RPAREN { Cassign(find_ident $3, $4) }
  | LBRACKET exprlist RBRACKET { Ctuple(List.rev $2) }
  | LPAREN APPLY expr expr machtype RPAREN { Cop(Capply $5, [$3; $4]) }
  | LPAREN EXTCALL STRING expr machtype RPAREN { Cop(Cextcall($3, $5, true), [$4]) }
  | LPAREN LOAD expr machtype RPAREN { Cop(Cload $4, [$3]) }
  | LPAREN unaryop expr RPAREN { Cop($2, [$3]) }
  | LPAREN binaryop expr expr RPAREN { Cop($2, [$3; $4]) }
  | LPAREN SEQ sequence RPAREN { $3 }
  | LPAREN IF expr expr expr RPAREN { Cifthenelse($3, $4, $5) }
  | LPAREN SWITCH INTCONST expr caselist RPAREN { make_switch $3 $4 $5 }
  | LPAREN WHILE expr sequence RPAREN
      { Ccatch(Cloop(Cifthenelse($3, $4, Cexit)), Ctuple []) }
  | LPAREN CATCH sequence WITH sequence RPAREN { Ccatch($3, $5) }
  | EXIT        { Cexit }
  | LPAREN TRY sequence WITH bind_ident sequence RPAREN
                { unbind_ident $5; Ctrywith($3, $5, $6) }
  | LPAREN ADDRAREF expr expr RPAREN
      { Cop(Cload typ_addr, [access_array $3 $4 Arch.size_addr]) }
  | LPAREN INTAREF expr expr RPAREN
      { Cop(Cload typ_int, [access_array $3 $4 Arch.size_int]) }
  | LPAREN FLOATAREF expr expr RPAREN
      { Cop(Cload typ_float, [access_array $3 $4 Arch.size_float]) }
  | LPAREN ADDRASET expr expr expr RPAREN
      { Cop(Cstore, [access_array $3 $4 Arch.size_addr; $5]) }
  | LPAREN INTASET expr expr expr RPAREN
      { Cop(Cstore, [access_array $3 $4 Arch.size_int; $5]) }
  | LPAREN FLOATASET expr expr expr RPAREN
      { Cop(Cstore, [access_array $3 $4 Arch.size_float; $5]) }
;
exprlist:
    exprlist expr               { $2 :: $1 }
  | /**/                        { [] }
;
letdef:
    oneletdef                   { [$1] }
  | LPAREN letdefmult RPAREN    { $2 }
;
letdefmult:
    /**/                        { [] }
  | oneletdef letdefmult        { $1 :: $2 }
;
oneletdef:
    IDENT expr                  { (bind_ident $1, $2) }
;
chunk:
    UNSIGNED BYTE               { Byte_unsigned }
  | SIGNED BYTE                 { Byte_signed }
  | UNSIGNED HALF               { Sixteen_unsigned }
  | SIGNED HALF                 { Sixteen_signed }
;
unaryop:
    PROJ INTCONST               { Cproj($2, 1) }
  | PROJ INTCONST SUBI INTCONST { Cproj($2, $4 - $2 - 1) }
  | LOAD chunk                  { Cloadchunk $2 }
  | ALLOC                       { Calloc }
  | MODIFY                      { Cmodify }
  | FLOATOFINT                  { Cfloatofint }
  | INTOFFLOAT                  { Cintoffloat }
  | RAISE                       { Craise }
;
binaryop:
    STORE                       { Cstore }
  | STORE chunk                 { Cstorechunk $2 }
  | ADDI                        { Caddi }
  | SUBI                        { Csubi }
  | MULI                        { Cmuli }
  | DIVI                        { Cdivi }
  | MODI                        { Cmodi }
  | AND                         { Cand }
  | OR                          { Cor }
  | XOR                         { Cxor }
  | LSL                         { Clsl }
  | LSR                         { Clsr }
  | ASR                         { Casr }
  | EQI                         { Ccmpi Ceq }
  | NEI                         { Ccmpi Cne }
  | LTI                         { Ccmpi Clt }
  | LEI                         { Ccmpi Cle }
  | GTI                         { Ccmpi Cgt }
  | GEI                         { Ccmpi Cge }
  | ADDA                        { Cadda }
  | SUBA                        { Csuba }
  | EQA                         { Ccmpa Ceq }
  | NEA                         { Ccmpa Cne }
  | LTA                         { Ccmpa Clt }
  | LEA                         { Ccmpa Cle }
  | GTA                         { Ccmpa Cgt }
  | GEA                         { Ccmpa Cge }
  | ADDF                        { Caddf }
  | SUBF                        { Csubf }
  | MULF                        { Cmulf }
  | DIVF                        { Cdivf }
  | EQF                         { Ccmpf Ceq }
  | NEF                         { Ccmpf Cne }
  | LTF                         { Ccmpf Clt }
  | LEF                         { Ccmpf Cle }
  | GTF                         { Ccmpf Cgt }
  | GEF                         { Ccmpf Cge }
  | CHECKBOUND                  { Ccheckbound }
;
sequence:
    expr sequence               { Csequence($1, $2) }
  | expr                        { $1 }
;
caselist:
    onecase sequence caselist   { ($1, $2) :: $3 }
  | /**/                        { [] }
;
onecase:
    CASE INTCONST COLON onecase { $2 :: $4 }
  | CASE INTCONST COLON         { [$2] }
;
bind_ident:
    IDENT                       { bind_ident $1 }
;
datadecl:
    LPAREN datalist RPAREN      { List.rev $2 }
;
datalist:
    datalist dataitem           { $2 :: $1 }
  | /**/                        { [] }
;
dataitem:
    STRING COLON                { Cdefine_symbol $1 }
  | INTCONST COLON              { Cdefine_label $1 }
  | BYTE INTCONST               { Cint8 $2 }
  | HALF INTCONST               { Cint16 $2 }
  | INT INTCONST                { Cint $2 }
  | FLOAT FLOATCONST            { Cfloat $2 }
  | ADDR STRING                 { Csymbol_address $2 }
  | ADDR INTCONST               { Clabel_address $2 }
  | KSTRING STRING              { Cstring $2 }
  | SKIP INTCONST               { Cskip $2 }
  | ALIGN INTCONST              { Calign $2 }
;

