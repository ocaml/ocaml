type token =
    ARGUMENT of (string)
  | IDENTIFIER of (string)
  | INTEGER of (int)
  | STAR
  | MINUS
  | UNDERUNDER
  | SHARP
  | AT
  | COLONCOLON
  | COMMA
  | UNDERSCORE
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | SEMI
  | EQUAL
  | SUPERIOR
  | PREFIX
  | OPERATOR of (string)
  | EOL

open Parsing

open Primitives
open Input_handling
open Parser_aux

(* Line 8, file parser.ml *)
let yytransl_const = [|
  260 (* STAR *);
  261 (* MINUS *);
  262 (* UNDERUNDER *);
  263 (* SHARP *);
  264 (* AT *);
  265 (* COLONCOLON *);
  266 (* COMMA *);
  267 (* UNDERSCORE *);
  268 (* LPAREN *);
  269 (* RPAREN *);
  270 (* LBRACKET *);
  271 (* RBRACKET *);
  272 (* LBRACE *);
  273 (* RBRACE *);
  274 (* SEMI *);
  275 (* EQUAL *);
  276 (* SUPERIOR *);
  277 (* PREFIX *);
  279 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* ARGUMENT *);
  258 (* IDENTIFIER *);
  259 (* INTEGER *);
  278 (* OPERATOR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\003\000\003\000\004\000\005\000\006\000\
\006\000\007\000\007\000\008\000\009\000\010\000\010\000\017\000\
\017\000\011\000\011\000\012\000\012\000\019\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\018\000\018\000\018\000\
\013\000\013\000\013\000\013\000\013\000\015\000\015\000\014\000\
\022\000\022\000\023\000\023\000\024\000\025\000\025\000\021\000\
\021\000\021\000\021\000\021\000\026\000\026\000\026\000\026\000\
\026\000\026\000\026\000\016\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\002\000\001\000\002\000\001\000\002\000\
\001\000\002\000\001\000\001\000\002\000\001\000\001\000\001\000\
\000\000\002\000\001\000\002\000\001\000\002\000\001\000\002\000\
\002\000\002\000\002\000\002\000\002\000\001\000\003\000\001\000\
\001\000\001\000\001\000\004\000\004\000\003\000\001\000\003\000\
\003\000\001\000\003\000\001\000\003\000\003\000\003\000\001\000\
\003\000\001\000\002\000\002\000\001\000\001\000\002\000\003\000\
\003\000\003\000\003\000\001\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\060\000\061\000\002\000\000\000\062\000\000\000\
\063\000\005\000\000\000\064\000\007\000\065\000\000\000\066\000\
\009\000\000\000\011\000\067\000\012\000\068\000\000\000\069\000\
\014\000\070\000\015\000\000\000\071\000\019\000\000\000\032\000\
\000\000\072\000\021\000\000\000\030\000\000\000\034\000\073\000\
\033\000\000\000\035\000\074\000\000\000\000\000\039\000\075\000\
\000\000\076\000\001\000\003\000\004\000\006\000\008\000\010\000\
\013\000\018\000\000\000\024\000\025\000\026\000\027\000\028\000\
\029\000\020\000\016\000\000\000\022\000\000\000\000\000\053\000\
\000\000\000\000\000\000\000\000\054\000\000\000\000\000\000\000\
\048\000\000\000\023\000\031\000\000\000\000\000\000\000\000\000\
\055\000\000\000\000\000\000\000\000\000\000\000\052\000\051\000\
\000\000\000\000\040\000\000\000\038\000\036\000\037\000\000\000\
\058\000\000\000\056\000\000\000\057\000\000\000\000\000\000\000\
\000\000\041\000\000\000\043\000"

let yydgoto = "\017\000\
\020\000\023\000\025\000\028\000\030\000\032\000\036\000\093\000\
\040\000\042\000\045\000\050\000\056\000\060\000\064\000\033\000\
\065\000\094\000\059\000\053\000\106\000\107\000\109\000\110\000\
\096\000\097\000"

let yysindex = "\159\000\
\007\255\026\255\022\255\006\255\041\255\047\255\016\255\021\255\
\051\255\008\255\009\255\045\255\034\255\070\255\020\255\033\255\
\000\000\007\255\000\000\000\000\000\000\033\255\000\000\022\255\
\000\000\000\000\033\255\000\000\000\000\000\000\033\255\000\000\
\000\000\006\255\000\000\000\000\000\000\000\000\033\255\000\000\
\000\000\000\000\000\000\033\255\000\000\000\000\061\255\000\000\
\137\255\000\000\000\000\045\255\000\000\074\255\000\000\000\000\
\000\000\033\255\000\000\000\000\132\255\033\255\000\000\000\000\
\041\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\119\255\000\000\061\255\087\255\000\000\
\132\255\117\255\070\255\174\255\000\000\174\255\042\255\090\255\
\000\000\047\255\000\000\000\000\047\255\006\255\132\255\093\255\
\000\000\053\255\092\255\086\255\095\255\107\255\000\000\000\000\
\132\255\132\255\000\000\132\255\000\000\000\000\000\000\101\255\
\000\000\132\255\000\000\132\255\000\000\070\255\101\255\083\255\
\083\255\000\000\083\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\112\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\097\255\000\000\
\000\000\000\000\000\000\000\000\000\000\120\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\127\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\071\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\178\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\125\255\000\000\000\000\000\000\118\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\187\255\
\000\000\000\000\000\000\000\000\000\000\000\000\196\255\203\255\
\210\255\000\000\133\255\000\000"

let yygindex = "\000\000\
\129\000\000\000\125\000\250\255\089\000\253\255\000\000\147\000\
\000\000\000\000\143\000\126\000\000\000\000\000\000\000\002\000\
\123\000\244\255\000\000\104\000\201\255\058\000\056\000\000\000\
\000\000\239\255"

let yytablesize = 233
let yytable = "\052\000\
\058\000\061\000\021\000\035\000\026\000\095\000\055\000\018\000\
\027\000\041\000\044\000\043\000\046\000\051\000\057\000\099\000\
\046\000\066\000\031\000\021\000\034\000\062\000\037\000\068\000\
\024\000\026\000\022\000\072\000\070\000\019\000\019\000\019\000\
\071\000\104\000\049\000\047\000\027\000\048\000\019\000\052\000\
\073\000\054\000\019\000\029\000\019\000\074\000\047\000\120\000\
\048\000\031\000\113\000\114\000\039\000\051\000\049\000\019\000\
\019\000\127\000\128\000\085\000\129\000\113\000\114\000\074\000\
\019\000\049\000\075\000\019\000\131\000\019\000\122\000\047\000\
\023\000\048\000\111\000\083\000\112\000\023\000\108\000\012\000\
\012\000\023\000\023\000\012\000\023\000\012\000\023\000\012\000\
\012\000\103\000\049\000\113\000\114\000\012\000\117\000\119\000\
\115\000\118\000\023\000\116\000\023\000\113\000\114\000\023\000\
\124\000\121\000\123\000\023\000\023\000\113\000\023\000\125\000\
\023\000\108\000\017\000\023\000\023\000\023\000\086\000\023\000\
\048\000\101\000\017\000\087\000\126\000\102\000\017\000\088\000\
\089\000\016\000\090\000\105\000\091\000\086\000\044\000\048\000\
\092\000\049\000\087\000\042\000\076\000\077\000\088\000\089\000\
\078\000\090\000\067\000\091\000\069\000\045\000\045\000\092\000\
\049\000\098\000\038\000\079\000\080\000\063\000\081\000\001\000\
\002\000\003\000\004\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\014\000\015\000\016\000\037\000\
\084\000\082\000\100\000\130\000\087\000\132\000\000\000\000\000\
\088\000\089\000\050\000\090\000\000\000\091\000\050\000\000\000\
\050\000\000\000\050\000\050\000\059\000\000\000\000\000\059\000\
\050\000\059\000\000\000\059\000\059\000\049\000\000\000\000\000\
\049\000\059\000\049\000\000\000\049\000\049\000\000\000\047\000\
\000\000\047\000\049\000\047\000\047\000\000\000\046\000\000\000\
\046\000\047\000\046\000\046\000\000\000\000\000\000\000\000\000\
\046\000"

let yycheck = "\012\000\
\013\000\014\000\001\000\007\000\003\000\061\000\013\000\001\001\
\003\001\002\001\002\001\010\000\011\000\012\000\013\000\002\001\
\015\000\016\000\003\001\018\000\005\001\002\001\002\001\022\000\
\003\001\024\000\001\001\034\000\027\000\023\001\023\001\023\001\
\031\000\089\000\021\001\002\001\003\001\004\001\023\001\052\000\
\039\000\008\001\023\001\003\001\023\001\044\000\002\001\103\000\
\004\001\003\001\009\001\010\001\002\001\052\000\021\001\023\001\
\023\001\113\000\114\000\058\000\116\000\009\001\010\001\062\000\
\023\001\021\001\006\001\023\001\124\000\023\001\018\001\002\001\
\002\001\004\001\092\000\002\001\094\000\007\001\091\000\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\003\001\021\001\009\001\010\001\023\001\098\000\102\000\
\095\000\101\000\002\001\010\001\004\001\009\001\010\001\007\001\
\019\001\013\001\015\001\011\001\012\001\009\001\014\001\017\001\
\016\001\126\000\003\001\019\001\020\001\021\001\002\001\023\001\
\004\001\003\001\003\001\007\001\018\001\007\001\007\001\011\001\
\012\001\003\001\014\001\015\001\016\001\002\001\017\001\004\001\
\020\001\021\001\007\001\015\001\004\001\005\001\011\001\012\001\
\008\001\014\001\018\000\016\001\024\000\017\001\018\001\020\001\
\021\001\065\000\008\000\019\001\020\001\015\000\022\001\001\000\
\002\000\003\000\004\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\014\000\015\000\016\000\002\001\
\054\000\052\000\075\000\122\000\007\001\126\000\255\255\255\255\
\011\001\012\001\009\001\014\001\255\255\016\001\013\001\255\255\
\015\001\255\255\017\001\018\001\010\001\255\255\255\255\013\001\
\023\001\015\001\255\255\017\001\018\001\010\001\255\255\255\255\
\013\001\023\001\015\001\255\255\017\001\018\001\255\255\013\001\
\255\255\015\001\023\001\017\001\018\001\255\255\013\001\255\255\
\015\001\023\001\017\001\018\001\255\255\255\255\255\255\255\255\
\023\001"

let yyact = [|
  (fun _ -> failwith "parser")
(* Rule 1, file parser.mly, line 105 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 1 : string)::(peek_val parser_env 0 : string list) ) : string list))
(* Rule 2, file parser.mly, line 107 *)
; (fun parser_env -> Obj.repr(( [] ) : string list))
(* Rule 3, file parser.mly, line 111 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 1 : string) ) : string))
(* Rule 4, file parser.mly, line 117 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 1 : int)::(peek_val parser_env 0 : int list) ) : int list))
(* Rule 5, file parser.mly, line 119 *)
; (fun parser_env -> Obj.repr(( [] ) : int list))
(* Rule 6, file parser.mly, line 123 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 1 : int) ) : int))
(* Rule 7, file parser.mly, line 127 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 0 : int) ) : int))
(* Rule 8, file parser.mly, line 131 *)
; (fun parser_env -> Obj.repr(( Some (peek_val parser_env 1 : int) ) : int option))
(* Rule 9, file parser.mly, line 133 *)
; (fun parser_env -> Obj.repr(( None ) : int option))
(* Rule 10, file parser.mly, line 137 *)
; (fun parser_env -> Obj.repr(( Some (- (peek_val parser_env 0 : int)) ) : int option))
(* Rule 11, file parser.mly, line 139 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 0 : int option) ) : int option))
(* Rule 12, file parser.mly, line 145 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 0 : string) ) : string))
(* Rule 13, file parser.mly, line 149 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 1 : string) ) : string))
(* Rule 14, file parser.mly, line 153 *)
; (fun parser_env -> Obj.repr(( Some (peek_val parser_env 0 : string) ) : string option))
(* Rule 15, file parser.mly, line 155 *)
; (fun parser_env -> Obj.repr(( None ) : string option))
(* Rule 16, file parser.mly, line 159 *)
; (fun parser_env -> Obj.repr(( Some (peek_val parser_env 0 : string) ) : 'opt_identifier))
(* Rule 17, file parser.mly, line 161 *)
; (fun parser_env -> Obj.repr(( None ) : 'opt_identifier))
(* Rule 18, file parser.mly, line 165 *)
; (fun parser_env -> Obj.repr(( Some (peek_val parser_env 1 : string) ) : string option))
(* Rule 19, file parser.mly, line 167 *)
; (fun parser_env -> Obj.repr(( None ) : string option))
(* Rule 20, file parser.mly, line 173 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 1 : 'variable)::(peek_val parser_env 0 : string list) ) : string list))
(* Rule 21, file parser.mly, line 175 *)
; (fun parser_env -> Obj.repr(( [] ) : string list))
(* Rule 22, file parser.mly, line 179 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 1 : 'variable) ) : 'variable_eol))
(* Rule 23, file parser.mly, line 183 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 0 : string) ) : 'local_name))
(* Rule 24, file parser.mly, line 185 *)
; (fun parser_env -> Obj.repr(( "*" ) : 'local_name))
(* Rule 25, file parser.mly, line 187 *)
; (fun parser_env -> Obj.repr(( "-" ) : 'local_name))
(* Rule 26, file parser.mly, line 189 *)
; (fun parser_env -> Obj.repr(( "@" ) : 'local_name))
(* Rule 27, file parser.mly, line 191 *)
; (fun parser_env -> Obj.repr(( "=" ) : 'local_name))
(* Rule 28, file parser.mly, line 193 *)
; (fun parser_env -> Obj.repr(( ">" ) : 'local_name))
(* Rule 29, file parser.mly, line 195 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 0 : string) ) : 'local_name))
(* Rule 30, file parser.mly, line 199 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 0 : 'local_name) ) : 'variable))
(* Rule 31, file parser.mly, line 201 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 2 : string) ^ "." ^ (peek_val parser_env 0 : 'local_name) ) : 'variable))
(* Rule 32, file parser.mly, line 203 *)
; (fun parser_env -> Obj.repr(( "" ) : 'variable))
(* Rule 33, file parser.mly, line 210 *)
; (fun parser_env -> Obj.repr(( BA_none ) : Parser_aux.break_arg))
(* Rule 34, file parser.mly, line 212 *)
; (fun parser_env -> Obj.repr(( BA_pc (peek_val parser_env 0 : int) ) : Parser_aux.break_arg))
(* Rule 35, file parser.mly, line 214 *)
; (fun parser_env -> Obj.repr(( BA_function (peek_val parser_env 0 : 'variable_eol) ) : Parser_aux.break_arg))
(* Rule 36, file parser.mly, line 216 *)
; (fun parser_env -> Obj.repr(( BA_pos1 ((peek_val parser_env 2 : 'opt_identifier), (peek_val parser_env 1 : int), (peek_val parser_env 0 : int option)) ) : Parser_aux.break_arg))
(* Rule 37, file parser.mly, line 218 *)
; (fun parser_env -> Obj.repr(( BA_pos2 ((peek_val parser_env 2 : 'opt_identifier), (peek_val parser_env 0 : int)) ) : Parser_aux.break_arg))
(* Rule 38, file parser.mly, line 224 *)
; (fun parser_env -> Obj.repr(( ((peek_val parser_env 2 : 'opt_identifier), Some (peek_val parser_env 1 : int), (peek_val parser_env 0 : int option)) ) : string option * int option * int option))
(* Rule 39, file parser.mly, line 226 *)
; (fun parser_env -> Obj.repr(( ((peek_val parser_env 0 : string option), None, None) ) : string option * int option * int option))
(* Rule 40, file parser.mly, line 232 *)
; (fun parser_env -> Obj.repr(( ((peek_val parser_env 2 : 'variable), (peek_val parser_env 1 : 'pattern)) ) : string * Parser_aux.pattern))
(* Rule 41, file parser.mly, line 236 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 2 : 'pattern)::(peek_val parser_env 0 : 'pattern_sm_list) ) : 'pattern_sm_list))
(* Rule 42, file parser.mly, line 238 *)
; (fun parser_env -> Obj.repr(( [(peek_val parser_env 0 : 'pattern)] ) : 'pattern_sm_list))
(* Rule 43, file parser.mly, line 243 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 2 : 'pattern_label)::(peek_val parser_env 0 : 'pattern_label_list) ) : 'pattern_label_list))
(* Rule 44, file parser.mly, line 245 *)
; (fun parser_env -> Obj.repr(( [(peek_val parser_env 0 : 'pattern_label)] ) : 'pattern_label_list))
(* Rule 45, file parser.mly, line 250 *)
; (fun parser_env -> Obj.repr(( ((peek_val parser_env 2 : 'variable), (peek_val parser_env 0 : 'pattern)) ) : 'pattern_label))
(* Rule 46, file parser.mly, line 255 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 0 : 'pattern) :: (peek_val parser_env 2 : 'pattern_comma_list) ) : 'pattern_comma_list))
(* Rule 47, file parser.mly, line 257 *)
; (fun parser_env -> Obj.repr(( [(peek_val parser_env 0 : 'pattern); (peek_val parser_env 2 : 'pattern)] ) : 'pattern_comma_list))
(* Rule 48, file parser.mly, line 262 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 0 : 'simple_pattern) ) : 'pattern))
(* Rule 49, file parser.mly, line 264 *)
; (fun parser_env -> Obj.repr(( P_concat ((peek_val parser_env 2 : 'pattern), (peek_val parser_env 0 : 'pattern)) ) : 'pattern))
(* Rule 50, file parser.mly, line 266 *)
; (fun parser_env -> Obj.repr(( P_tuple (List.rev (peek_val parser_env 0 : 'pattern_comma_list)) ) : 'pattern))
(* Rule 51, file parser.mly, line 268 *)
; (fun parser_env -> Obj.repr(( P_constr ((peek_val parser_env 1 : 'variable), (peek_val parser_env 0 : 'simple_pattern)) ) : 'pattern))
(* Rule 52, file parser.mly, line 270 *)
; (fun parser_env -> Obj.repr(( P_constr ("", (peek_val parser_env 0 : 'simple_pattern)) ) : 'pattern))
(* Rule 53, file parser.mly, line 275 *)
; (fun parser_env -> Obj.repr(( P_dummy ) : 'simple_pattern))
(* Rule 54, file parser.mly, line 277 *)
; (fun parser_env -> Obj.repr(( P_variable (peek_val parser_env 0 : string) ) : 'simple_pattern))
(* Rule 55, file parser.mly, line 279 *)
; (fun parser_env -> Obj.repr(( P_list [] ) : 'simple_pattern))
(* Rule 56, file parser.mly, line 281 *)
; (fun parser_env -> Obj.repr(( P_list (peek_val parser_env 1 : 'pattern_sm_list) ) : 'simple_pattern))
(* Rule 57, file parser.mly, line 283 *)
; (fun parser_env -> Obj.repr(( P_record (peek_val parser_env 1 : 'pattern_label_list) ) : 'simple_pattern))
(* Rule 58, file parser.mly, line 285 *)
; (fun parser_env -> Obj.repr(( (peek_val parser_env 1 : 'pattern) ) : 'simple_pattern))
(* Rule 59, file parser.mly, line 287 *)
; (fun parser_env -> Obj.repr(( P_nth ((peek_val parser_env 1 : int), (peek_val parser_env 0 : 'pattern)) ) : 'simple_pattern))
(* Rule 60, file parser.mly, line 294 *)
; (fun parser_env -> Obj.repr(( stop_user_input () ) : unit))
(* Entry argument_list_eol *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry argument_eol *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry integer_list_eol *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry integer_eol *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry integer *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry opt_integer_eol *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry opt_signed_integer_eol *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry identifier *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry identifier_eol *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry identifier_or_eol *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry opt_identifier_eol *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry variable_list_eol *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry break_argument_eol *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry match_arguments_eol *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry list_arguments_eol *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry end_of_line *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
|]
let yytables =
  { actions=yyact;
    transl_const=yytransl_const;
    transl_block=yytransl_block;
    lhs=yylhs;
    len=yylen;
    defred=yydefred;
    dgoto=yydgoto;
    sindex=yysindex;
    rindex=yyrindex;
    gindex=yygindex;
    tablesize=yytablesize;
    table=yytable;
    check=yycheck;
    error_function=parse_error }
let argument_list_eol lexfun lexbuf = yyparse yytables 1 lexfun lexbuf
let argument_eol lexfun lexbuf = yyparse yytables 2 lexfun lexbuf
let integer_list_eol lexfun lexbuf = yyparse yytables 3 lexfun lexbuf
let integer_eol lexfun lexbuf = yyparse yytables 4 lexfun lexbuf
let integer lexfun lexbuf = yyparse yytables 5 lexfun lexbuf
let opt_integer_eol lexfun lexbuf = yyparse yytables 6 lexfun lexbuf
let opt_signed_integer_eol lexfun lexbuf = yyparse yytables 7 lexfun lexbuf
let identifier lexfun lexbuf = yyparse yytables 8 lexfun lexbuf
let identifier_eol lexfun lexbuf = yyparse yytables 9 lexfun lexbuf
let identifier_or_eol lexfun lexbuf = yyparse yytables 10 lexfun lexbuf
let opt_identifier_eol lexfun lexbuf = yyparse yytables 11 lexfun lexbuf
let variable_list_eol lexfun lexbuf = yyparse yytables 12 lexfun lexbuf
let break_argument_eol lexfun lexbuf = yyparse yytables 13 lexfun lexbuf
let match_arguments_eol lexfun lexbuf = yyparse yytables 14 lexfun lexbuf
let list_arguments_eol lexfun lexbuf = yyparse yytables 15 lexfun lexbuf
let end_of_line lexfun lexbuf = yyparse yytables 16 lexfun lexbuf
