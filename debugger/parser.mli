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

val argument_list_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string list
val argument_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
val integer_list_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int list
val integer_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
val integer :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
val opt_integer_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int option
val opt_signed_integer_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int option
val identifier :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
val identifier_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
val identifier_or_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string option
val opt_identifier_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string option
val variable_list_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string list
val break_argument_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parser_aux.break_arg
val match_arguments_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string * Parser_aux.pattern
val list_arguments_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string option * int option * int option
val end_of_line :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
