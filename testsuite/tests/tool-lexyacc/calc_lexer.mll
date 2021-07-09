{
open Calc_parser        (* The type token is defined in calc_parser.mli *)
exception Eof
}

rule token = parse
    [' ' '\t' '\r']   { token lexbuf }     (* skip blanks *)
  | ['\n' ]           { EOL }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIV }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | eof               { raise Eof }
