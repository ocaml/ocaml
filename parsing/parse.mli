(* Entry points in the parser *)

val toplevel_phrase : Lexing.lexbuf -> Parsetree.toplevel_phrase
val implementation : Lexing.lexbuf -> Parsetree.structure
val interface : Lexing.lexbuf -> Parsetree.signature

exception Error of int * int        (* Syntax error *)
