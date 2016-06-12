(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris           *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Lexer definitions for the Tests Specification Language *)

{
open Lexing
open Tsl_parser

}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

rule token = parse
  | blank * { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | "(*" blank* "TEST" { TSL_BEGIN }
  | "*)" { TSL_END }
  | '*'+ { TEST_DEPTH (String.length (Lexing.lexeme lexbuf)) }
  | "=" { EQUAL }  
  | identchar *
    { let s = Lexing.lexeme lexbuf in
      if s="include" then INCLUDE else IDENTIFIER s
    }
  | "\"" [^'"']* "\""
    { let s = Lexing.lexeme lexbuf in
      let string_length = (String.length s) -2 in
      let s' = String.sub s 1 string_length in
      STRING s'
    }
  | _
    { failwith "Unexpected character in tsl_lexer" }
