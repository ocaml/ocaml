(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Lexer definitions for the Tests Specification Language and for
   response files *)

{
open Tsl_parser

let comment_start_pos = ref []
let has_comments = ref false

let lexer_error message =
  failwith (Printf.sprintf "Tsl lexer: %s" message)
}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let identchar = ['A'-'Z' 'a'-'z' '_' '.' '-' '\'' '0'-'9']
let num = ['0'-'9']

rule is_test = parse
  | blank * { is_test lexbuf }
  | newline { Lexing.new_line lexbuf; is_test lexbuf }
  | "/*" blank* "TEST" { true }
  | "/*" blank* "TEST_BELOW" { true }
  | "(*" blank* "TEST" { true }
  | "(*" blank* "TEST_BELOW" { true }
  | _ { false }
  | eof { false }

and token = parse
  | blank * { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | "/*" blank* "TEST" { TSL_BEGIN_C_STYLE `Above }
  | "/*" blank* "TEST_BELOW" _ * "/*" blank* "TEST" {
      let s = Lexing.lexeme lexbuf in
      String.iter (fun c -> if c = '\n' then Lexing.new_line lexbuf) s;
      TSL_BEGIN_C_STYLE `Below
    }
  | "*/" { TSL_END_C_STYLE }
  | "(*" blank* "TEST" { TSL_BEGIN_OCAML_STYLE `Above }
  | "(*" blank* "TEST_BELOW" _ * "(*" blank* "TEST" {
      let s = Lexing.lexeme lexbuf in
      String.iter (fun c -> if c = '\n' then Lexing.new_line lexbuf) s;
      TSL_BEGIN_OCAML_STYLE `Below
    }
  | "*)" { TSL_END_OCAML_STYLE }
  | "," { COMMA }
  | '*'+ { TEST_DEPTH (String.length (Lexing.lexeme lexbuf)) }
  | "*" (num+ as n) { TEST_DEPTH (int_of_string n)}
  | "+=" { PLUSEQUAL }
  | "=" { EQUAL }
  | identchar *
    { let s = Lexing.lexeme lexbuf in
      match s with
        | "include" -> INCLUDE
        | "set" -> SET
        | "unset" -> UNSET
        | "with" -> WITH
        | _ -> IDENTIFIER s
    }
  | "{" { LEFT_BRACE }
  | "}" { RIGHT_BRACE }
  | ";" { SEMI }
  | "(*"
    {
      comment_start_pos := [Lexing.lexeme_start_p lexbuf];
      has_comments := true;
      comment lexbuf
    }
  | '"'
    { STRING (string "" lexbuf) }
  | _
    {
      let pos = Lexing.lexeme_start_p lexbuf in
      let file = pos.Lexing.pos_fname in
      let line = pos.Lexing.pos_lnum in
      let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      let message = Printf.sprintf "%s:%d:%d: unexpected character %s"
        file line column (Lexing.lexeme lexbuf) in
      lexer_error message
    }
  | eof
    { lexer_error "unexpected eof" }
(* Backslashes are ignored in strings except at the end of lines where they
   cause the newline to be ignored. After an escaped newline, any blank
   characters at the start of the line are ignored and optionally one blank
   character may be escaped with a backslash.

   In particular, this means that the following:
script = "some-directory\\
         \ foo"
   is interpreted as the OCaml string "some-directory\\ foo".
   *)
and string acc = parse
  | [^ '\\' '"' ]+
    { string (acc ^ Lexing.lexeme lexbuf) lexbuf }
  | '\\' newline blank* ('\\' (blank as blank))?
    { let space =
        match blank with None -> "" | Some blank -> String.make 1 blank
      in
      Lexing.new_line lexbuf;
      string (acc ^ space) lexbuf }
  | '\\'
    {string (acc ^ "\\") lexbuf}
  | '"'
    {acc}
and comment = parse
  | "(*"
    {
      comment_start_pos :=
        (Lexing.lexeme_start_p lexbuf) :: !comment_start_pos;
      comment lexbuf
    }
  | "*)"
    {
      comment_start_pos := List.tl !comment_start_pos;
      if !comment_start_pos = [] then token lexbuf else comment lexbuf
    }
  | eof
    {
      let pos = List.hd !comment_start_pos in
      let file = pos.Lexing.pos_fname in
      let line = pos.Lexing.pos_lnum in
      let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      let message = Printf.sprintf "%s:%d:%d: unterminated comment"
        file line column in
      lexer_error message
    }
  | newline
    {
      Lexing.new_line lexbuf;
      comment lexbuf
    }
  | _
    {
      comment lexbuf
    }

(* Parse one line of a response file (for scripts and hooks) *)
and modifier = parse
  | '-' (identchar* as variable)
    { variable, `Remove }
  | (identchar* as variable) "=\"" (_* as str) '"'
    { variable, `Add str }
  | (identchar* as variable) "+=\"" (_* as str) '"'
    { variable, `Append str }
  | _
    { failwith "syntax error in script response file" }
