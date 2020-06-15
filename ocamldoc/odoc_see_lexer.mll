{
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** the lexer for special comments. *)

open Odoc_parser

let buf = Buffer.create 32

}

rule main = parse
  [' ' '\013' '\009' '\012'] +
  {
    main lexbuf
  }

  | [ '\010' ]
      {
        main lexbuf
      }

  | "<"
      {
        url lexbuf
      }

  | "\""
      {
        doc lexbuf
      }


  | '\''
      {
        file lexbuf
      }

  | eof
      {
        EOF
      }

  | _
      {
        Buffer.reset buf ;
        Buffer.add_string buf (Lexing.lexeme lexbuf);
        desc lexbuf
      }

and url = parse
  | ([^'>'] | '\n')+">"
      {
        let s = Lexing.lexeme lexbuf in
        See_url (String.sub s 0 ((String.length s) -1))
      }


and doc = parse
  | ([^'"'] | '\n' | "\\'")* "\""
      {
        let s = Lexing.lexeme lexbuf in
        See_doc (String.sub s 0 ((String.length s) -1))
      }

and file = parse
  | ([^'\''] | '\n' | "\\\"")* "'"
      {
        let s = Lexing.lexeme lexbuf in
        See_file (String.sub s 0 ((String.length s) -1))
      }


and desc = parse
    eof
      { Desc (Buffer.contents buf) }
  | _
      {
        Buffer.add_string buf (Lexing.lexeme lexbuf);
        desc lexbuf
      }
