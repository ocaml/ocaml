(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

exception Text_syntax of int * int * string (* line, char, string *)

module Texter =
  struct
    (* builds a text structure from a string. *)
    let text_of_string s =
      let lexbuf = Lexing.from_string s in
      try
        Odoc_text_lexer.init ();
        Odoc_text_parser.main Odoc_text_lexer.main lexbuf
      with
        _ ->
          raise (Text_syntax (!Odoc_text_lexer.line_number, 
                              !Odoc_text_lexer.char_number, 
                              s)
                )
  end
 
