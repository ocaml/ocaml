(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The lexer generator. Command-line parsing. *)

#open "syntax";;
#open "testscanner";;
#open "grammar";;
#open "lexgen";;
#open "output";;

let main () =
  ic := stdin;
  oc := stdout;
  let lexbuf = lexing.from_channel ic in
  let (Lexdef(header,_) as def) =
    try
      grammar.lexer_definition testscanner.main lexbuf
    with
      parsing.Parse_error x ->
        prerr_string "Syntax error around char ";
        prerr_int (lexing.lexeme_start lexbuf);
        prerr_endline ".";
        sys.exit 2
    | scan_aux.Lexical_error s ->
        prerr_string "Lexical error around char ";
        prerr_int (lexing.lexeme_start lexbuf);
        prerr_string ": ";
        prerr_string s;
        prerr_endline ".";
        sys.exit 2 in
  let ((init, states, acts) as dfa) = make_dfa def in
  output_lexdef header dfa
;;

main(); sys.exit 0
;;
