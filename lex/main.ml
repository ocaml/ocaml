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

open Syntax
open Lexgen
open Output

let main () =
  if Array.length Sys.argv != 2 then begin
    prerr_endline "Usage: camllex <input file>";
    exit 2
  end;
  let source_name = Sys.argv.(1) in
  let dest_name =
    if Filename.check_suffix source_name ".mll" then
      Filename.chop_suffix source_name ".mll" ^ ".ml"
    else
      source_name ^ ".ml" in
  ic := open_in_bin source_name;
  oc := open_out dest_name;
  let lexbuf =
    Lexing.from_channel !ic in
  let (Lexdef(header,_) as def) =
    try
      Parser.lexer_definition Lexer.main lexbuf
    with exn ->
      close_out !oc;
      Sys.remove dest_name;
      begin match exn with
        Parsing.Parse_error ->
          prerr_string "Syntax error around char ";
          prerr_int (Lexing.lexeme_start lexbuf);
          prerr_endline "."
      | Lexer.Lexical_error s ->
          prerr_string "Lexical error around char ";
          prerr_int (Lexing.lexeme_start lexbuf);
          prerr_string ": ";
          prerr_string s;
          prerr_endline "."
      | _ -> raise exn
      end;
      exit 2 in
  let ((init, states, acts) as dfa) = make_dfa def in
  output_lexdef header dfa;
  close_in !ic;
  close_out !oc

let _ = Printexc.catch main (); exit 0

