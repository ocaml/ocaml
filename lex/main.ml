(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
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
    prerr_endline "Usage: ocamllex <input file>";
    exit 2
  end;
  let source_name = Sys.argv.(1) in
  let dest_name =
    if Filename.check_suffix source_name ".mll" then
      Filename.chop_suffix source_name ".mll" ^ ".ml"
    else
      source_name ^ ".ml" in
  let ic = open_in_bin source_name in
  let oc = open_out dest_name in
  let lexbuf = Lexing.from_channel ic in
  let def =
    try
      Parser.lexer_definition Lexer.main lexbuf
    with exn ->
      close_out oc;
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
  let (entries, transitions) = Lexgen.make_dfa def in
  let tables = Compact.compact_tables transitions in
  Output.output_lexdef ic oc def.header tables entries def.trailer;
  close_in ic;
  close_out oc

let _ = Printexc.catch main (); exit 0

