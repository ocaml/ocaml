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

(* Output the DFA tables and its entry points *)

open Printf
open Syntax
open Lexgen
open Compact

(* To copy the ML code fragments *)

let copy_buffer = String.create 1024

let copy_chunk ic oc (Location(start,stop)) =
  seek_in ic start;
  let n = ref (stop - start) in
  while !n > 0 do
    let m = input ic copy_buffer 0 (min !n 1024) in
    output oc copy_buffer 0 m;
    n := !n - m
  done

(* To output an array of short ints, encoded as a string *)

let output_byte oc b =
  output_char oc '\\';
  output_char oc (Char.chr(48 + b / 100));
  output_char oc (Char.chr(48 + (b / 10) mod 10));
  output_char oc (Char.chr(48 + b mod 10))

let output_array oc v =
  output_string oc "   \"";
  for i = 0 to Array.length v - 1 do
    output_byte oc (v.(i) land 0xFF);
    output_byte oc ((v.(i) asr 8) land 0xFF);
    if i land 7 = 7 then output_string oc "\\\n    "
  done;
  output_string oc "\""

(* Output the tables *)

let output_tables oc tbl =
  output_string oc "let lex_tables = {\n";
  fprintf oc "  Lexing.lex_base = \n%a;\n" output_array tbl.tbl_base;
  fprintf oc "  Lexing.lex_backtrk = \n%a;\n" output_array tbl.tbl_backtrk;
  fprintf oc "  Lexing.lex_default = \n%a;\n" output_array tbl.tbl_default;
  fprintf oc "  Lexing.lex_trans = \n%a;\n" output_array tbl.tbl_trans;
  fprintf oc "  Lexing.lex_check = \n%a\n" output_array tbl.tbl_check;
  output_string oc "}\n\n"

(* Output the entries *)

let output_entry ic oc e =
  fprintf oc "%s lexbuf =\n" e.auto_name;
  fprintf oc "  match Lexing.engine lex_tables %d lexbuf with\n    "
             e.auto_initial_state;
  let first = ref true in
  List.iter
    (fun (num, loc) ->
      if !first then first := false else fprintf oc "  | ";
      fprintf oc "%d -> (" num;
      copy_chunk ic oc loc;
      fprintf oc ")\n")
    e.auto_actions;
  fprintf oc "  | _ -> failwith \"%s: empty token\"\n\n" e.auto_name

(* Main output function *)

let output_lexdef ic oc header tables entry_points trailer =
  Printf.printf "%d states, %d transitions, table size %d bytes\n"
    (Array.length tables.tbl_base)
    (Array.length tables.tbl_trans)
    (2 * (Array.length tables.tbl_base + Array.length tables.tbl_backtrk +
          Array.length tables.tbl_default + Array.length tables.tbl_trans +
          Array.length tables.tbl_check));
  flush stdout;
  copy_chunk ic oc header;
  output_tables oc tables;
  begin match entry_points with
    [] -> ()
  | entry1 :: entries ->
      output_string oc "let rec "; output_entry ic oc entry1;
      List.iter
        (fun e -> output_string oc "and "; output_entry ic oc e)
        entries
  end;
  copy_chunk ic oc trailer
