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

open Lexing

type t =
  { loc_start: int; loc_end: int }

let none = { loc_start = -1; loc_end = -1 }

let symbol_loc () = 
  { loc_start = Parsing.symbol_start(); loc_end = Parsing.symbol_end() }

let rhs_loc n =
  { loc_start = Parsing.rhs_start n; loc_end = Parsing.rhs_end n }

let input_name = ref ""

let input_lexbuf = ref (None : lexbuf option)

(* Determine line numbers and position of beginning of lines in a file *)

let line_pos_file filename loc =
  let ic = open_in_bin filename in
  let pos = ref 0
  and linenum = ref 1
  and linebeg = ref 0 in
  begin try
    while !pos < loc do
      incr pos;
      if input_char ic = '\n' then begin
        incr linenum;
        linebeg := !pos
      end
    done
  with End_of_file -> ()
  end;
  close_in ic;
  (!linenum, !linebeg)

(* Terminal info *)

type terminal_info_status = Unknown | Bad_term | Good_term

let status = ref Unknown
and num_lines = ref 0
and cursor_up = ref ""
and start_standout = ref ""
and end_standout = ref ""

let setup_terminal_info() =
  try
    Terminfo.setupterm();
    num_lines := Terminfo.getnum "li";
    cursor_up := Terminfo.getstr "up";
    begin try
      start_standout := Terminfo.getstr "us";
      end_standout := Terminfo.getstr "ue"
    with Not_found ->
      start_standout := Terminfo.getstr "so";
      end_standout := Terminfo.getstr "se"
    end;
    status := Good_term
  with _ ->
    status := Bad_term

(* Print the location using standout mode. *)

let rec highlight_location loc =
  match !status with
    Unknown ->
      setup_terminal_info(); highlight_location loc
  | Bad_term ->
      false
  | Good_term ->
      match !input_lexbuf with
        None -> false
      | Some lb ->
          (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
          let pos0 = -lb.lex_abs_pos in
          (* Do nothing if the buffer does not contain the whole phrase. *)
          if pos0 < 0 then false else begin
            (* Count number of lines in phrase *)
            let lines = ref 0 in
            for i = pos0 to String.length lb.lex_buffer - 1 do
              if lb.lex_buffer.[i] = '\n' then incr lines
            done;
            (* If too many lines, give up *)
            if !lines >= !num_lines - 2 then false else begin
              (* Move cursor up that number of lines *)
              for i = 1 to !lines do
                Terminfo.puts stdout !cursor_up 1
              done;
              (* Print the input, switching to standout for the location *)
              let bol = ref true in
              for pos = 0 to String.length lb.lex_buffer - pos0 - 1 do
                if !bol then (print_char '#'; bol := false);
                if pos = loc.loc_start then
                  Terminfo.puts stdout !start_standout 1;
                if pos = loc.loc_end then
                  Terminfo.puts stdout !end_standout 1;
                let c = lb.lex_buffer.[pos + pos0] in
                print_char c;
                bol := (c = '\n') 
              done;
              true
            end
          end

(* Print the location in some way or another *)

open Format

let print loc =
  if String.length !input_name = 0 then
    if highlight_location loc then () else begin
      print_string "Characters ";
      print_int loc.loc_start; print_string "-";
      print_int loc.loc_end; print_string ":";
      force_newline()
    end
  else begin
    let (linenum, linebeg) = line_pos_file !input_name loc.loc_start in
    print_string "File \""; print_string !input_name;
    print_string "\", line "; print_int linenum;
    print_string ", characters "; print_int (loc.loc_start - linebeg);
    print_string "-"; print_int (loc.loc_end - linebeg);
    print_string ":";
    force_newline()
  end

let print_warning loc msg =
  print loc;
  print_string "Warning: "; print_string msg; print_newline()
