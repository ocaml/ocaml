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
and cursor_down = ref ""
and start_standout = ref ""
and end_standout = ref ""

let setup_terminal_info() =
  try
    Terminfo.setupterm();
    num_lines := Terminfo.getnum "li";
    cursor_up := Terminfo.getstr "up";
    cursor_down := Terminfo.getstr "do";
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

let num_loc_lines = ref 0 (* number of lines already printed after input *)

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
            let lines = ref !num_loc_lines in
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
              let bol = ref false in
	      print_string "# ";
              for pos = 0 to String.length lb.lex_buffer - pos0 - 1 do
                if !bol then (print_string "  "; bol := false);
                if pos = loc.loc_start then
                  Terminfo.puts stdout !start_standout 1;
                if pos = loc.loc_end then
                  Terminfo.puts stdout !end_standout 1;
                let c = lb.lex_buffer.[pos + pos0] in
                print_char c;
                bol := (c = '\n')
              done;
              (* Make sure standout mode is over *)
              Terminfo.puts stdout !end_standout 1;
              (* Position cursor back to original location *)
              for i = 1 to !num_loc_lines do
                Terminfo.puts stdout !cursor_down 1
              done;
              true
            end
          end

(* Print the location in some way or another *)

open Format

let reset () =
  num_loc_lines := 0

let (msg_file, msg_line, msg_chars, msg_to, msg_colon, warn_head) =
  match (Sys.get_config ()).Sys.os_type with
  | "MacOS" -> ("File \"", "\"; line ", "; characters ", " to ", "", "### ")
  | _ -> ("File \"", "\", line ", ", characters ", "-", ":", "")
;;

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
	print_string msg_file; print_string !input_name;
	print_string msg_line; print_int linenum;
	print_string msg_chars; print_int (loc.loc_start - linebeg);
	print_string msg_to; print_int (loc.loc_end - linebeg);
	print_string msg_colon;
    force_newline()
  end

let print_warning loc msg =
  print loc;
  print_string warn_head;
  print_string "Warning: "; print_string msg; print_newline();
  incr num_loc_lines

