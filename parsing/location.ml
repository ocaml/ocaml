(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Lexing

type t =
  { loc_start: int; loc_end: int; loc_ghost: bool }

let none = { loc_start = -1; loc_end = -1; loc_ghost = true }

let symbol_rloc () =
  { loc_start = Parsing.symbol_start(); loc_end = Parsing.symbol_end();
    loc_ghost = false }

let symbol_gloc () =
  { loc_start = Parsing.symbol_start(); loc_end = Parsing.symbol_end();
    loc_ghost = true }

let rhs_loc n =
  { loc_start = Parsing.rhs_start n; loc_end = Parsing.rhs_end n;
    loc_ghost = false }

let input_name = ref ""

let input_lexbuf = ref (None : lexbuf option)

(* Terminal info *)

let status = ref Terminfo.Uninitialised


(* Print the location using standout mode. *)

let num_loc_lines = ref 0 (* number of lines already printed after input *)

let rec highlight_locations loc1 loc2 =
  match !status with
    Terminfo.Uninitialised ->
      status := Terminfo.setup stdout; highlight_locations loc1 loc2
  | Terminfo.Bad_term ->
      begin match !input_lexbuf with
        None -> false
      | Some lb ->
          try
            (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
            let pos0 = -lb.lex_abs_pos in
            (* Do nothing if the buffer does not contain the whole phrase. *)
            if pos0 < 0 then raise Exit;
            (* Count number of lines in phrase *)
            let lines = ref !num_loc_lines in
            for i = pos0 to String.length lb.lex_buffer - 1 do
              if lb.lex_buffer.[i] = '\n' then incr lines
            done;
            let pos_at_bol = ref 0 in
            print_string "Toplevel input:\n# ";
            (* Print the input, switching to standout for the location *)
            for pos = 0 to String.length lb.lex_buffer - pos0 - 1 do
              let c = lb.lex_buffer.[pos + pos0] in
              if c = '\n' then begin
                if !pos_at_bol <= loc1.loc_start && loc1.loc_end <= pos then
                begin
                  print_string "\n  ";
                  for i = !pos_at_bol to loc1.loc_start - 1 do
                    print_char ' '
                  done;
                  for i = loc1.loc_start to loc1.loc_end - 1 do
                    print_char '^'
                  done;
                  print_char '\n'
                end
                else if !pos_at_bol <= loc1.loc_start && loc1.loc_start < pos
                then begin
                  print_char '\r';
                  print_char (if !pos_at_bol = 0 then '#' else ' ');
                  print_char ' ';
                  for i = !pos_at_bol to loc1.loc_start - 1 do
                    print_char '.'
                  done;
                  print_char '\n'
                end
                else if !pos_at_bol <= loc1.loc_end && loc1.loc_end < pos
                then begin
                  for i = pos - 1 downto loc1.loc_end do
                    print_string "\b.\b";
                  done;
                  print_char '\n';
                end
                else print_char '\n';
                pos_at_bol := pos + 1;
                if pos < String.length lb.lex_buffer - pos0 - 1 then
                  print_string "  "
                else ();
              end
              else print_char c;
            done;
            flush stdout;
            true;
          with Exit -> false
      end
  | Terminfo.Good_term num_lines ->
      match !input_lexbuf with
        None -> false
      | Some lb ->
          try
            (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
            let pos0 = -lb.lex_abs_pos in
            (* Do nothing if the buffer does not contain the whole phrase. *)
            if pos0 < 0 then raise Exit;
            (* Count number of lines in phrase *)
            let lines = ref !num_loc_lines in
            for i = pos0 to String.length lb.lex_buffer - 1 do
              if lb.lex_buffer.[i] = '\n' then incr lines
            done;
            (* If too many lines, give up *)
            if !lines >= num_lines - 2 then raise Exit;
            (* Move cursor up that number of lines *)
            flush stdout; Terminfo.backup !lines;
            (* Print the input, switching to standout for the location *)
            let bol = ref false in
            print_string "# ";
            for pos = 0 to String.length lb.lex_buffer - pos0 - 1 do
              if !bol then (print_string "  "; bol := false);
              if pos = loc1.loc_start || pos = loc2.loc_start then
                (flush stdout; Terminfo.standout true);
              if pos = loc1.loc_end || pos = loc2.loc_end then
                (flush stdout; Terminfo.standout false);
              let c = lb.lex_buffer.[pos + pos0] in
              print_char c;
              bol := (c = '\n')
            done;
            flush stdout;
            (* Make sure standout mode is over *)
            Terminfo.standout false;
            (* Position cursor back to original location *)
            Terminfo.resume !num_loc_lines;
            flush stdout;
            true;
          with Exit -> false

(* Print the location in some way or another *)

open Format

let reset () =
  num_loc_lines := 0

let (msg_file, msg_line, msg_chars, msg_to, msg_colon, msg_head) =
  match Sys.os_type with
  | "MacOS" -> ("File \"", "\"; line ", "; characters ", " to ", "", "### ")
  | _ -> ("File \"", "\", line ", ", characters ", "-", ":", "")

let print ppf loc =
  if String.length !input_name = 0 then
    if highlight_locations loc none then () else
      fprintf ppf "Characters %i-%i:@." loc.loc_start loc.loc_end
  else begin
    let (filename, linenum, linebeg) =
            Linenum.for_position !input_name loc.loc_start in
    fprintf ppf "%s%s%s%i" msg_file filename msg_line linenum;
    fprintf ppf "%s%i" msg_chars (loc.loc_start - linebeg);
    fprintf ppf "%s%i%s@.%s"
     msg_to (loc.loc_end - linebeg) msg_colon msg_head;
  end

let print_warning loc ppf w =
  if Warnings.is_active w then begin
    let printw ppf w =
      let n = Warnings.print ppf w in
      num_loc_lines := !num_loc_lines + n
    in
    fprintf ppf "%a" print loc;
    fprintf ppf "Warning: %a@." printw w;
    pp_print_flush ppf ();
    incr num_loc_lines;
  end
;;

let prerr_warning loc w = print_warning loc err_formatter w;;

let echo_eof () =
  print_newline ();
  incr num_loc_lines
