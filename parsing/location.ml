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

let num_loc_lines = ref 0 (* number of lines already printed after input *)

(* Highlight the location using standout mode. *)

let highlight_terminfo ppf num_lines lb loc1 loc2 =
  (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
  let pos0 = -lb.lex_abs_pos in
  (* Do nothing if the buffer does not contain the whole phrase. *)
  if pos0 < 0 then raise Exit;
  (* Count number of lines in phrase *)
  let lines = ref !num_loc_lines in
  for i = pos0 to lb.lex_buffer_len - 1 do
    if lb.lex_buffer.[i] = '\n' then incr lines
  done;
  (* If too many lines, give up *)
  if !lines >= num_lines - 2 then raise Exit;
  (* Move cursor up that number of lines *)
  flush stdout; Terminfo.backup !lines;
  (* Print the input, switching to standout for the location *)
  let bol = ref false in
  print_string "# ";
  for pos = 0 to lb.lex_buffer_len - pos0 - 1 do
    if !bol then (print_string "  "; bol := false);
    if pos = loc1.loc_start || pos = loc2.loc_start then
      Terminfo.standout true;
    if pos = loc1.loc_end || pos = loc2.loc_end then
      Terminfo.standout false;
    let c = lb.lex_buffer.[pos + pos0] in
    print_char c;
    bol := (c = '\n')
  done;
  (* Make sure standout mode is over *)
  Terminfo.standout false;
  (* Position cursor back to original location *)
  Terminfo.resume !num_loc_lines;
  flush stdout

(* Highlight the location by printing it again. *)

let highlight_dumb ppf lb loc =
  (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
  let pos0 = -lb.lex_abs_pos in
  (* Do nothing if the buffer does not contain the whole phrase. *)
  if pos0 < 0 then raise Exit;
  let end_pos = lb.lex_buffer_len - pos0 - 1 in
  (* Determine line numbers for the start and end points *)
  let line_start = ref 0 and line_end = ref 0 in
  for pos = 0 to end_pos do
    if lb.lex_buffer.[pos + pos0] = '\n' then begin
      if loc.loc_start > pos then incr line_start;
      if loc.loc_end   > pos then incr line_end
    end
  done;
  (* Print character location (useful for Emacs) *)
  Format.fprintf ppf "Characters %i-%i:@." loc.loc_start loc.loc_end;
  (* Print the input, underlining the location *)
  print_string "  ";
  let line = ref 0 in
  let pos_at_bol = ref 0 in
  for pos = 0 to end_pos do
    let c = lb.lex_buffer.[pos + pos0] in
    if c <> '\n' then begin
      if !line = !line_start && !line = !line_end then
        (* loc is on one line: print whole line *)
        print_char c
      else if !line = !line_start then
        (* first line of multiline loc: print ... before loc_start *)
        if pos < loc.loc_start
        then print_char '.'
        else print_char c
      else if !line = !line_end then
        (* last line of multiline loc: print ... after loc_end *)
        if pos < loc.loc_end
        then print_char c
        else print_char '.'
      else if !line > !line_start && !line < !line_end then
        (* intermediate line of multiline loc: print whole line *)
        print_char c
    end else begin
      if !line = !line_start && !line = !line_end then begin
        (* loc is on one line: underline location *)
        print_string "\n  ";
        for i = !pos_at_bol to loc.loc_start - 1 do
          print_char ' '
        done;
        for i = loc.loc_start to loc.loc_end - 1 do
          print_char '^'
        done
      end;
      if !line >= !line_start && !line <= !line_end then begin
        print_char '\n';
        if pos < loc.loc_end then print_string "  "
      end;
      incr line;
      pos_at_bol := pos + 1;
    end
  done

(* Highlight the location using one of the supported modes. *)

let rec highlight_locations ppf loc1 loc2 =
  match !status with
    Terminfo.Uninitialised ->
      status := Terminfo.setup stdout; highlight_locations ppf loc1 loc2
  | Terminfo.Bad_term ->
      begin match !input_lexbuf with
        None -> false
      | Some lb ->
          let norepeat =
            try Sys.getenv "TERM" = "norepeat" with Not_found -> false in
          if norepeat then false else
            try highlight_dumb ppf lb loc1; true
            with Exit -> false
      end
  | Terminfo.Good_term num_lines ->
      begin match !input_lexbuf with
        None -> false
      | Some lb ->
          try highlight_terminfo ppf num_lines lb loc1 loc2; true
          with Exit -> false
      end

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
    if highlight_locations ppf loc none then () else
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
