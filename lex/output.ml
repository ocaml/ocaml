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

(* Generating a DFA as a set of mutually recursive functions *)

open Syntax

let ic = ref stdin
and oc = ref stdout

(* 1- Generating the actions *)

let copy_buffer = String.create 1024

let copy_chunk (Location(start,stop)) =
  let rec copy s =
    if s <= 0 then () else
      let n = if s < 1024 then s else 1024 in
      let m = input !ic copy_buffer 0 n in
        output !oc copy_buffer 0 m;
        copy (s - m)
  in
    seek_in !ic start;
    copy (stop - start)

let output_action (i,act) =
  output_string !oc ("action_" ^ string_of_int i ^ " lexbuf = (\n");
  copy_chunk act;
  output_string !oc ")\nand ";
  ()

(* 2- Generating the states *)

let states = ref ([||] : automata array)

let enumerate_vect v =
  let rec enum env pos =
    if pos >= Array.length v then env else
      try
        let pl = List.assoc v.(pos) env in
          pl := pos :: !pl; enum env (succ pos)
        with Not_found ->
          enum ((v.(pos), ref [pos]) :: env) (succ pos) in
    Sort.list
      (fun (e1, pl1) (e2, pl2) -> List.length !pl1 >= List.length !pl2)
      (enum [] 0)

let output_move = function
    Backtrack ->
      output_string !oc "backtrack lexbuf"
  | Goto dest ->
      match !states.(dest) with
        Perform act_num ->
          output_string !oc ("action_" ^ string_of_int act_num ^ " lexbuf")
      | _ ->
          output_string !oc ("state_" ^ string_of_int dest ^ " lexbuf")

let output_char_for_read oc = function
    '\''  -> output_string oc "\\'"
  | '\\' -> output_string oc "\\\\"
  | '\n' -> output_string oc "\\n"
  | '\t' -> output_string oc "\\t"
  | c ->
      let n = Char.code c in
      if n >= 32 & n < 127 then
        output_char oc c
      else begin
        output_char oc '\\';
        output_char oc (Char.chr (48 + n / 100));
        output_char oc (Char.chr (48 + (n / 10) mod 10));
        output_char oc (Char.chr (48 + n mod 10))
      end

let rec output_chars = function
    [] ->
      failwith "output_chars"
  | [c] ->
      output_string !oc "'";
      output_char_for_read !oc (Char.chr c);
      output_string !oc "'"
  | c::cl ->
      output_string !oc "'";
      output_char_for_read !oc (Char.chr c);
      output_string !oc "'|";
      output_chars cl

let output_one_trans (dest, chars) =
  output_chars !chars;
  output_string !oc " -> ";
  output_move dest;
  output_string !oc "\n |  ";
  ()

let output_all_trans trans =
  output_string !oc "  match get_next_char lexbuf with\n    ";
  match enumerate_vect trans with
    [] ->
      failwith "output_all_trans"
  | (default, _) :: rest ->
      List.iter output_one_trans rest;
      output_string !oc "_ -> ";
      output_move default;
      output_string !oc "\nand ";
      ()

let output_state state_num = function
    Perform i ->
      ()
  | Shift(what_to_do, moves) ->
      output_string !oc
        ("state_"  ^ string_of_int state_num ^ " lexbuf =\n");
      begin match what_to_do with
        No_remember -> ()
      | Remember i ->
          output_string !oc "  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;\n";
          output_string !oc ("  lexbuf.lex_last_action <- Obj.magic action_" ^
                             string_of_int i ^ ";\n")
      end;
      output_all_trans moves

(* 3- Generating the entry points *)
          
let rec output_entries = function
    [] -> failwith "output_entries"
  | (name,state_num) :: rest ->
      output_string !oc (name ^ " lexbuf =\n");
      output_string !oc "  start_lexing lexbuf;\n";
      output_string !oc ("  state_" ^ string_of_int state_num ^ " lexbuf\n");
      match rest with
        [] -> output_string !oc "\n"; ()
      | _  -> output_string !oc "\nand "; output_entries rest

(* All together *)

let output_lexdef header (initial_st, st, actions) =
  print_int (Array.length st); print_string " states, ";
  print_int (List.length actions); print_string " actions.";
  print_newline();
  output_string !oc "open Obj\nopen Lexing\n\n";
  copy_chunk header;
  output_string !oc "\nlet rec ";
  states := st;
  List.iter output_action actions;
  for i = 0 to Array.length st - 1 do
    output_state i st.(i)
  done;
  output_entries initial_st


