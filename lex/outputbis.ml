(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Output the DFA tables and its entry points *)

open Printf
open Lexgen
open Common

let output_auto_defs oc has_refill =
  if has_refill then
    output_string oc
      "exception Ocaml_lex_refill of (Lexing.lexbuf -> int)\n\n";

  fprintf oc "let __ocaml_lex_init_lexbuf lexbuf mem_size =\
\n  let pos = lexbuf.Lexing.lex_curr_pos in\
\n  lexbuf.Lexing.lex_mem <- Array.create mem_size (-1) ;\
\n  lexbuf.Lexing.lex_start_pos <- pos ;\
\n  lexbuf.Lexing.lex_last_pos <- pos ;\
\n  lexbuf.Lexing.lex_last_action <- -1\
\n\n\
" ;

  let on_refill =
    if has_refill then
      "-1"
    else
        "lexbuf.Lexing.refill_buff lexbuf ;\
\n      __ocaml_lex_next_char lexbuf"
  in
  fprintf oc
    "let rec __ocaml_lex_next_char lexbuf =\
\n  if lexbuf.Lexing.lex_curr_pos >= lexbuf.Lexing.lex_buffer_len then begin\
\n    if lexbuf.Lexing.lex_eof_reached then\
\n      256\
\n    else begin\
\n      %s\
\n    end\
\n  end else begin\
\n    let i = lexbuf.Lexing.lex_curr_pos in\
\n    let c = lexbuf.Lexing.lex_buffer.[i] in\
\n    lexbuf.Lexing.lex_curr_pos <- i+1 ;\
\n    Char.code c\
\n  end\
\n\n"
    on_refill


let output_pats oc pats = List.iter (fun p -> fprintf oc "|%d" p) pats

let output_action oc mems r =
  output_memory_actions "    " oc mems ;
  match r with
  | Backtrack ->
    fprintf oc
      "    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;\n" ;
    fprintf oc "    lexbuf.Lexing.lex_last_action\n"
  | Goto n ->
    fprintf oc "    __ocaml_lex_state%d lexbuf\n" n

let output_pat oc i =
  if i >= 256 then
    fprintf oc "|eof"
  else
    fprintf oc "|'%s'" (Char.escaped (Char.chr i))

let output_clause oc pats mems r =
  fprintf oc "(* " ;
  List.iter (output_pat oc) pats ;
  fprintf oc " *)\n" ;
  fprintf oc "  %a ->\n" output_pats pats ;  output_action oc mems r

let output_default_clause oc mems r =
  fprintf oc "  | _ ->\n" ; output_action oc mems r


let output_moves oc moves =
  let t = Hashtbl.create 17 in
  let add_move i (m,mems) =
    let mems,r = try Hashtbl.find t m with Not_found -> mems,[] in
    Hashtbl.replace t m (mems,(i::r)) in

  for i = 0 to 256 do
    add_move i moves.(i)
  done ;

  let most_frequent = ref Backtrack
  and most_mems = ref []
  and size = ref 0 in
  Hashtbl.iter
    (fun m (mems,pats) ->
      let size_m = List.length pats in
      if size_m > !size then begin
        most_frequent := m ;
        most_mems := mems ;
        size := size_m
      end)
    t ;
  Hashtbl.iter
    (fun m (mems,pats) ->
      if m <> !most_frequent then output_clause oc (List.rev pats) mems m)
    t ;
  output_default_clause oc !most_mems !most_frequent


let output_tag_actions pref oc mvs =
  output_string oc "(*" ;
  List.iter
    (fun i -> match i with
    | SetTag (t,m) -> fprintf oc " t%d <- [%d] ;" t m
    | EraseTag t -> fprintf oc " t%d <- -1 ;" t)
    mvs ;
  output_string oc " *)\n" ;
  List.iter
    (fun i ->  match i with
    | SetTag (t,m) ->
        fprintf oc "%s%a <- %a ;\n"
          pref output_mem_access t output_mem_access m
    | EraseTag t ->
        fprintf oc "%s%a <- -1 ;\n"
          pref output_mem_access t)
    mvs

let output_trans pref oc has_refill i trans =
  let self = sprintf "__ocaml_lex_state%d" i in
  fprintf oc "%s %s lexbuf = " pref self;
  match trans with
  | Perform (n,mvs) ->
      output_tag_actions "  " oc mvs ;
      fprintf oc "  %d\n" n
  | Shift (trans, move) ->
    let self =
      match trans with
      | Remember (n,mvs) ->
        output_tag_actions "  " oc mvs ;
        fprintf oc
          "  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;\n" ;
        fprintf oc "  lexbuf.Lexing.lex_last_action <- %d ;\n" n;
        if has_refill then
          let next = sprintf "__ocaml_lex_state%d_next" i in
          fprintf oc "  %s lexbuf\n" next;
          fprintf oc "and %s lexbuf = " next;
          next
        else
          self
      | No_remember ->
        self
    in
    output_string oc "match __ocaml_lex_next_char lexbuf with\n";
    if has_refill then
      fprintf oc "\n  | -1 -> raise (Ocaml_lex_refill %s)\n" self;
    output_moves oc move

let output_automata oc has_refill auto =
  output_auto_defs oc has_refill;
  let n = Array.length auto in
  output_trans "let rec" oc has_refill 0 auto.(0) ;
  for i = 1 to n-1 do
    output_trans "\nand" oc has_refill i auto.(i)
  done ;
  output_char oc '\n'


(* Output the entries *)

let output_entry_refill oc e =
  let init_num, init_moves = e.auto_initial_state in
  fprintf oc "%s %alexbuf =\
\n  __ocaml_lex_init_lexbuf lexbuf %d; %a\
\n  __ocaml_lex_%s_rec __ocaml_lex_state%d %alexbuf
\nand __ocaml_lex_%s_rec __ocaml_lex_state %alexbuf =
\n  try
\n    let __ocaml_lex_result = __ocaml_lex_state lexbuf in\
\n    lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;\
\n    lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with\
\n      Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos+lexbuf.Lexing.lex_curr_pos};\
\n    match __ocaml_lex_result with\n"
    e.auto_name output_args e.auto_args
    e.auto_mem_size
    (output_memory_actions "  ") init_moves
    e.auto_name init_num output_args e.auto_args
    e.auto_name output_args e.auto_args

let output_entry_close_refill oc e =
  fprintf oc "  with Ocaml_lex_refill __ocaml_lex_state ->\
            \n  __ocaml_lex_refill \
                 (fun lexbuf -> lexbuf.Lexing.refill_buff lexbuf;\
            \n      __ocaml_lex_%s_rec __ocaml_lex_state %alexbuf)\
            \n    lexbuf"
    e.auto_name output_args e.auto_args

let output_entry_classic oc e =
  let init_num, init_moves = e.auto_initial_state in
  fprintf oc "%s %alexbuf =\
\n  __ocaml_lex_init_lexbuf lexbuf %d; %a\
\n  let __ocaml_lex_result = __ocaml_lex_state%d lexbuf in\
\n  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;\
\n  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with\
\n    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos+lexbuf.Lexing.lex_curr_pos};\
\n  match __ocaml_lex_result with\n"
    e.auto_name
    output_args e.auto_args
    e.auto_mem_size
    (output_memory_actions "  ") init_moves
    init_num


let output_entry sourcefile ic oc has_refill tr e =
  if has_refill then
    output_entry_refill oc e
  else
    output_entry_classic oc e;
  List.iter
    (fun (num, env, loc) ->
      fprintf oc "  | ";
      fprintf oc "%d ->\n" num;
      output_env ic oc tr env ;
      copy_chunk ic oc tr loc true;
      fprintf oc "\n")
    e.auto_actions;
  fprintf oc "  | _ -> raise (Failure \"lexing: empty token\")\n";
  if has_refill then
    output_entry_close_refill oc e;
  output_string oc "\n\n"


(* Main output function *)

let output_lexdef sourcefile ic oc tr header rh
                  entry_points transitions trailer =

  copy_chunk ic oc tr header false;
  let has_refill = output_refill_handler ic oc tr rh in
  output_automata oc has_refill transitions;
  begin match entry_points with
    [] -> ()
  | entry1 :: entries ->
    output_string oc "let rec ";
    output_entry sourcefile ic oc has_refill tr entry1;
      List.iter
        (fun e -> output_string oc "and ";
          output_entry sourcefile ic oc has_refill tr e)
        entries;
      output_string oc ";;\n\n";
  end;
  copy_chunk ic oc tr trailer false
