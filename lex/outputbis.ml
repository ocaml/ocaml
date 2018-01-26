(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Output the DFA tables and its entry points *)

open Printf
open Lexgen
open Common

type ctx = {
  oc: out_channel;
  has_refill: bool;
  goto_state: (ctx -> int -> unit);
  last_action: int option;
}

let pr ctx = fprintf ctx.oc

let output_auto_defs ctx =
  if ctx.has_refill then
    pr ctx {|
let rec __ocaml_lex_refill_buf lexbuf _buf _len _curr _last _last_action state k =
  if lexbuf.Lexing.lex_eof_reached then
    state lexbuf _last_action _buf _len _curr _last k 256
  else begin
    lexbuf.Lexing.lex_curr_pos <- _curr;
    lexbuf.Lexing.lex_last_pos <- _last;
    __ocaml_lex_refill
      (fun lexbuf ->
        let _curr = lexbuf.Lexing.lex_curr_pos in
        let _last = lexbuf.Lexing.lex_last_pos in
        let _len = lexbuf.Lexing.lex_buffer_len in
        let _buf = lexbuf.Lexing.lex_buffer in
        if _curr < _len then
          state lexbuf _last_action _buf _len (_curr + 1) _last k
            (Char.code (Bytes.unsafe_get _buf _curr))
        else
          __ocaml_lex_refill_buf lexbuf _buf _len _curr _last _last_action
            state k
      )
      lexbuf
  end
|}
  else
    pr ctx {|
let rec __ocaml_lex_refill_buf lexbuf _buf _len _curr _last =
  if lexbuf.Lexing.lex_eof_reached then
    256, _buf, _len, _curr, _last
  else begin
    lexbuf.Lexing.lex_curr_pos <- _curr;
    lexbuf.Lexing.lex_last_pos <- _last;
    lexbuf.Lexing.refill_buff lexbuf;
    let _curr = lexbuf.Lexing.lex_curr_pos in
    let _last = lexbuf.Lexing.lex_last_pos in
    let _len = lexbuf.Lexing.lex_buffer_len in
    let _buf = lexbuf.Lexing.lex_buffer in
    if _curr < _len then
      Char.code (Bytes.unsafe_get _buf _curr), _buf, _len, (_curr + 1), _last
    else
      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
  end

|}

let output_memory_actions pref oc = function
  | []  -> ()
  | mvs ->
      output_string oc "(* " ;
  fprintf oc "L=%d " (List.length mvs) ;
  List.iter
    (fun mv -> match mv with
    | Copy (tgt, src) ->
        fprintf oc "[%d] <- [%d] ;" tgt src
    | Set tgt ->
        fprintf oc "[%d] <- p ; " tgt)
    mvs ;
  output_string oc " *)\n" ;
  List.iter
    (fun mv -> match mv with
    | Copy (tgt, src) ->
        fprintf oc
          "%s%a <- %a ;\n"
          pref output_mem_access tgt output_mem_access src
    | Set tgt ->
        fprintf oc "%s%a <- _curr;\n"
          pref output_mem_access tgt)
    mvs


let output_pats ctx pats = List.iter (fun p -> pr ctx "|%d" p) pats

let last_action ctx =
  match ctx.last_action with
  | None -> "_last_action"
  | Some i -> Printf.sprintf "%i (* = last_action *)" i

let output_action ctx mems r =
  output_memory_actions "    " ctx.oc mems ;
  match r with
  | Backtrack ->
      pr ctx {|
    let _curr = _last in
    lexbuf.Lexing.lex_curr_pos <- _curr;
    lexbuf.Lexing.lex_last_pos <- _last;
    |};
      if ctx.has_refill then
        pr ctx "    k lexbuf %s\n" (last_action ctx)
      else
        pr ctx "    %s\n" (last_action ctx)
  | Goto n ->
      ctx.goto_state ctx n

let output_pat ctx i =
  if i >= 256 then
    pr ctx "|eof"
  else
    pr ctx "|'%s'" (Char.escaped (Char.chr i))

let output_clause ctx pats mems r =
  pr ctx "(* " ;
  List.iter (output_pat ctx) pats ;
  pr ctx " *)\n  " ;
  output_pats ctx pats;
  pr ctx " ->\n";
  output_action ctx mems r

let output_default_clause ctx mems r =
  pr ctx "  | _ ->\n" ; output_action ctx mems r


let output_moves ctx moves =
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
       if m <> !most_frequent then
         output_clause ctx (List.rev pats) mems m)
    t ;
  output_default_clause ctx !most_mems !most_frequent


let output_tag_actions pref ctx mvs =
  pr ctx "(*" ;
  List.iter
    (fun i -> match i with
    | SetTag (t,m) -> pr ctx " t%d <- [%d] ;" t m
    | EraseTag t -> pr ctx " t%d <- -1 ;" t)
    mvs ;
  pr ctx " *)\n" ;
  List.iter
    (fun i ->  match i with
    | SetTag (t,m) ->
        pr ctx "%s%a <- %a ;\n"
          pref output_mem_access t output_mem_access m
    | EraseTag t ->
        pr ctx "%s%a <- -1 ;\n"
          pref output_mem_access t)
    mvs

let output_trans_body ctx = function
  | Perform (n,mvs) ->
      output_tag_actions "  " ctx mvs ;
      pr ctx  {|
  lexbuf.Lexing.lex_curr_pos <- _curr;
  lexbuf.Lexing.lex_last_pos <- _last;
|};
      pr ctx "  %s%d\n"
        (if ctx.has_refill then Printf.sprintf "k lexbuf " else "")
        n
  | Shift (trans, move) ->
      let ctx =
        match trans with
        | Remember (n,mvs) ->
            output_tag_actions "  " ctx mvs ;
            pr ctx
              "  let _last = _curr in\n";
            begin match ctx.last_action with
            | Some i when i = n ->
                pr ctx "  (* let _last_action =  %d in*)\n" n;
                ctx
            | _ ->
                pr ctx "  let _last_action = %d in\n" n;
                {ctx with last_action = Some n}
            end
        | No_remember ->
            ctx
      in
      if ctx.has_refill then begin
        (* TODO: bind this 'state' function at toplevel instead *)
        pr ctx {|
  let state lexbuf _last_action _buf _len _curr _last k = function
|};
      output_moves ctx move;
      pr ctx {|
  in
  if _curr >= _len then
     __ocaml_lex_refill_buf lexbuf _buf _len _curr _last _last_action state k
  else
     state lexbuf _last_action _buf _len (_curr + 1) _last k
       (Char.code (Bytes.unsafe_get _buf _curr))
|}
      end
      else begin
        pr ctx {|
  let next_char, _buf, _len, _curr, _last =
     if _curr >= _len then
       __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
     else
       Char.code (Bytes.unsafe_get _buf _curr),
       _buf, _len, (_curr + 1), _last
  in
  begin match next_char with
|};
        output_moves ctx move;
        pr ctx "end\n"
      end

let output_automata ctx auto inline =
  output_auto_defs ctx;
  let n = Array.length auto in
  let first = ref true in
  for i = 0 to n-1 do
    if not inline.(i) then begin
      pr ctx
        "%s __ocaml_lex_state%d lexbuf _last_action _buf _len _curr _last %s= "
        (if !first then "let rec" else "\n and")
        i
        (if ctx.has_refill then "k " else "");
      output_trans_body ctx auto.(i);
      first := false;
    end
  done;
  output_char ctx.oc '\n'


(* Output the entries *)

let output_entry ic ctx tr e =
  let init_num, init_moves = e.auto_initial_state in
  pr ctx "%s %alexbuf =\n" e.auto_name output_args e.auto_args;
  if e.auto_mem_size > 0 then
    pr ctx "  lexbuf.Lexing.lex_mem <- Array.make %d (-1);" e.auto_mem_size;
  pr ctx {|
  let _curr = lexbuf.Lexing.lex_curr_pos in
  let _last = _curr in
  let _len = lexbuf.Lexing.lex_buffer_len in
  let _buf = lexbuf.Lexing.lex_buffer in
  let _last_action = -1 in
  lexbuf.Lexing.lex_start_pos <- _curr;
|};
  output_memory_actions "  " ctx.oc init_moves;
  if ctx.has_refill
  then pr ctx "\n  let k = (fun lexbuf __ocaml_lex_result ->"
  else begin
    pr ctx "\n  let __ocaml_lex_result =\n";
    ctx.goto_state ctx init_num;
    pr ctx "in\n";
  end;
  pr ctx "\
\n  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;\
\n  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with\
\n    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos+lexbuf.Lexing.lex_curr_pos};\
\n  match __ocaml_lex_result with\n";
  List.iter
    (fun (num, env, loc) ->
      pr ctx "  | ";
      pr ctx "%d ->\n" num;
      output_env ic ctx.oc tr env ;
      copy_chunk ic ctx.oc tr loc true;
      pr ctx "\n")
    e.auto_actions;
  pr ctx "  | _ -> raise (Failure \"lexing: empty token\")\n";
  if ctx.has_refill then begin
    pr ctx "  )\n  in  \n";
    ctx.goto_state ctx init_num
  end
  else
    pr ctx "\n\n"


(* Main output function *)

let output_lexdef ic oc tr header rh
                  entry_points transitions trailer =

  copy_chunk ic oc tr header false;
  let has_refill = output_refill_handler ic oc tr rh in
  let counters = Array.make (Array.length transitions) 0 in
  let count i = counters.(i) <- counters.(i) + 1 in
  List.iter (fun e -> count (fst e.auto_initial_state)) entry_points;
  Array.iter
    (function
      | Shift (_, a) ->
          let tbl = Hashtbl.create 8 in
          Array.iter
            (function
              | (Goto i, _) when not (Hashtbl.mem tbl i) ->
                  Hashtbl.add tbl i (); count i
              | _ -> ()
            )
            a
      | Perform _ -> ()
    )
    transitions;
  let inline =
    Array.mapi
      (fun i -> function
         | Perform _ -> true
         | Shift _ -> counters.(i) = 1
      )
      transitions
  in
  let goto_state ctx n =
    if inline.(n) then
      output_trans_body ctx transitions.(n)
    else
      pr ctx "    __ocaml_lex_state%d lexbuf %s _buf _len _curr _last %s\n" n
        (last_action ctx)
        (if ctx.has_refill then " k" else "")
  in
  let ctx = {has_refill; oc; goto_state; last_action=None} in
  output_automata ctx transitions inline;
  begin match entry_points with
    [] -> ()
  | entry1 :: entries ->
    output_string oc "let rec ";
    output_entry ic ctx tr entry1;
      List.iter
        (fun e -> output_string oc "and ";
          output_entry ic ctx tr e)
        entries;
      output_string oc ";;\n\n";
  end;
  copy_chunk ic oc tr trailer false
