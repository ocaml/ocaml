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
  goto_state: (ctx -> string -> int -> unit);
  last_action: int option;
}

let pr ctx = fprintf ctx.oc

let output_auto_defs ctx =
  if ctx.has_refill then begin
    pr ctx "\n";
    pr ctx "let rec __ocaml_lex_refill_buf lexbuf _buf _len _curr _last \
                                           _last_action state k =\n";
    pr ctx "  if lexbuf.Lexing.lex_eof_reached then\n";
    pr ctx "    state lexbuf _last_action _buf _len _curr _last k 256\n";
    pr ctx "  else begin\n";
    pr ctx "    lexbuf.Lexing.lex_curr_pos <- _curr;\n";
    pr ctx "    lexbuf.Lexing.lex_last_pos <- _last;\n";
    pr ctx "    __ocaml_lex_refill\n";
    pr ctx "      (fun lexbuf ->\n";
    pr ctx "        let _curr = lexbuf.Lexing.lex_curr_pos in\n";
    pr ctx "        let _last = lexbuf.Lexing.lex_last_pos in\n";
    pr ctx "        let _len = lexbuf.Lexing.lex_buffer_len in\n";
    pr ctx "        let _buf = lexbuf.Lexing.lex_buffer in\n";
    pr ctx "        if _curr < _len then\n";
    pr ctx "          state lexbuf _last_action _buf _len (_curr + 1) \
                            _last k\n";
    pr ctx "            (Char.code (Bytes.unsafe_get _buf _curr))\n";
    pr ctx "        else\n";
    pr ctx "          __ocaml_lex_refill_buf lexbuf _buf _len _curr _last \
                                             _last_action\n";
    pr ctx "            state k\n";
    pr ctx "      )\n";
    pr ctx "      lexbuf\n";
    pr ctx "  end\n";
    pr ctx "\n";
  end else begin
    pr ctx "\n";
    pr ctx "let rec __ocaml_lex_refill_buf lexbuf _buf _len _curr _last =\n";
    pr ctx "  if lexbuf.Lexing.lex_eof_reached then\n";
    pr ctx "    256, _buf, _len, _curr, _last\n";
    pr ctx "  else begin\n";
    pr ctx "    lexbuf.Lexing.lex_curr_pos <- _curr;\n";
    pr ctx "    lexbuf.Lexing.lex_last_pos <- _last;\n";
    pr ctx "    lexbuf.Lexing.refill_buff lexbuf;\n";
    pr ctx "    let _curr = lexbuf.Lexing.lex_curr_pos in\n";
    pr ctx "    let _last = lexbuf.Lexing.lex_last_pos in\n";
    pr ctx "    let _len = lexbuf.Lexing.lex_buffer_len in\n";
    pr ctx "    let _buf = lexbuf.Lexing.lex_buffer in\n";
    pr ctx "    if _curr < _len then\n";
    pr ctx "      Char.code (Bytes.unsafe_get _buf _curr), _buf, _len, \
                            (_curr + 1), _last\n";
    pr ctx "    else\n";
    pr ctx "      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last\n";
    pr ctx "  end\n";
    pr ctx "\n";
  end

let output_memory_actions pref oc = function
  | []  -> ()
  | mvs ->
    output_string oc pref;
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

let output_pats ctx = function
  | [x] -> pr ctx "| %d" x
  | pats -> List.iter (fun p -> pr ctx "|%d" p) pats

let last_action ctx =
  match ctx.last_action with
  | None -> "_last_action"
  | Some i -> Printf.sprintf "%i (* = last_action *)" i

let output_action ctx pref mems r =
  output_memory_actions pref ctx.oc mems;
  match r with
  | Backtrack ->
      pr ctx "%slet _curr = _last in\n\
              %slexbuf.Lexing.lex_curr_pos <- _curr;\n\
              %slexbuf.Lexing.lex_last_pos <- _last;\n"
        pref pref pref;
      if ctx.has_refill then
        pr ctx "%sk lexbuf %s\n" pref (last_action ctx)
      else
        pr ctx "%s%s\n" pref (last_action ctx)
  | Goto n ->
      ctx.goto_state ctx pref n

let output_pat ctx i =
  if i >= 256 then
    pr ctx "|eof"
  else
    pr ctx "|'%s'" (Char.escaped (Char.chr i))

let output_clause ctx pref pats mems r =
  pr ctx "%s(* " pref;
  List.iter (output_pat ctx) pats;
  pr ctx " *)\n%s" pref;
  output_pats ctx pats;
  pr ctx " ->\n";
  output_action ctx ("  "^pref) mems r

let output_default_clause ctx pref mems r =
  pr ctx "%s| _ ->\n" pref;
  output_action ctx ("  "^pref) mems r

let output_moves ctx pref moves =
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
         output_clause ctx pref (List.rev pats) mems m)
    t ;
  output_default_clause ctx pref !most_mems !most_frequent


let output_tag_actions pref ctx mvs =
  pr ctx "%s(*" pref;
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

let output_trans_body pref ctx = function
  | Perform (n,mvs) ->
      output_tag_actions pref ctx mvs ;
      pr ctx "%slexbuf.Lexing.lex_curr_pos <- _curr;\n" pref;
      pr ctx "%slexbuf.Lexing.lex_last_pos <- _last;\n" pref;
      pr ctx "%s%s%d\n" pref (if ctx.has_refill then "k lexbuf " else "") n
  | Shift (trans, move) ->
      let ctx =
        match trans with
        | Remember (n,mvs) ->
            output_tag_actions pref ctx mvs ;
            pr ctx "%slet _last = _curr in\n" pref;
            begin match ctx.last_action with
            | Some i when i = n ->
                pr ctx "%s(* let _last_action = %d in*)\n" pref n;
                ctx
            | _ ->
                pr ctx "%slet _last_action = %d in\n" pref n;
                {ctx with last_action = Some n}
            end
        | No_remember ->
            ctx
      in
      if ctx.has_refill then begin
        (* TODO: bind this 'state' function at toplevel instead *)
        pr ctx
          "%slet state lexbuf _last_action _buf _len _curr _last k = function\n"
          pref;
        output_moves ctx pref move;
        pr ctx "%sin\n\
                %sif _curr >= _len then\n\
                %s  __ocaml_lex_refill_buf lexbuf _buf _len _curr _last \
                                                  _last_action state k\n\
                %selse\n\
                %s  state lexbuf _last_action _buf _len (_curr + 1) _last k\n\
                %s    (Char.code (Bytes.unsafe_get _buf _curr))\n"
        pref pref pref pref pref pref
      end
      else begin
        pr ctx "%slet next_char, _buf, _len, _curr, _last =\n\
                %s  if _curr >= _len then\n\
                %s    __ocaml_lex_refill_buf lexbuf _buf _len _curr _last\n\
                %s  else\n\
                %s    Char.code (Bytes.unsafe_get _buf _curr),\n\
                %s    _buf, _len, (_curr + 1), _last\n\
                %sin\n\
                %sbegin match next_char with\n"
          pref pref pref pref pref pref pref pref;
        output_moves ctx (pref ^ "  ") move;
        pr ctx "%send\n" pref
      end

let output_automata ctx auto inline =
  output_auto_defs ctx;
  let n = Array.length auto in
  let first = ref true in
  for i = 0 to n-1 do
    if not inline.(i) then begin
      pr ctx
        "%s __ocaml_lex_state%d lexbuf _last_action _buf _len _curr _last %s=\n"
        (if !first then "let rec" else "\nand")
        i
        (if ctx.has_refill then "k " else "");
      output_trans_body "  " ctx auto.(i);
      first := false;
    end
  done;
  pr ctx "\n\n"


(* Output the entries *)

let output_init ctx pref e init_moves =
  if e.auto_mem_size > 0 then
    pr ctx "%slexbuf.Lexing.lex_mem <- Array.make %d (-1);\n"
      pref e.auto_mem_size;
  pr ctx "%slet _curr = lexbuf.Lexing.lex_curr_pos in\n" pref;
  pr ctx "%slet _last = _curr in\n" pref;
  pr ctx "%slet _len = lexbuf.Lexing.lex_buffer_len in\n" pref;
  pr ctx "%slet _buf = lexbuf.Lexing.lex_buffer in\n" pref;
  pr ctx "%slet _last_action = -1 in\n" pref;
  pr ctx "%slexbuf.Lexing.lex_start_pos <- _curr;\n" pref;
  output_memory_actions pref ctx.oc init_moves

let output_rules ic ctx pref tr e =
  pr ctx "%sbegin\n" pref;
  pr ctx "%s  let _curr_p = lexbuf.Lexing.lex_curr_p in\n" pref;
  pr ctx "%s  if _curr_p != Lexing.dummy_pos then begin\n" pref;
  pr ctx "%s    lexbuf.Lexing.lex_start_p <- _curr_p;\n" pref;
  pr ctx "%s    lexbuf.Lexing.lex_curr_p <-\n" pref;
  pr ctx "%s      {_curr_p with Lexing.pos_cnum =\n" pref;
  pr ctx "%s       lexbuf.Lexing.lex_abs_pos+lexbuf.Lexing.lex_curr_pos}\n"
         pref;
  pr ctx "%s  end\n" pref;
  pr ctx "%send;\n" pref;
  pr ctx "%smatch __ocaml_lex_result with\n" pref;
  List.iter
    (fun (num, env, loc) ->
      pr ctx "%s| %d ->\n" pref num;
      output_env ic ctx.oc tr env;
      copy_chunk ic ctx.oc tr loc true;
      pr ctx "\n")
    e.auto_actions;
  pr ctx "%s| _ -> raise (Failure \"lexing: empty token\")\n" pref

let output_entry ic ctx tr e =
  let init_num, init_moves = e.auto_initial_state in
  pr ctx "%s %alexbuf =\n" e.auto_name output_args e.auto_args;

  if ctx.has_refill then begin
    pr ctx "  let k lexbuf __ocaml_lex_result =\n";
    output_rules ic ctx "    " tr e;
    pr ctx "  in\n";
    output_init ctx "  " e init_moves;
    ctx.goto_state ctx "  " init_num
  end else begin
    pr ctx "  let __ocaml_lex_result =\n";
    output_init ctx "    " e init_moves;
    ctx.goto_state ctx "    " init_num;
    pr ctx "  in\n";
    output_rules ic ctx "  " tr e
  end;
  pr ctx "\n\n"


(* Determine which states to inline *)

let choose_inlining entry_points transitions =
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
  Array.mapi
    (fun i -> function
       | Perform _ -> true
       | Shift _ -> counters.(i) = 1
    )
    transitions

let goto_state inline transitions ctx pref n =
  if inline.(n) then
    output_trans_body pref ctx transitions.(n)
  else
    pr ctx "%s__ocaml_lex_state%d lexbuf %s _buf _len _curr _last%s\n"
      pref n
      (last_action ctx)
      (if ctx.has_refill then " k" else "")

(* Main output function *)

let output_lexdef ic oc tr header rh
                  entry_points transitions trailer =

  copy_chunk ic oc tr header false;
  let has_refill = output_refill_handler ic oc tr rh in
  let inline = choose_inlining entry_points transitions in
  let ctx =
    {
      has_refill;
      oc;
      goto_state = goto_state inline transitions;
      last_action = None;
    }
  in
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
