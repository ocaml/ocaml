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

(* Compiling a lexer definition *)

open Syntax

(* Deep abstract syntax for regular expressions *)

type regexp =
    Empty
  | Chars of int
  | Action of int
  | Seq of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp

(* From shallow to deep syntax *)

let chars = ref ([] : char list list)
let chars_count = ref 0
let actions = ref ([] : (int * location) list)
let actions_count = ref 0

let rec encode_regexp = function
    Epsilon -> Empty
  | Characters cl ->
      let n = !chars_count in
      chars := cl :: !chars;
      incr chars_count;
      Chars(n)
  | Sequence(r1,r2) ->
      Seq(encode_regexp r1, encode_regexp r2)
  | Alternative(r1,r2) ->
      Alt(encode_regexp r1, encode_regexp r2)
  | Repetition r ->
      Star (encode_regexp r)

let encode_casedef =
  List.fold_left
   (fun reg (expr,act) ->
     let act_num = !actions_count in
     incr actions_count;
     actions := (act_num, act) :: !actions;
     Alt(reg, Seq(encode_regexp expr, Action act_num)))
  Empty

let encode_lexdef (Lexdef(_, ld)) =
  chars := [];
  chars_count := 0;
  actions := [];
  actions_count := 0;
  let name_regexp_list =
    List.map (fun (name, casedef) -> (name, encode_casedef casedef)) ld in
  let chr = Array.of_list (List.rev !chars)
  and act = !actions in
  chars := [];
  actions := [];
  (chr, name_regexp_list, act)

(* To generate directly a NFA from a regular expression.
   Confer Aho-Sethi-Ullman, dragon book, chap. 3 *)

type transition =
    OnChars of int
  | ToAction of int

let rec merge_trans s1 s2 =
  match (s1, s2) with
    ([], _) -> s2
  | (_, []) -> s1
  | ((OnChars n1 as t1) :: r1, (OnChars n2 as t2) :: r2) ->
      if n1 == n2 then t1 :: merge_trans r1 r2 else
      if n1 <  n2 then t1 :: merge_trans r1 s2 else
                       t2 :: merge_trans s1 r2
  | ((ToAction n1 as t1) :: r1, (ToAction n2 as t2) :: r2) ->
      if n1 == n2 then t1 :: merge_trans r1 r2 else
      if n1 <  n2 then t1 :: merge_trans r1 s2 else
                       t2 :: merge_trans s1 r2
  | ((OnChars n1 as t1) :: r1, (ToAction n2 as t2) :: r2) ->
      t1 :: merge_trans r1 s2
  | ((ToAction n1 as t1) :: r1, (OnChars n2 as t2) :: r2) ->
      t2 :: merge_trans s1 r2

let rec nullable = function
    Empty      -> true
  | Chars _    -> false
  | Action _   -> false
  | Seq(r1,r2) -> nullable r1 & nullable r2
  | Alt(r1,r2) -> nullable r1 or nullable r2
  | Star r     -> true

let rec firstpos = function
    Empty      -> []
  | Chars pos  -> [OnChars pos]
  | Action act -> [ToAction act]
  | Seq(r1,r2) -> if nullable r1
                  then merge_trans (firstpos r1) (firstpos r2)
                  else firstpos r1
  | Alt(r1,r2) -> merge_trans (firstpos r1) (firstpos r2)
  | Star r     -> firstpos r

let rec lastpos = function
    Empty      -> []
  | Chars pos  -> [OnChars pos]
  | Action act -> [ToAction act]
  | Seq(r1,r2) -> if nullable r2
                  then merge_trans (lastpos r1) (lastpos r2)
                  else lastpos r2
  | Alt(r1,r2) -> merge_trans (lastpos r1) (lastpos r2)
  | Star r     -> lastpos r

let followpos size name_regexp_list =
  let v = Array.new size [] in
    let fill_pos first = function
        OnChars pos -> v.(pos) <- merge_trans first v.(pos); ()
      | ToAction _  -> () in
    let rec fill = function
        Seq(r1,r2) ->
          fill r1; fill r2;
          List.iter (fill_pos (firstpos r2)) (lastpos r1)
      | Alt(r1,r2) ->
          fill r1; fill r2
      | Star r ->
          fill r;
          List.iter (fill_pos (firstpos r)) (lastpos r)
      | _ -> () in
    List.iter (fun (name, regexp) -> fill regexp) name_regexp_list;
    v

let no_action = 32767

let split_trans_set = List.fold_left
  (fun (act, pos_set as act_pos_set) ->
     function OnChars pos   -> (act, pos :: pos_set)
         |    ToAction act1 -> if act1 < act then (act1, pos_set)
                                             else act_pos_set)
  (no_action, [])

let memory  = (Hashtbl.new 131 : (transition list, int) Hashtbl.t)
and todo    = ref ([] : (transition list * int) list)
and next    = ref 0

let reset_state_mem () =
  Hashtbl.clear memory; todo := []; next := 0; ()

let get_state st = 
  try
    Hashtbl.find memory st
  with Not_found ->
    let nbr = !next in
    incr next;
    Hashtbl.add memory st nbr;
    todo := (st, nbr) :: !todo;
    nbr

let rec map_on_states f =
  match !todo with
    []  -> []
  | (st,i)::r -> todo := r; let res = f st in (res,i) :: map_on_states f

let number_of_states () =
  !next

let goto_state = function
    [] -> Backtrack
  | ps -> Goto (get_state ps)

let transition_from chars follow pos_set = 
  let tr = Array.new 256 []
  and shift = Array.new 256 Backtrack in
    List.iter
      (fun pos ->
        List.iter
          (fun c ->
             tr.(Char.code c) <-
               merge_trans tr.(Char.code c) follow.(pos))
          chars.(pos))
      pos_set;
    for i = 0 to 255 do
      shift.(i) <- goto_state tr.(i)
    done;
    shift

let translate_state chars follow state =
  match split_trans_set state with
    n, [] -> Perform n
  | n, ps -> Shift( (if n == no_action then No_remember else Remember n),
                    transition_from chars follow ps)

let make_dfa lexdef =
  let (chars, name_regexp_list, actions) =
    encode_lexdef lexdef in
  let follow =
    followpos (Array.length chars) name_regexp_list in
  reset_state_mem();
  let initial_states =
    List.map (fun (name, regexp) -> (name, get_state(firstpos regexp)))
        name_regexp_list in
  let states =
    map_on_states (translate_state chars follow) in
  let v =
    Array.new (number_of_states()) (Perform 0) in
  List.iter (fun (auto, i) -> v.(i) <- auto) states;
  reset_state_mem();
  (initial_states, v, actions)
