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

type lexer_entry =
  { lex_name: string;
    lex_regexp: regexp;
    lex_actions: (int * location) list }
    
(* Representation of automata *)

type automata =
    Perform of int
  | Shift of automata_trans * automata_move array
and automata_trans =
    No_remember
  | Remember of int
and automata_move =
    Backtrack
  | Goto of int

(* Representation of entry points *)

type automata_entry =
  { auto_name: string;
    auto_initial_state: int;
    auto_actions: (int * location) list }
    
(* From shallow to deep syntax *)

let chars = ref ([] : int list list)
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

let encode_casedef casedef =
  List.fold_left
   (fun reg (expr, act) ->
     let act_num = !actions_count in
     incr actions_count;
     actions := (act_num, act) :: !actions;
     Alt(reg, Seq(encode_regexp expr, Action act_num)))
   Empty
   casedef

let encode_lexdef def =
  chars := [];
  chars_count := 0;
  let entry_list =
    List.map
      (fun (entry_name, casedef) ->
        actions := [];
        actions_count := 0;
        let re = encode_casedef casedef in
        { lex_name = entry_name;
          lex_regexp = re;
          lex_actions = List.rev !actions })
      def.entrypoints in
  let chr = Array.of_list (List.rev !chars) in
  chars := [];
  actions := [];
  (chr, entry_list)

(* To generate directly a NFA from a regular expression.
   Confer Aho-Sethi-Ullman, dragon book, chap. 3 *)

type transition =
    OnChars of int
  | ToAction of int

module TransSet =
  Set.Make(struct type t = transition let compare = compare end)

let rec nullable = function
    Empty      -> true
  | Chars _    -> false
  | Action _   -> false
  | Seq(r1,r2) -> nullable r1 & nullable r2
  | Alt(r1,r2) -> nullable r1 or nullable r2
  | Star r     -> true

let rec firstpos = function
    Empty      -> TransSet.empty
  | Chars pos  -> TransSet.add (OnChars pos) TransSet.empty
  | Action act -> TransSet.add (ToAction act) TransSet.empty
  | Seq(r1,r2) -> if nullable r1
                  then TransSet.union (firstpos r1) (firstpos r2)
                  else firstpos r1
  | Alt(r1,r2) -> TransSet.union (firstpos r1) (firstpos r2)
  | Star r     -> firstpos r

let rec lastpos = function
    Empty      -> TransSet.empty
  | Chars pos  -> TransSet.add (OnChars pos) TransSet.empty
  | Action act -> TransSet.add (ToAction act) TransSet.empty
  | Seq(r1,r2) -> if nullable r2
                  then TransSet.union (lastpos r1) (lastpos r2)
                  else lastpos r2
  | Alt(r1,r2) -> TransSet.union (lastpos r1) (lastpos r2)
  | Star r     -> lastpos r

let followpos size entry_list =
  let v = Array.create size TransSet.empty in
  let fill_pos first = function
      OnChars pos -> v.(pos) <- TransSet.union first v.(pos)
    | ToAction _  -> () in
  let rec fill = function
      Seq(r1,r2) ->
        fill r1; fill r2;
        TransSet.iter (fill_pos (firstpos r2)) (lastpos r1)
    | Alt(r1,r2) ->
        fill r1; fill r2
    | Star r ->
        fill r;
        TransSet.iter (fill_pos (firstpos r)) (lastpos r)
    | _ -> () in
  List.iter (fun entry -> fill entry.lex_regexp) entry_list;
  v

let no_action = max_int

let split_trans_set trans_set =
  TransSet.fold
    (fun trans (act, pos_set as act_pos_set) ->
      match trans with
        OnChars pos -> (act, pos :: pos_set)
      | ToAction act1 -> if act1 < act then (act1, pos_set) else act_pos_set)
    trans_set
    (no_action, [])

module StateMap =
  Map.Make(struct type t = TransSet.t let compare = TransSet.compare end)

let state_map = ref (StateMap.empty: int StateMap.t)
let todo = (Stack.create() : (TransSet.t * int) Stack.t)
let next_state_num = ref 0

let reset_state_mem () =
  state_map := StateMap.empty;
  Stack.clear todo;
  next_state_num := 0

let get_state st = 
  try
    StateMap.find st !state_map
  with Not_found ->
    let num = !next_state_num in
    incr next_state_num;
    state_map := StateMap.add st num !state_map;
    Stack.push (st, num) todo;
    num

let map_on_all_states f =
  let res = ref [] in
  begin try
    while true do
      let (st, i) = Stack.pop todo in
      let r = f st in
      res := (r, i) :: !res
    done
  with Stack.Empty -> ()
  end;
  !res

let goto_state st =
  if TransSet.is_empty st then Backtrack else Goto (get_state st)

let transition_from chars follow pos_set = 
  let tr = Array.create 257 TransSet.empty in
  let shift = Array.create 257 Backtrack in
    List.iter
      (fun pos ->
        List.iter
          (fun c ->
             tr.(c) <- TransSet.union tr.(c) follow.(pos))
          chars.(pos))
      pos_set;
    for i = 0 to 256 do
      shift.(i) <- goto_state tr.(i)
    done;
    shift

let translate_state chars follow state =
  match split_trans_set state with
    (n, []) -> Perform n
  | (n, ps) -> Shift((if n = no_action then No_remember else Remember n),
                     transition_from chars follow ps)

let make_dfa lexdef =
  let (chars, entry_list) = encode_lexdef lexdef in
  let follow = followpos (Array.length chars) entry_list in
  reset_state_mem();
  let initial_states =
    List.map
      (fun le ->
        { auto_name = le.lex_name;
          auto_initial_state = get_state(firstpos le.lex_regexp);
          auto_actions = le.lex_actions })
      entry_list in
  let states = map_on_all_states (translate_state chars follow) in
  let actions = Array.create !next_state_num (Perform 0) in
  List.iter (fun (act, i) -> actions.(i) <- act) states;
  reset_state_mem();
  (initial_states, actions)
