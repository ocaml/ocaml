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

(* Compaction of an automata *)

open Lexgen

(* Determine the integer occurring most frequently in an array *)

let most_frequent_elt v =
  let frequencies = Hashtbl.new 17 in
  let max_freq = ref 0 in
  let most_freq = ref (v.(0)) in
  for i = 0 to Array.length v - 1 do
    let e = v.(i) in
    let r =
      try
        Hashtbl.find frequencies e
      with Not_found ->
        let r = ref 1 in Hashtbl.add frequencies e r; r in
    incr r;
    if !r > !max_freq then begin max_freq := !r; most_freq := e end
  done;
  !most_freq

(* Transform an array into a list of (position, non-default element) *)

let non_default_elements def v =
  let rec nondef i =
    if i >= Array.length v then [] else begin
      let e = v.(i) in
      if e = def then nondef(i+1) else (i, e) :: nondef(i+1)
    end in
  nondef 0

(* Compact the transition and check arrays *)

let trans = ref(Array.new 1024 0)
and check = ref(Array.new 1024 (-1))
and last_used = ref 0

let grow_transitions () =
  let old_trans = !trans
  and old_check = !check in
  let n = Array.length old_trans in
  trans := Array.new (2*n) 0;
  Array.blit old_trans 0 !trans 0 !last_used;
  check := Array.new (2*n) (-1);
  Array.blit old_check 0 !check 0 !last_used

let pack_moves state_num move_t =
  let move_v = Array.new 256 0 in
  for i = 0 to 255 do
    move_v.(i) <-
      (match move_t.(i) with
        Backtrack -> -1
      | Goto n -> n)
  done;
  let default = most_frequent_elt move_v in
  let nondef = non_default_elements default move_v in
  let rec pack_from b =
    while b + 256 > Array.length !trans do grow_transitions() done;
    let rec try_pack = function
      [] -> b
    | (pos, v) :: rem ->
        if !check.(b + pos) = -1 then try_pack rem else pack_from (b+1) in
    try_pack nondef in
  let base = pack_from 0 in
  List.iter
    (fun (pos, v) ->
      !trans.(base + pos) <- v;
      !check.(base + pos) <- state_num)
    nondef;
  if base + 256 > !last_used then last_used := base + 256;
  (base, default)

(* Build the tables *)

type lex_tables =
  { tbl_base: int array;                 (* Perform / Shift *)
    tbl_backtrk: int array;              (* No_remember / Remember *)
    tbl_default: int array;              (* Default transition *)
    tbl_trans: int array;                (* Transitions (compacted) *)
    tbl_check: int array }               (* Check (compacted) *)

let compact_tables state_v =
  let n = Array.length state_v in
  let base = Array.new n 0
  and backtrk = Array.new n (-1)
  and default = Array.new n 0 in
  for i = 0 to n - 1 do
    match state_v.(i) with
      Perform n ->
        base.(i) <- -(n+1)
    | Shift(trans, move) ->
        begin match trans with
          No_remember -> ()
        | Remember n -> backtrk.(i) <- n
        end;
        let (b, d) = pack_moves i move in
        base.(i) <- b;
        default.(i) <- d
  done;
  { tbl_base = base;
    tbl_backtrk = backtrk;
    tbl_default = default;
    tbl_trans = Array.sub !trans 0 !last_used;
    tbl_check = Array.sub !check 0 !last_used }


