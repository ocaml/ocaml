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

open Lambda


(* Input: a list of (key, action) pairs, where keys are integers. *)
(* Output: a table of (low, high, offset) triples for Ktranslate
           an array of actions for Kswitch *)

let make_decision_tree casei =
  (* Sort the cases by increasing keys *)
  let cases =
    Sort.list (fun (key1,act1) (key2,act2) -> key1 <= key2) casei in
  (* Extract the keys and the actions *)
  let keyv = Array.of_list (List.map fst cases)
  and actv = Array.of_list (List.map snd cases) in
  let n = Array.length keyv in
  (* Partition the set of keys keyv into maximal dense enough segments.
     A segment is dense enough if its span (max point - min point) is
     less than four times its size (number of points). *)
  let rec partition start =
    if start >= n then [] else
    let stop = ref (n-1) in
    while let span = keyv.(!stop) - keyv.(start) in
          span >= 256 or span > 4 * (!stop - start) do
      decr stop
    done;
    (* We've found a dense enough segment.
       In the worst case, !stop = start and the segment is a single point *)
    (* Record the segment and continue *)
    (start, !stop) :: partition (!stop + 1) in
  let part = partition 0 in
  (* Build the two tables *)
  let transl = Array.new (List.length part) (0, 0, 0)
  and switch = ref [] in
  let tr_pos = ref 0
  and sw_ind = ref 1 in (* Slot 0 in switch is reserved for Lstaticfail *)
  List.iter
    (fun (start, stop) ->
      transl.(!tr_pos) <- (keyv.(start), keyv.(stop), !sw_ind);
      for i = start to stop do
        switch := (!sw_ind + keyv.(i) - keyv.(start), actv.(i)) :: !switch
      done;
      incr tr_pos;
      sw_ind := !sw_ind + keyv.(stop) - keyv.(start) + 1)
    part;
  (transl, !switch, !sw_ind)
