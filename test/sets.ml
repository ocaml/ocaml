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

module IntSet = Set.Make(struct type t = int let compare x y = x-y end)

let even = List.fold_right IntSet.add [0; -2; 2; 4; 6; -10] IntSet.empty

let odd = List.fold_right IntSet.add [9; -7; 5; 1; -3] IntSet.empty

let _ =
  for i = -10 to 10 do
    Printf.printf "%d  %b  %b\n" i (IntSet.mem i even) (IntSet.mem i odd)
  done

module IntSetSet = Set.Make(IntSet)

let setofset = List.fold_right IntSetSet.add [even; odd] IntSetSet.empty

let _ =
  List.iter
    (fun s -> Printf.printf "%b\n" (IntSetSet.mem s setofset))
    [IntSet.empty; even; odd; IntSet.union even odd]

let _ = exit 0
