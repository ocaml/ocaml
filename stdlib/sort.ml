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

(* Merging and sorting *)

open Array

let rec merge order l1 l2 =
  match l1 with
    [] -> l2
  | h1 :: t1 ->
      match l2 with
        [] -> l1
      | h2 :: t2 ->
          if order h1 h2
          then h1 :: merge order t1 l2
          else h2 :: merge order l1 t2

let list order l =
  let rec initlist = function
      [] -> []
    | [e] -> [[e]]
    | e1::e2::rest ->
        (if order e1 e2 then [e1;e2] else [e2;e1]) :: initlist rest in
  let rec merge2 = function
      l1::l2::rest -> merge order l1 l2 :: merge2 rest
    | x -> x in
  let rec mergeall = function
      [] -> []
    | [l] -> l
    | llist -> mergeall (merge2 llist) in
  mergeall(initlist l)

let swap arr i j =
  let tmp = unsafe_get arr i in
  unsafe_set arr i (unsafe_get arr j);
  unsafe_set arr j tmp

let array order arr =
  let rec qsort lo hi =
    if hi <= lo then ()
    else if hi - lo < 5 then begin
      (* Use insertion sort *)
      for i = lo + 1 to hi do
        let val_i = unsafe_get arr i in
        if order val_i (unsafe_get arr (i - 1)) then begin
          unsafe_set arr i (unsafe_get arr (i - 1));
          let j = ref (i - 1) in
          while !j >= 1 && order val_i (unsafe_get arr (!j - 1)) do
            unsafe_set arr !j (unsafe_get arr (!j - 1));
            decr j
          done;
          unsafe_set arr !j val_i
        end
      done
    end else begin
      let mid = (lo + hi) lsr 1 in
      (* Select median value from among LO, MID, and HI *)
      let pivotpos =
        let vlo = unsafe_get arr lo
        and vhi = unsafe_get arr hi
        and vmid = unsafe_get arr mid in
        if order vlo vmid then
          if order vmid vhi then mid
          else if order vlo vhi then hi else lo
        else
          if order vhi vmid then mid
          else if order vhi vlo then hi else lo in
      swap arr pivotpos hi;
      let pivot = unsafe_get arr hi in
      let i = ref lo and j = ref hi in
      while !i < !j do
        while !i < hi && order (unsafe_get arr !i) pivot do incr i done;
        while !j > lo && order pivot (unsafe_get arr !j) do decr j done;
        if !i < !j then swap arr !i !j
      done;
      swap arr !i hi;
      (* Recurse on larger half first *)
      if (!i - 1) - lo >= hi - (!i + 1) then begin
        qsort lo (!i - 1); qsort (!i + 1) hi
      end else begin
        qsort (!i + 1) hi; qsort lo (!i - 1)
      end
    end in
  qsort 0 (Array.length arr - 1)
