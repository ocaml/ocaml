(*************************************************************************)
(*                                                                       *)
(*                                OCaml                                  *)
(*                                                                       *)
(*         Damien Doligez, Jane Street Capital                           *)
(*                                                                       *)
(*   Copyright 2015 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

Random.init 12345;;

let size = 10000;;
let duration = 300;;

type block = int array;;

type objdata =
  | Present of block
  | Absent of int  (* GC count at time of erase *)
;;

type bunch = {
  objs : objdata array;
  wp : block Weak.t;
};;

let data =
    {
      objs = Array.make size (Absent 0);
      wp = Weak.create size;
    }
;;

let gccount () = (Gc.quick_stat ()).Gc.major_collections;;

(*
   Check the correctness condition on the data at [i]:
   1. if the block is present, the weak pointer must be full
   2. if the block was removed at GC n, and the weak pointer is still
      full, then the current GC must be at most n+1.
*)
let check i =
  let gc1 = gccount () in
  match data.objs.(i), Weak.check data.wp i with
  | Present _, false ->
    Printf.eprintf "error (early erasure):\n\
                   \  strong pointer is still present,\n\
                   \  weak pointer was erased\n";
      exit 3;
  | Absent n, true ->
    if gc1 > n+1 then begin
      Printf.eprintf "error (late erasure):\n\
                     \  strong pointer erased at GC %d\n\
                     \  weak pointer still present at GC %d\n" n gc1;
      exit 3;
    end
  | _ -> ()
;;

let replace gc1 i =
  let x = Array.make (1 + Random.int 10) 42 in
  data.objs.(i) <- Present x;
  Weak.set data.wp i (Some x);
;;

(* This function allows its caller to check for the presence of data
   without holding on to its value as pattern-matching does when
   compiled to byte-code. *)
let is_present i =
  match data.objs.(i) with
  | Present _ -> true
  | Absent _ -> false
;;

(*
   Modify the data at [i] in one of the following ways:
   1. if the block and weak pointer are absent, fill them
   2. if the block and weak pointer are present, randomly erase the block
   3. in other cases, check the entry and randomly overwrite it
*)
let change i =
  let gc1 = gccount () in
  match is_present i, Weak.check data.wp i with
  | false, false -> replace gc1 i;
  | true, true ->
    if Random.int 2 = 0 then begin
      data.objs.(i) <- Absent gc1;
      let gc2 = gccount () in
      if gc1 <> gc2 then data.objs.(i) <- Absent gc2;
    end
  | _ ->
    check i;
    if Random.int 2 = 0 then replace gc1 i;
;;

while gccount () < duration do
  for i = 0 to size - 1 do
    change i;
  done;
done
