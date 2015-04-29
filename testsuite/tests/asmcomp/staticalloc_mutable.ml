(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                       Pierre Chambart, OCamlPro                     *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Check the effectiveness of static allocation of toplevel mutable values. *)

type stuff = {
  mutable v : int;
  mutable l : int list;
  mutable stuff : stuff option;
}

let () =
  let x0 = Gc.allocated_bytes () in
  let x1 = Gc.allocated_bytes () in
  let v = { v = 3; l = [1;2;3]; stuff = None } in
  let v' = { v = 3; l = [1;2;3]; stuff = None } in
  assert(v != v');
  let v2 = { v = 12; l = []; stuff = Some v } in
  v.v <- 4;
  v.l <- [4;5];
  v.stuff <- Some v2;
  let p1 = (v, v) in
  let g () = (snd p1, fst p1) in
  assert (g () == p1);
  let x2 = Gc.allocated_bytes () in
  assert(x1 -. x0 = x2 -. x1)
     (* check that we did not allocated anything between x1 and x2 *)
