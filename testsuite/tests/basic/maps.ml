(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

module IntMap = Map.Make(struct type t = int let compare x y = x-y end)

let m1 = IntMap.add 4 "Y" (IntMap.singleton 3 "X1")
let m2 = IntMap.add 4 "Y" (IntMap.singleton 5 "X2")

let show m = IntMap.iter (fun k v -> Printf.printf "%d %s\n" k v) m

let () =
  print_endline "Union+concat";
  show (IntMap.merge (fun _ l r -> match l, r with Some x, None | None, Some x -> Some x | Some x, Some y -> Some (x ^ x) | _ -> assert false) m1 m2);
  print_endline "Inter";
  show (IntMap.merge (fun _ l r -> match l, r with Some x, Some y when x = y -> Some x | _ -> None) m1 m2);
  ()

