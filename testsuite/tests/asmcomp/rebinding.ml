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

(* Check the effectiveness of eliminating useless intermediate allocation *)


let f x =
  let a = (x,x) in
  let (b,_) = a in
  b

let () =
  let l = [1;2;3;4;5;6;7;8;9] in
  let x0 = Gc.allocated_bytes () in
  let x1 = Gc.allocated_bytes () in
  for i = 1 to 1000 do
    ignore (f l)
  done;
  let x2 = Gc.allocated_bytes () in
  assert(x1 -. x0 = x2 -. x1)
     (* check that we have not allocated anything between x1 and x2 *)
