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

(* Ensures that global exception can be statically allocated *)

exception My_exception of int

let () =
  let x0 = Gc.allocated_bytes () in
  let x1 = Gc.allocated_bytes () in
  for i = 1 to 1000 do
    try
      if i mod 2 = 0
      then raise (My_exception 123)
      else (failwith [@inlined]) "some text"
    with _ -> ()
  done;
  let x2 = Gc.allocated_bytes () in
  assert(x1 -. x0 = x2 -. x1)
     (* check that we have not allocated anything between x1 and x2 *)
