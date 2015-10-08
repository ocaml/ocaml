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

(* Ensures that float references are correctly unboxed *)

let () =
  let x0 = Gc.allocated_bytes () in
  let x1 = Gc.allocated_bytes () in
  let r = ref 0. in
  for i = 1 to 1000 do
    r := !r +. 1.
  done;
  let x2 = Gc.allocated_bytes () in
  let v = !r +. 0. in
  assert(abs_float (v -. 1000.) < 0.01);
  assert(x1 -. x0 = x2 -. x1)
  (* check that we have not allocated anything between x1 and x2 *)
