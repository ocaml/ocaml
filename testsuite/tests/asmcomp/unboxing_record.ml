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

type 'a record =
  { mutable a : int;
    mutable b : 'a }

let () =
  let x0 = Gc.allocated_bytes () in
  let x1 = Gc.allocated_bytes () in
  let record = { a = 0; b = 0. } in
  for i = 0 to 1000 do
    record.a <- record.a + 1;
    record.b <- float record.a +. record.b;
  done;
  let x2 = Gc.allocated_bytes () in
  let v1 = record.a in
  let v2 = record.b +. 0. in
  assert(v1 = 1001);
  assert(abs_float (v2 -. float ((1001 * 1002) / 2)) < 0.01);
  assert(x1 -. x0 = x2 -. x1)
  (* check that we have not allocated anything between x1 and x2 *)
