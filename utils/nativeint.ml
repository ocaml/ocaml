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

(* lsb is the least significant bit of the native int (0 <= lsb <= 1)
   msw is the remaining 31 or 63 most significant bits. *)
type t = { msw: int; lsb: int }

let from i = { msw = i asr 1; lsb = i land 1 }

let to_int n = (n.msw lsl 1) lor n.lsb

let add n1 n2 =
  let s = n1.lsb + n2.lsb in
  { msw = n1.msw + n2.msw + s asr 1; lsb = s land 1 }

let sub n1 n2 =
  let d = n1.lsb - n2.lsb in
  { msw = n1.msw - n2.msw + d asr 1; lsb = d land 1 }

let shift n s =
  if s = 0 then n else
  if s > 0 then
    { msw = (n.msw lsl s) lor (n.lsb lsl (s-1)); lsb = 0 }
  else
    { msw = n.msw asr (-s); lsb = (n.msw asr (-s - 1)) land 1 }

let sign n =
  if n.msw < 0 then -1 else
  if n.msw = 0 && n.lsb = 0 then 0 else 1

let compare n1 n2 = sign(sub n1 n2)

let cmp n1 i2 = compare n1 (from i2)

let to_string n =
  let a = if n.msw >= 0 then n else sub (from 0) n in
  let (q, r) =
    (* Watch out for the case n.msw = min_int, in which case a.msw < 0 *)
    if a.msw <> min_int
    then (a.msw / 5, a.msw mod 5)
    else (max_int / 5, max_int mod 5 + 1) in
  (if n.msw >= 0 then "" else "-") ^
  (if q > 0 then string_of_int q else "") ^
  string_of_int (r * 2 + a.lsb)

let to_hexa_string n =
  let a = if n.msw >= 0 then n else sub (from 0) n in
  let q = a.msw lsr 3 in
  let r = a.msw land 0x7 in
  Printf.sprintf "%s0x%x%x"
    (if n.msw >= 0 then "" else "-")
    q
    (r * 2 + a.lsb)
