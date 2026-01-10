(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = int

let zero = 0
let one = 1
let minus_one = -1
external neg : int -> int = "%negint"
external add : int -> int -> int = "%addint"
external sub : int -> int -> int = "%subint"
external mul : int -> int -> int = "%mulint"
external div : int -> int -> int = "%divint"
external rem : int -> int -> int = "%modint"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
let abs x = if x >= 0 then x else -x
let max_int = (-1) lsr 1
let min_int = max_int + 1
external logand : int -> int -> int = "%andint"
external logor : int -> int -> int = "%orint"
external logxor : int -> int -> int = "%xorint"
let lognot x = logxor x (-1)
external shift_left : int -> int -> int = "%lslint"
external shift_right : int -> int -> int = "%asrint"
external shift_right_logical : int -> int -> int = "%lsrint"
let equal : int -> int -> bool = ( = )
let compare : int -> int -> int = Stdlib.compare
let min x y : t = if x <= y then x else y
let max x y : t = if x >= y then x else y

(* Number of leading zeros.  Hacker's Delight (2 ed.), algorithm 5.12 *)

let leading_zeros x =
  let x = ref x and n = ref Sys.int_size in
  if Sys.int_size > 32 then begin
    let y = shift_right_logical !x 32 in
    if y <> 0 then (n := !n - 32; x := y)
  end;
  let y = shift_right_logical !x 16 in
  if y <> 0 then (n := !n - 16; x := y);
  let y = shift_right_logical !x  8 in
  if y <> 0 then (n := !n -  8; x := y);
  let y = shift_right_logical !x  4 in
  if y <> 0 then (n := !n -  4; x := y);
  let y = shift_right_logical !x 2 in
  if y <> 0 then (n := !n -  2; x := y);
  let y = shift_right_logical !x 1 in
  if y <> 0 then !n - 2 else !n - !x

let unsigned_bitsize x =
  Sys.int_size - leading_zeros x

(* Number of leading sign bits. *)

let leading_sign_bits x =
  if x >= 0 then leading_zeros x - 1 else leading_zeros (lognot x) - 1

let signed_bitsize x =
  Sys.int_size - leading_sign_bits x

(* Number of trailing zeros.  Hacker's Delight (2 ed.), algorithm 5.21 *)

let trailing_zeros x =
  if x = 0 then Sys.int_size else begin
    let x = ref x and n = ref (Sys.int_size - 1) in
    if Sys.int_size > 32 then begin
      let y = shift_left !x 32 in
      if y <> 0 then (n := !n - 32; x := y)
    end;
    let y = shift_left !x 16 in
    if y <> 0 then (n := !n - 16; x := y);
    let y = shift_left !x  8 in
    if y <> 0 then (n := !n -  8; x := y);
    let y = shift_left !x  4 in
    if y <> 0 then (n := !n -  4; x := y);
    let y = shift_left !x  2 in
    if y <> 0 then (n := !n -  2; x := y);
    let y = shift_left !x  1 in
    if y <> 0 then !n - 1 else !n
  end

(* Population count.  Hacker's Delight (2 ed.), algorithm 5.2 *)

external int64_to_int : int64 -> int = "%int64_to_int"
let cst1 = int64_to_int 0x5555_5555_5555_5555L
let cst2 = int64_to_int 0x3333_3333_3333_3333L
let cst3 = int64_to_int 0x0F0F_0F0F_0F0F_0F0FL

let popcount x =
  let x = sub x (logand (shift_right_logical x 1) cst1) in
  let x = add (logand x cst2)
              (logand (shift_right_logical x 2) cst2) in
  let x = logand (add x (shift_right_logical x 4)) cst3 in
  let x = add x (shift_right_logical x 8) in
  let x = add x (shift_right_logical x 16) in
  if Sys.int_size > 32 then begin
    let x = add x (shift_right_logical x 32) in
    x land 0x7F
  end else
    x land 0x3F

external to_float : int -> float = "%floatofint"
external of_float : float -> int = "%intoffloat"

(*
external int_of_string : string -> int = "caml_int_of_string"
let of_string s = try Some (int_of_string s) with Failure _ -> None
*)

external format_int : string -> int -> string = "caml_format_int"
let to_string x = format_int "%d" x

external seeded_hash_param :
  int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]
let seeded_hash seed x = seeded_hash_param 10 100 seed x
let hash x = seeded_hash_param 10 100 0 x

(* Floor division, ceil division *)

let fdiv n d =
  let q = div n d in
  if logxor n d >= 0 (* n and d have same sign *) || n = q * d
  then q else pred q

let cdiv n d =
  let q = div n d in
  if logxor n d < 0 (* n and d have different signs *) || n = q * d
  then q else succ q

(* Euclidean division and remainder *)

let erem n d =
  let r = rem n d in
  if r >= 0 then r else if d >= 0 then r + d else r - d

let ediv n d =
  let q = div n d in
  let r = n - q * d in
  if r >= 0 then q else if d >= 0 then pred q else succ q
