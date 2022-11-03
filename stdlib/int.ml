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
