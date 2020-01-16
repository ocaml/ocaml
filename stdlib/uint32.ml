(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                             Jeremy Yallop                              *)
(*                                                                        *)
(*   Copyright 2020 Jeremy Yallop                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = uint32

external of_int : int -> t = "caml_uint32_of_int"
external to_int : t -> int = "caml_uint32_to_int"
external add : t -> t -> t = "caml_uint32_add"
external sub : t -> t -> t = "caml_uint32_sub"
external mul : t -> t -> t = "caml_uint32_mul"
external div : t -> t -> t = "caml_uint32_div"
external logand : t -> t -> t = "caml_uint32_and"
external logor : t -> t -> t = "caml_uint32_or"
external logxor : t -> t -> t = "caml_uint32_xor"
external rem : t -> t -> t = "caml_uint32_mod"
external shift_left : t -> int -> t = "caml_uint32_shift_left"
external shift_right : t -> int -> t = "caml_uint32_shift_right"
external of_string : string -> t = "caml_uint32_of_string"
external of_int32 : int32 -> t = "caml_uint32_of_int32"
external to_int32 : t -> int32 = "caml_uint32_to_int32"
external of_float : float -> uint32 = "caml_uint32_of_float"
external to_float : uint32 -> float = "caml_uint32_to_float"
external format : string -> uint32 -> string = "caml_uint32_format"
let to_string n = format "%u" n
let zero = 0u
let one = 1u
let max_int = 0xffffffffu
let succ n = add n one
let pred n = sub n one
let lognot n = logxor n max_int
let compare (x : t) (y : t) = Stdlib.compare x y
let equal (x : t) (y : t) = Stdlib.(x = y)
let of_string_opt s =
  try Some (of_string s)
  with Failure _ -> None
