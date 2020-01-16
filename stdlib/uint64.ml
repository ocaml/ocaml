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

type t = uint64

external of_int : int -> t = "caml_uint64_of_int"
external to_int : t -> int = "caml_uint64_to_int"
external add : t -> t -> t = "caml_uint64_add"
external sub : t -> t -> t = "caml_uint64_sub"
external mul : t -> t -> t = "caml_uint64_mul"
external div : t -> t -> t = "caml_uint64_div"
external logand : t -> t -> t = "caml_uint64_and"
external logor : t -> t -> t = "caml_uint64_or"
external logxor : t -> t -> t = "caml_uint64_xor"
external rem : t -> t -> t = "caml_uint64_rem"
external shift_left : t -> int -> t = "caml_uint64_shift_left"
external shift_right : t -> int -> t = "caml_uint64_shift_right"
external of_string : string -> t = "caml_uint64_of_string"
external of_int64 : int64 -> t = "caml_uint64_of_int64"
external to_int64 : t -> int64 = "caml_uint64_to_int64"
external of_nativeint : nativeint -> uint64 = "caml_uint64_of_nativeint"
external to_nativeint : uint64 -> nativeint = "caml_uint64_to_nativeint"
external of_uint32 : uint32 -> t = "caml_uint64_of_uint32"
external to_uint32 : t -> uint32 = "caml_uint64_to_uint32"
external of_int32 : int32 -> t = "caml_uint64_of_int32"
external to_int32 : t -> int32 = "caml_uint64_to_int32"
external of_float : float -> t = "caml_uint64_of_float"
external to_float : t -> float = "caml_uint64_to_float"
external format : string -> uint64 -> string = "caml_uint64_format"
let to_string n = format "%u" n
let zero = 0U
let one = 1U
let max_int = 0XFFFF_FFFF_FFFF_FFFF_U
let succ n = add n one
let pred n = sub n one
let lognot n = logxor n max_int
let compare (x : t) (y : t) = Stdlib.compare x y
let equal (x : t) (y : t) = Stdlib.(x = y)
let of_string_opt s =
  try Some (of_string s)
  with Failure _ -> None
