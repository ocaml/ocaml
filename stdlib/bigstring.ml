(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Nicolas Ojeda Bar, LexiFi                        *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Bigstring]: Large, one-dimensional byte arrays *)

type t = bigstring

external create : int -> t = "caml_ba_uint8_create"
external length : t -> int = "%caml_ba_dim_1"
external get : t -> int -> char = "%caml_ba_ref_1"
external set : t -> int -> char -> unit = "%caml_ba_set_1"
external unsafe_get : t -> int -> char = "%caml_ba_unsafe_ref_1"
external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"
external sub_nocopy : t -> int -> int -> t = "caml_ba_sub"
external fill : t -> char -> unit = "caml_ba_uint8_fill" [@@noalloc]
external unsafe_blit :
  t -> int -> t -> int -> int -> unit = "caml_ba_uint8_blit" [@@noalloc]
external unsafe_blit_from_bytes :
  bytes -> int -> t -> int -> int -> unit = "caml_ba_uint8_blit_from_bytes"
  [@@noalloc]
external unsafe_blit_to_bytes :
  t -> int -> bytes -> int -> int -> unit = "caml_ba_uint8_blit_to_bytes"
  [@@noalloc]

let init n f =
  if n < 0 then invalid_arg "Bitstring.init";
  let b = create n in
  for i = 0 to n - 1 do
    unsafe_set b i (f i)
  done;
  b

let blit b1 pos1 b2 pos2 len =
  if pos1 < 0 || pos2 < 0 || len < 0 ||
     pos1 > length b1 - len || pos2 > length b2 - len
  then invalid_arg "Bigstring.blit"
  else unsafe_blit b1 pos1 b2 pos2 len

let copy b =
  let sz = length b in
  let b' = create sz in
  unsafe_blit b 0 b' 0 sz;
  b'

let blit_bytes b1 pos1 b2 pos2 len =
  if pos1 < 0 || pos2 < 0 || len < 0 ||
     pos1 > Bytes.length b1 - len || pos2 > length b2 - len
  then invalid_arg "Bigstring.blit_bytes"
  else unsafe_blit_from_bytes b1 pos1 b2 pos2 len

let blit_string s pos1 b pos2 len =
  if pos1 < 0 || pos2 < 0 || len < 0 ||
     pos1 > String.length s - len || pos2 > length b - len
  then invalid_arg "Bigstring.blit_string"
  else unsafe_blit_from_bytes (Bytes.unsafe_of_string s) pos1 b pos2 len

let unsafe_sub_bytes b pos len =
  let b' = Bytes.create len in
  unsafe_blit_to_bytes b pos b' 0 len;
  b'

let sub b pos len =
  copy (sub_nocopy b pos len)

let sub_bytes b pos len =
  if len < 0 || pos < 0 || pos > length b - len
  then invalid_arg "Bigstring.sub_bytes"
  else unsafe_sub_bytes b pos len

let sub_string b pos len =
  if len < 0 || pos < 0 || pos > length b - len
  then invalid_arg "Bigstring.sub_string"
  else Bytes.unsafe_to_string (unsafe_sub_bytes b pos len)

let of_bytes b =
  let len = Bytes.length b in
  let b' = create len in
  unsafe_blit_from_bytes b 0 b' 0 len;
  b'

let to_bytes b =
  let len = length b in
  let b' = Bytes.create len in
  unsafe_blit_to_bytes b 0 b' 0 len;
  b'

let of_string s =
  of_bytes (Bytes.unsafe_of_string s)

let to_string b =
  Bytes.unsafe_to_string (to_bytes b)

let to_seq b =
  let rec loop sz i () =
    if i = sz then
      Seq.Nil
    else
      let x = unsafe_get b i in
      Seq.Cons (x, loop sz (succ i))
  in
  loop (length b) 0

external get_uint16_ne: t -> int -> int = "%caml_bigstring_get16"
external get_int32_ne: t -> int -> int32 = "%caml_bigstring_get32"
external get_int64_ne: t -> int -> int64 = "%caml_bigstring_get64"

external set_int16_ne: t -> int -> int -> unit = "%caml_bigstring_set16"
external set_int32_ne: t -> int -> int32 -> unit = "%caml_bigstring_set32"
external set_int64_ne: t -> int -> int64 -> unit = "%caml_bigstring_set64"

external swap16: int -> int = "%bswap16"
external swap32: int32 -> int32 = "%bswap_int32"
external swap64: int64 -> int64 = "%bswap_int64"

let get_uint8 b i =
  Char.code (get b i)

let get_int8 b i =
  ((get_uint8 b i) lsl (Sys.int_size - 8)) asr (Sys.int_size - 8)

let get_uint16_le b i =
  if Sys.big_endian then swap16 (get_uint16_ne b i)
  else get_uint16_ne b i

let get_uint16_be b i =
  if not Sys.big_endian then swap16 (get_uint16_ne b i)
  else get_uint16_ne b i

let get_int16_ne b i =
  ((get_uint16_ne b i) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int16_le b i =
  ((get_uint16_le b i) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int16_be b i =
  ((get_uint16_be b i) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int32_le b i =
  if Sys.big_endian then swap32 (get_int32_ne b i)
  else get_int32_ne b i

let get_int32_be b i =
  if not Sys.big_endian then swap32 (get_int32_ne b i)
  else get_int32_ne b i

let get_int64_le b i =
  if Sys.big_endian then swap64 (get_int64_ne b i)
  else get_int64_ne b i

let get_int64_be b i =
  if not Sys.big_endian then swap64 (get_int64_ne b i)
  else get_int64_ne b i

let set_int8 b i x =
  set b i (Char.unsafe_chr x)

let set_int16_le b i x =
  if Sys.big_endian then set_int16_ne b i (swap16 x)
  else set_int16_ne b i x

let set_int16_be b i x =
  if not Sys.big_endian then set_int16_ne b i (swap16 x)
  else set_int16_ne b i x

let set_int32_le b i x =
  if Sys.big_endian then set_int32_ne b i (swap32 x)
  else set_int32_ne b i x

let set_int32_be b i x =
  if not Sys.big_endian then set_int32_ne b i (swap32 x)
  else set_int32_ne b i x

let set_int64_le b i x =
  if Sys.big_endian then set_int64_ne b i (swap64 x)
  else set_int64_ne b i x

let set_int64_be b i x =
  if not Sys.big_endian then set_int64_ne b i (swap64 x)
  else set_int64_ne b i x

open Bigarray

external to_array1 : t -> (char, int8_unsigned_elt, c_layout) Array1.t = "%identity"
external of_genarray : ('a, 'b, 'c) Genarray.t -> t = "caml_ba_uint8_of_genarray"
let of_array1 a = of_genarray (genarray_of_array1 a)
let of_array2 a = of_genarray (genarray_of_array2 a)
let of_array3 a = of_genarray (genarray_of_array3 a)
