(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                           Daniel C. Buenzli                            *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external format_int : string -> int -> string = "caml_format_int"

let err_no_pred = "U+0000 has no predecessor"
let err_no_succ = "U+10FFFF has no successor"
let err_not_sv i = format_int "%X" i ^ " is not a Unicode scalar value"
let err_not_latin1 u = "U+" ^ format_int "%04X" u ^ " is not a latin1 character"

type t = uchar
external unsafe_of_int : int -> t = "%identity"
external to_int : t -> int = "%identity"

let min = unsafe_of_int 0x0000
let max = unsafe_of_int 0x10FFFF
let lo_bound = unsafe_of_int 0xD7FF
let hi_bound = unsafe_of_int 0xE000

let bom = unsafe_of_int 0xFEFF
let rep = unsafe_of_int 0xFFFD

let succ u =
  if u = lo_bound then hi_bound else
  if u = max then invalid_arg err_no_succ else
  unsafe_of_int (to_int u + 1)

let pred u =
  if u = hi_bound then lo_bound else
  if u = min then invalid_arg err_no_pred else
  unsafe_of_int (to_int u - 1)

let is_valid i =
  let i = unsafe_of_int i in
  (min <= i && i <= lo_bound) || (hi_bound <= i && i <= max)
let of_int i =
  if is_valid i then unsafe_of_int i
  else invalid_arg (err_not_sv i)

let is_char u = to_int u < 256
let of_char c = unsafe_of_int (Char.code c)
let to_char u =
  let u = to_int u in
  if u > 255 then invalid_arg (err_not_latin1 u) else
  Char.unsafe_chr u

let unsafe_to_char x = Char.unsafe_chr (to_int x)

let equal : t -> t -> bool = ( = )
let compare : t -> t -> int = Stdlib.compare

external seeded_hash_param :
  int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]
let seeded_hash seed x = seeded_hash_param 10 100 seed x
let hash x = seeded_hash_param 10 100 0 x

(* UTF codecs tools *)

type utf_decode = int
(* This is an int [0xDUUUUUU] decomposed as follows:
   - [D] is four bits for decode information, the highest bit is set if the
     decode is valid. The three lower bits indicate the number of elements
     from the source that were consumed by the decode.
   - [UUUUUU] is the decoded Unicode character or the Unicode replacement
     character U+FFFD if for invalid decodes. *)

let valid_bit = 27
let decode_bits = 24

let[@inline] utf_decode_is_valid d = (d lsr valid_bit) = 1
let[@inline] utf_decode_length d = (d lsr decode_bits) land 0b111
let[@inline] utf_decode_uchar d = unsafe_of_int (d land 0xFFFFFF)
let[@inline] utf_decode n u = ((8 lor n) lsl decode_bits) lor (to_int u)
let[@inline] utf_decode_invalid n = (n lsl decode_bits) lor to_int rep

let utf_8_decode_length_of_byte = function
  | '\x00' .. '\x7F' -> 1
  | '\x80' .. '\xC1' -> 0
  | '\xC2' .. '\xDF' -> 2
  | '\xE0' .. '\xEF' -> 3
  | '\xF0' .. '\xF4' -> 4
  | _ -> 0

let max_utf_8_decode_length = 4

let utf_8_byte_length u = match to_int u with
| u when u < 0 -> assert false
| u when u <= 0x007F -> 1
| u when u <= 0x07FF -> 2
| u when u <= 0xFFFF -> 3
| u when u <= 0x10FFFF -> 4
| _ -> assert false

let utf_16_byte_length u = match to_int u with
| u when u < 0 -> assert false
| u when u <= 0xFFFF -> 2
| u when u <= 0x10FFFF -> 4
| _ -> assert false
