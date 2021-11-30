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
let err_not_sv i = format_int "%X" i ^ " is not an Unicode scalar value"
let err_not_latin1 u = "U+" ^ format_int "%04X" u ^ " is not a latin1 character"

type t = int

let min = 0x0000
let max = 0x10FFFF
let lo_bound = 0xD7FF
let hi_bound = 0xE000

let bom = 0xFEFF
let rep = 0xFFFD

let succ u =
  if u = lo_bound then hi_bound else
  if u = max then invalid_arg err_no_succ else
  u + 1

let pred u =
  if u = hi_bound then lo_bound else
  if u = min then invalid_arg err_no_pred else
  u - 1

let is_valid i = (min <= i && i <= lo_bound) || (hi_bound <= i && i <= max)
let of_int i = if is_valid i then i else invalid_arg (err_not_sv i)
external unsafe_of_int : int -> t = "%identity"
external to_int : t -> int = "%identity"

let is_char u = u < 256
let of_char c = Char.code c
let to_char u =
  if u > 255 then invalid_arg (err_not_latin1 u) else
  Char.unsafe_chr u

let unsafe_to_char = Char.unsafe_chr

let equal : int -> int -> bool = ( = )
let compare : int -> int -> int = Stdlib.compare
let hash = to_int

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
let[@inline] utf_decode_invalid n = (n lsl decode_bits) lor rep

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
