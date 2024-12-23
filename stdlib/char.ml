(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Character operations *)

external code: char -> int = "%identity"
external unsafe_chr: int -> char = "%identity"

let chr n =
  if n < 0 || n > 255 then invalid_arg "Char.chr" else unsafe_chr n

external bytes_create: int -> bytes = "caml_create_bytes"
external bytes_unsafe_set : bytes -> int -> char -> unit
                           = "%bytes_unsafe_set"
external unsafe_to_string : bytes -> string = "%bytes_to_string"

let escaped = function
  | '\'' -> "\\'"
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | '\r' -> "\\r"
  | '\b' -> "\\b"
  | ' ' .. '~' as c ->
      let s = bytes_create 1 in
      bytes_unsafe_set s 0 c;
      unsafe_to_string s
  | c ->
      let n = code c in
      let s = bytes_create 4 in
      bytes_unsafe_set s 0 '\\';
      bytes_unsafe_set s 1 (unsafe_chr (48 + n / 100));
      bytes_unsafe_set s 2 (unsafe_chr (48 + (n / 10) mod 10));
      bytes_unsafe_set s 3 (unsafe_chr (48 + n mod 10));
      unsafe_to_string s

let lowercase_ascii = function
  | 'A' .. 'Z' as c -> unsafe_chr(code c + 32)
  | c -> c

let uppercase_ascii = function
  | 'a' .. 'z' as c -> unsafe_chr(code c - 32)
  | c -> c

type t = char

let compare c1 c2 = code c1 - code c2
let equal (c1: t) (c2: t) = compare c1 c2 = 0

external seeded_hash_param :
  int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]
let seeded_hash seed x = seeded_hash_param 10 100 seed x
let hash x = seeded_hash_param 10 100 0 x

module Ascii = struct

  (* Characters *)

  let min = '\x00'
  let max = '\x7F'

  (* Predicates *)

  let is_valid = function '\x00' .. '\x7F' -> true | _ -> false
  let is_upper = function 'A' .. 'Z' -> true | _ -> false
  let is_lower = function 'a' .. 'z' -> true | _ -> false
  let is_letter = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false
  let is_alphanum = function
    | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false

  let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false
  let is_blank = function ' ' | '\t' -> true | _ -> false
  let is_graphic = function '!' .. '~' -> true | _ -> false
  let is_print = function ' ' .. '~' -> true | _ -> false
  let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false

  (* Decimal digits *)

  let is_digit = function '0' .. '9' -> true | _ -> false
  let digit_to_int = function
    | '0' .. '9' as c -> code c - 0x30
    | c -> invalid_arg (escaped c ^ ": not a decimal digit")

  let digit_of_int n = unsafe_chr (0x30 + abs (n mod 10))

  (* Hexadecimal digits *)

  let is_hex_digit = function
    | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
    | _ -> false

  let hex_digit_to_int = function
    | '0' .. '9' as c -> code c - 0x30
    | 'A' .. 'F' as c -> 10 + code c - 0x41
    | 'a' .. 'f' as c -> 10 + code c - 0x61
    | c -> invalid_arg (escaped c ^ ": not a hexadecimal digit")

  let lower_hex_digit_of_int n =
    let d = abs (n mod 16) in
    unsafe_chr (if d < 10 then 0x30 + d else 0x57 + d)

  let upper_hex_digit_of_int n =
    let d = abs (n mod 16) in
    unsafe_chr (if d < 10 then 0x30 + d else 0x37 + d)

  (* Casing transforms *)

  let lowercase = lowercase_ascii
  let uppercase = uppercase_ascii
end
