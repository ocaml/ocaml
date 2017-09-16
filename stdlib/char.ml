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

external string_unsafe_get : string -> int -> char = "%string_unsafe_get"

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

let lowercase c =
  if (c >= 'A' && c <= 'Z')
  || (c >= '\192' && c <= '\214')
  || (c >= '\216' && c <= '\222')
  then unsafe_chr(code c + 32)
  else c

let uppercase c =
  if (c >= 'a' && c <= 'z')
  || (c >= '\224' && c <= '\246')
  || (c >= '\248' && c <= '\254')
  then unsafe_chr(code c - 32)
  else c

let lowercase_ascii c =
  if (c >= 'A' && c <= 'Z')
  then unsafe_chr(code c + 32)
  else c

let uppercase_ascii c =
  if (c >= 'a' && c <= 'z')
  then unsafe_chr(code c - 32)
  else c

type t = char

let compare c1 c2 = code c1 - code c2
let equal (c1: t) (c2: t) = compare c1 c2 = 0

(* For bitvects: counting the number of bits of a character *)

(*
  let bitcount =
    let bytes = Bytes.create 256 in
    for c = 0 to 255 do
      let nbits = ref 0 in
      for b = 0 to 7 do
        if (1 lsl b) land c <> 0 then incr nbits
      done;
      bytes.[c] <- Char.chr !nbits
    done;
    Bytes.unsafe_to_string bytes
*)

let bitcount =
  "\000\001\001\002\001\002\002\003\001\002\002\003\002\003\003\004\
   \001\002\002\003\002\003\003\004\002\003\003\004\003\004\004\005\
   \001\002\002\003\002\003\003\004\002\003\003\004\003\004\004\005\
   \002\003\003\004\003\004\004\005\003\004\004\005\004\005\005\006\
   \001\002\002\003\002\003\003\004\002\003\003\004\003\004\004\005\
   \002\003\003\004\003\004\004\005\003\004\004\005\004\005\005\006\
   \002\003\003\004\003\004\004\005\003\004\004\005\004\005\005\006\
   \003\004\004\005\004\005\005\006\004\005\005\006\005\006\006\007\
   \001\002\002\003\002\003\003\004\002\003\003\004\003\004\004\005\
   \002\003\003\004\003\004\004\005\003\004\004\005\004\005\005\006\
   \002\003\003\004\003\004\004\005\003\004\004\005\004\005\005\006\
   \003\004\004\005\004\005\005\006\004\005\005\006\005\006\006\007\
   \002\003\003\004\003\004\004\005\003\004\004\005\004\005\005\006\
   \003\004\004\005\004\005\005\006\004\005\005\006\005\006\006\007\
   \003\004\004\005\004\005\005\006\004\005\005\006\005\006\006\007\
   \004\005\005\006\005\006\006\007\005\006\006\007\006\007\007\008"

(* Return the number of bits sets to 1 in the binary representation
   of a char. *)
let popcount c =
 code (string_unsafe_get bitcount (code c))
