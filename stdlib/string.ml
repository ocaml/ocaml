(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Damien Doligez, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* String operations, based on byte sequence operations *)

external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
external set : bytes -> int -> char -> unit = "%string_safe_set"
external create : int -> bytes = "caml_create_string"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit : string -> int ->  bytes -> int -> int -> unit
                     = "caml_blit_string" "noalloc"
external unsafe_fill : bytes -> int -> int -> char -> unit
                     = "caml_fill_string" "noalloc"

module B = Bytes

let make = (Obj.magic B.make : int -> char -> string)
let init = (Obj.magic B.init : int -> (int -> char) -> string)
let copy = (Obj.magic B.copy : string -> string)
let sub = (Obj.magic B.sub : string -> int -> int -> string)
let fill = B.fill
let blit =
  (Obj.magic B.blit : string -> int -> bytes -> int -> int -> unit)
let concat = (Obj.magic B.concat : string -> string list -> string)
let iter = (Obj.magic B.iter : (char -> unit) -> string -> unit)
let iteri = (Obj.magic B.iteri : (int -> char -> unit) -> string -> unit)
let map = (Obj.magic B.map : (char -> char) -> string -> string)

(* Beware: we cannot use B.trim or B.escape because they always make a
   copy, but String.mli spells out some cases where we are not allowed
   to make a copy. *)

external is_printable: char -> bool = "caml_is_printable"

let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let trim s =
  if s = "" then s
  else if is_space (unsafe_get s 0) || is_space (unsafe_get s (length s - 1))
    then B.unsafe_to_string (B.trim (B.unsafe_of_string s))
  else s

let escaped s =
  let rec needs_escape i =
    if i >= length s then false else
      match unsafe_get s i with
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> true
      | c when is_printable c -> needs_escape (i+1)
      | _ -> true
  in
  if needs_escape 0 then
    B.unsafe_to_string (B.escaped (B.unsafe_of_string s))
  else
    s

let index = (Obj.magic B.index : string -> char -> int)
let rindex = (Obj.magic B.rindex : string -> char -> int)
let index_from = (Obj.magic B.index_from : string -> int -> char -> int)
let rindex_from = (Obj.magic B.rindex_from : string -> int -> char -> int)
let contains = (Obj.magic B.contains : string -> char -> bool)
let contains_from = (Obj.magic B.contains_from : string -> int -> char -> bool)
let rcontains_from =
  (Obj.magic B.rcontains_from : string -> int -> char -> bool)
let uppercase = (Obj.magic B.uppercase : string -> string)
let lowercase = (Obj.magic B.lowercase : string -> string)
let capitalize = (Obj.magic B.capitalize : string -> string)
let uncapitalize = (Obj.magic B.uncapitalize : string -> string)

type t = string

let compare (x: t) (y: t) = Pervasives.compare x y
external equal : string -> string -> bool = "caml_string_equal"
