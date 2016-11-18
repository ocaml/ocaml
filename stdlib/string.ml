(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Damien Doligez, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* String operations, based on byte sequence operations *)

external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
external set : bytes -> int -> char -> unit = "%bytes_safe_set"
external create : int -> bytes = "caml_create_bytes"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external unsafe_blit : string -> int ->  bytes -> int -> int -> unit
                     = "caml_blit_string" [@@noalloc]
external unsafe_fill : bytes -> int -> int -> char -> unit
                     = "caml_fill_bytes" [@@noalloc]

module B = Bytes

let bts = B.unsafe_to_string
let bos = B.unsafe_of_string

let make n c =
  B.make n c |> bts
let init n f =
  B.init n f |> bts
let copy s =
  B.copy (bos s) |> bts
let sub s ofs len =
  B.sub (bos s) ofs len |> bts
let fill =
  B.fill
let blit =
  B.blit_string

let ensure_ge x y = if x >= y then x else invalid_arg "String.concat"

let rec sum_lengths acc seplen = function
  | [] -> acc
  | hd :: [] -> length hd + acc
  | hd :: tl -> sum_lengths (ensure_ge (length hd + seplen + acc) acc) seplen tl

let rec unsafe_blits dst pos sep seplen = function
    [] -> dst
  | hd :: [] ->
    unsafe_blit hd 0 dst pos (length hd); dst
  | hd :: tl ->
    unsafe_blit hd 0 dst pos (length hd);
    unsafe_blit sep 0 dst (pos + length hd) seplen;
    unsafe_blits dst (pos + length hd + seplen) sep seplen tl

let concat sep = function
    [] -> ""
  | l -> let seplen = length sep in bts @@
          unsafe_blits
            (B.create (sum_lengths 0 seplen l))
            0 sep seplen l

let iter f s =
  B.iter f (bos s)
let iteri f s =
  B.iteri f (bos s)
let map f s =
  B.map f (bos s) |> bts
let mapi f s =
  B.mapi f (bos s) |> bts

(* Beware: we cannot use B.trim or B.escape because they always make a
   copy, but String.mli spells out some cases where we are not allowed
   to make a copy. *)

let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let trim s =
  if s = "" then s
  else if is_space (unsafe_get s 0) || is_space (unsafe_get s (length s - 1))
    then bts (B.trim (bos s))
  else s

let escaped s =
  let rec needs_escape i =
    if i >= length s then false else
      match unsafe_get s i with
      | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> true
      | ' ' .. '~' -> needs_escape (i+1)
      | _ -> true
  in
  if needs_escape 0 then
    bts (B.escaped (bos s))
  else
    s

let index s c =
  B.index (bos s) c
let index_opt s c =
  B.index_opt (bos s) c
let rindex s c =
  B.rindex (bos s) c
let rindex_opt s c =
  B.rindex_opt (bos s) c
let index_from s i c=
  B.index_from (bos s) i c
let index_from_opt s i c=
  B.index_from_opt (bos s) i c
let rindex_from s i c =
  B.rindex_from (bos s) i c
let rindex_from_opt s i c =
  B.rindex_from_opt (bos s) i c
let contains s c =
  B.contains (bos s) c
let contains_from s i c =
  B.contains_from (bos s) i c
let rcontains_from s i c =
  B.rcontains_from (bos s) i c

let uppercase_ascii s =
  B.uppercase_ascii (bos s) |> bts
let lowercase_ascii s =
  B.lowercase_ascii (bos s) |> bts
let capitalize_ascii s =
  B.capitalize_ascii (bos s) |> bts
let uncapitalize_ascii s =
  B.uncapitalize_ascii (bos s) |> bts

type t = string

let compare (x: t) (y: t) = Pervasives.compare x y
external equal : string -> string -> bool = "caml_string_equal"

let split_on_char sep s =
  let r = ref [] in
  let j = ref (length s) in
  for i = length s - 1 downto 0 do
    if unsafe_get s i = sep then begin
      r := sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  sub s 0 !j :: !r

(* Deprecated functions implemented via other deprecated functions *)
[@@@ocaml.warning "-3"]
let uppercase s =
  B.uppercase (bos s) |> bts
let lowercase s =
  B.lowercase (bos s) |> bts
let capitalize s =
  B.capitalize (bos s) |> bts
let uncapitalize s =
  B.uncapitalize (bos s) |> bts
