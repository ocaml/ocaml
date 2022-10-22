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

(* WARNING: Some functions in this file are duplicated in bytes.ml for
   efficiency reasons. When you modify the one in this file you need to
   modify its duplicate in bytes.ml.
   These functions have a "duplicated" comment above their definition.
*)

external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_blit : string -> int ->  bytes -> int -> int -> unit
                     = "caml_blit_string" [@@noalloc]

module B = Bytes

let bts = B.unsafe_to_string
let bos = B.unsafe_of_string

let make n c =
  B.make n c |> bts
let init n f =
  B.init n f |> bts
let empty = ""
let of_bytes = B.to_string
let to_bytes = B.of_string
let sub s ofs len =
  B.sub (bos s) ofs len |> bts
let blit =
  B.blit_string

let ensure_ge (x:int) y = if x >= y then x else invalid_arg "String.concat"

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

let cat = ( ^ )

(* duplicated in bytes.ml *)
let iter f s =
  for i = 0 to length s - 1 do f (unsafe_get s i) done

(* duplicated in bytes.ml *)
let iteri f s =
  for i = 0 to length s - 1 do f i (unsafe_get s i) done

let map f s =
  B.map f (bos s) |> bts
let mapi f s =
  B.mapi f (bos s) |> bts
let fold_right f x a =
  B.fold_right f (bos x) a
let fold_left f a x =
  B.fold_left f a (bos x)
let exists f s =
  B.exists f (bos s)
let for_all f s =
  B.for_all f (bos s)

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
  let b = bos s in
  (* We satisfy [unsafe_escape]'s precondition by passing an
     immutable byte sequence [b]. *)
  bts (B.unsafe_escape b)

(* duplicated in bytes.ml *)
let rec index_rec s lim i c =
  if i >= lim then raise Not_found else
  if unsafe_get s i = c then i else index_rec s lim (i + 1) c

(* duplicated in bytes.ml *)
let index s c = index_rec s (length s) 0 c

(* duplicated in bytes.ml *)
let rec index_rec_opt s lim i c =
  if i >= lim then None else
  if unsafe_get s i = c then Some i else index_rec_opt s lim (i + 1) c

(* duplicated in bytes.ml *)
let index_opt s c = index_rec_opt s (length s) 0 c

(* duplicated in bytes.ml *)
let index_from s i c =
  let l = length s in
  if i < 0 || i > l then invalid_arg "String.index_from / Bytes.index_from" else
    index_rec s l i c

(* duplicated in bytes.ml *)
let index_from_opt s i c =
  let l = length s in
  if i < 0 || i > l then
    invalid_arg "String.index_from_opt / Bytes.index_from_opt"
  else
    index_rec_opt s l i c

(* duplicated in bytes.ml *)
let rec rindex_rec s i c =
  if i < 0 then raise Not_found else
  if unsafe_get s i = c then i else rindex_rec s (i - 1) c

(* duplicated in bytes.ml *)
let rindex s c = rindex_rec s (length s - 1) c

(* duplicated in bytes.ml *)
let rindex_from s i c =
  if i < -1 || i >= length s then
    invalid_arg "String.rindex_from / Bytes.rindex_from"
  else
    rindex_rec s i c

(* duplicated in bytes.ml *)
let rec rindex_rec_opt s i c =
  if i < 0 then None else
  if unsafe_get s i = c then Some i else rindex_rec_opt s (i - 1) c

(* duplicated in bytes.ml *)
let rindex_opt s c = rindex_rec_opt s (length s - 1) c

(* duplicated in bytes.ml *)
let rindex_from_opt s i c =
  if i < -1 || i >= length s then
    invalid_arg "String.rindex_from_opt / Bytes.rindex_from_opt"
  else
    rindex_rec_opt s i c

(* duplicated in bytes.ml *)
let contains_from s i c =
  let l = length s in
  if i < 0 || i > l then
    invalid_arg "String.contains_from / Bytes.contains_from"
  else
    try ignore (index_rec s l i c); true with Not_found -> false

(* duplicated in bytes.ml *)
let contains s c = contains_from s 0 c

(* duplicated in bytes.ml *)
let rcontains_from s i c =
  if i < 0 || i >= length s then
    invalid_arg "String.rcontains_from / Bytes.rcontains_from"
  else
    try ignore (rindex_rec s i c); true with Not_found -> false

let uppercase_ascii s =
  B.uppercase_ascii (bos s) |> bts
let lowercase_ascii s =
  B.lowercase_ascii (bos s) |> bts
let capitalize_ascii s =
  B.capitalize_ascii (bos s) |> bts
let uncapitalize_ascii s =
  B.uncapitalize_ascii (bos s) |> bts

(* duplicated in bytes.ml *)
let starts_with ~prefix s =
  let len_s = length s
  and len_pre = length prefix in
  let rec aux i =
    if i = len_pre then true
    else if unsafe_get s i <> unsafe_get prefix i then false
    else aux (i + 1)
  in len_s >= len_pre && aux 0

(* duplicated in bytes.ml *)
let ends_with ~suffix s =
  let len_s = length s
  and len_suf = length suffix in
  let diff = len_s - len_suf in
  let rec aux i =
    if i = len_suf then true
    else if unsafe_get s (diff + i) <> unsafe_get suffix i then false
    else aux (i + 1)
  in diff >= 0 && aux 0

external seeded_hash : int -> string -> int = "caml_string_hash" [@@noalloc]
let hash x = seeded_hash 0 x

(* duplicated in bytes.ml *)
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

type t = string

let compare (x: t) (y: t) = Stdlib.compare x y
external equal : string -> string -> bool = "caml_string_equal" [@@noalloc]

(** {1 Iterators} *)

let to_seq s = bos s |> B.to_seq

let to_seqi s = bos s |> B.to_seqi

let of_seq g = B.of_seq g |> bts

(* UTF decoders and validators *)

let get_utf_8_uchar s i = B.get_utf_8_uchar (bos s) i
let is_valid_utf_8 s = B.is_valid_utf_8 (bos s)

let get_utf_16be_uchar s i = B.get_utf_16be_uchar (bos s) i
let is_valid_utf_16be s = B.is_valid_utf_16be (bos s)

let get_utf_16le_uchar s i = B.get_utf_16le_uchar (bos s) i
let is_valid_utf_16le s = B.is_valid_utf_16le (bos s)

(** {6 Binary encoding/decoding of integers} *)

external get_uint8 : string -> int -> int = "%string_safe_get"
external get_uint16_ne : string -> int -> int = "%caml_string_get16"
external get_int32_ne : string -> int -> int32 = "%caml_string_get32"
external get_int64_ne : string -> int -> int64 = "%caml_string_get64"

let get_int8 s i = B.get_int8 (bos s) i
let get_uint16_le s i = B.get_uint16_le (bos s) i
let get_uint16_be s i = B.get_uint16_be (bos s) i
let get_int16_ne s i = B.get_int16_ne (bos s) i
let get_int16_le s i = B.get_int16_le (bos s) i
let get_int16_be s i = B.get_int16_be (bos s) i
let get_int32_le s i = B.get_int32_le (bos s) i
let get_int32_be s i = B.get_int32_be (bos s) i
let get_int64_le s i = B.get_int64_le (bos s) i
let get_int64_be s i = B.get_int64_be (bos s) i
