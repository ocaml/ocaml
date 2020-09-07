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
external set : bytes -> int -> char -> unit = "%string_safe_set"
external create : int -> bytes = "caml_create_string"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit : string -> int ->  bytes -> int -> int -> unit
                     = "caml_blit_string" [@@noalloc]
external unsafe_fill : bytes -> int -> int -> char -> unit
                     = "caml_fill_string" [@@noalloc]

let make n c =
  Bytes.make n c |> Bytes.unsafe_to_string
let init n f =
  Bytes.init n f |> Bytes.unsafe_to_string

let copy s = Bytes.unsafe_to_string (Bytes.of_string s)

let sub s ofs len =
  if ofs < 0 || len < 0 || ofs > length s - len
  then invalid_arg "String.sub"
  else begin
    let r = create len in
    unsafe_blit s ofs r 0 len;
    Bytes.unsafe_to_string r
  end

let fill =
  Bytes.fill
let blit =
  Bytes.blit_string

let ensure_ge (x:int) y = if x >= y then x else invalid_arg "String.concat"

let rec sum_lengths acc seplen = function
  | [] -> acc
  | hd :: [] -> length hd + acc
  | hd :: tl -> sum_lengths (ensure_ge (length hd + seplen + acc) acc) seplen tl

let rec unsafe_blits dst pos sep seplen = function
    [] -> ()
  | hd :: [] ->
    unsafe_blit hd 0 dst pos (length hd)
  | hd :: tl ->
    unsafe_blit hd 0 dst pos (length hd);
    unsafe_blit sep 0 dst (pos + length hd) seplen;
    unsafe_blits dst (pos + length hd + seplen) sep seplen tl

let concat sep = function
    [] -> ""
  | l ->
      let seplen = length sep in
      let len = sum_lengths 0 seplen l in
      let bytes = create len in
      unsafe_blits bytes 0 sep seplen l;
      Bytes.unsafe_to_string bytes


(* duplicated in bytes.ml *)
let iter f s =
  for i = 0 to length s - 1 do f (unsafe_get s i) done

(* duplicated in bytes.ml *)
let iteri f s =
  for i = 0 to length s - 1 do f i (unsafe_get s i) done

(* duplicated in bytes.ml *)
let map f s =
  let l = length s in
  if l = 0 then s else begin
      let r = create l in
      for i = 0 to l - 1 do unsafe_set r i (f (unsafe_get s i)) done;
      Bytes.unsafe_to_string r
    end

(* duplicated in bytes.ml *)
let mapi f s =
  let l = length s in
  if l = 0 then s else begin
      let r = create l in
      for i = 0 to l - 1 do unsafe_set r i (f i (unsafe_get s i)) done;
      Bytes.unsafe_to_string r
    end

external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

(* duplicated in bytes.ml *)
let trim s =
  if s = "" then s
  else if is_space (unsafe_get s 0) || is_space (unsafe_get s (length s - 1))
  then
    let len = length s in
    let i = ref 0 in
    while !i < len && is_space (unsafe_get s !i) do
      incr i
    done;
    let j = ref (len - 1) in
    while !j >= !i && is_space (unsafe_get s !j) do
      decr j
    done;
    if !j >= !i then
      sub s !i (!j - !i + 1)
    else
      ""
  else s

(* duplicated in bytes.ml *)
let escaped s =
  let n = ref 0 in
  for i = 0 to length s - 1 do
    n := !n +
      (match unsafe_get s i with
       | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
       | ' ' .. '~' -> 1
       | _ -> 4)
  done;
  if !n = length s then s else begin
    let s' = create !n in
    n := 0;
    for i = 0 to length s - 1 do
      begin match unsafe_get s i with
      | ('\"' | '\\') as c ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
      | '\n' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
      | '\t' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 't'
      | '\r' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'r'
      | '\b' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'b'
      | (' ' .. '~') as c -> unsafe_set s' !n c
      | c ->
          let a = char_code c in
          unsafe_set s' !n '\\';
          incr n;
          unsafe_set s' !n (char_chr (48 + a / 100));
          incr n;
          unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
          incr n;
          unsafe_set s' !n (char_chr (48 + a mod 10));
      end;
      incr n
    done;
    Bytes.unsafe_to_string s'
  end

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
  if i < 0 || i > l then invalid_arg "String.index_from" else
    index_rec s l i c

(* duplicated in bytes.ml *)
let index_from_opt s i c =
  let l = length s in
  if i < 0 || i > l then
    invalid_arg "String.index_from_opt"
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
    invalid_arg "String.rindex_from"
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
    invalid_arg "String.rindex_from_opt"
  else
    rindex_rec_opt s i c

(* duplicated in bytes.ml *)
let contains_from s i c =
  let l = length s in
  if i < 0 || i > l then
    invalid_arg "String.contains_from"
  else
    try ignore (index_rec s l i c); true with Not_found -> false

(* duplicated in bytes.ml *)
let contains s c = contains_from s 0 c

(* duplicated in bytes.ml *)
let rcontains_from s i c =
  if i < 0 || i >= length s then
    invalid_arg "String.rcontains_from"
  else
    try ignore (rindex_rec s i c); true with Not_found -> false

(* duplicated in bytes.ml *)
let uppercase_ascii s = map Char.uppercase_ascii s
let lowercase_ascii s = map Char.lowercase_ascii s

(* duplicated in bytes.ml *)
let apply1 f s =
  if length s = 0 then s else begin
    let r = Bytes.of_string s in
    unsafe_set r 0 (f(unsafe_get s 0));
    Bytes.unsafe_to_string r
  end

(* duplicated in bytes.ml *)
let capitalize_ascii s = apply1 Char.uppercase_ascii s
let uncapitalize_ascii s = apply1 Char.lowercase_ascii s

type t = string

let compare (x: t) (y: t) = Stdlib.compare x y
external equal : string -> string -> bool = "caml_string_equal" [@@noalloc]

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

include
  struct
    (* Deprecated functions implemented via other deprecated functions *)
    [@@@ocaml.warning "-3"]
    let uppercase s = map Char.uppercase s
    let lowercase s = map Char.lowercase s

    let capitalize s = apply1 Char.uppercase s
    let uncapitalize s = apply1 Char.lowercase s
  end

(** {1 Iterators} *)

  let to_seq s =
  let rec aux i () =
    if i = length s then Seq.Nil
    else
      let x = get s i in
      Seq.Cons (x, aux (i+1))
  in
  aux 0

let to_seqi s =
  let rec aux i () =
    if i = length s then Seq.Nil
    else
      let x = get s i in
      Seq.Cons ((i,x), aux (i+1))
  in
  aux 0

let of_seq s = Bytes.of_seq s |> Bytes.unsafe_to_string
