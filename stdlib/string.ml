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

let concat sep l =
  match l with
  | [] -> ""
  | hd :: tl ->
      let num = ref 0 and len = ref 0 in
      List.iter (fun s -> incr num; len := !len + length s) l;
      let r = B.create (!len + length sep * (!num - 1)) in
      unsafe_blit hd 0 r 0 (length hd);
      let pos = ref(length hd) in
      List.iter
        (fun s ->
          unsafe_blit sep 0 r !pos (length sep);
          pos := !pos + length sep;
          unsafe_blit s 0 r !pos (length s);
          pos := !pos + length s)
        tl;
      Bytes.unsafe_to_string r

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

let split sep s =
  let sep_max = length sep - 1 in
  if sep_max < 0 then invalid_arg "String.split: empty separator" else
  let s_max = length s - 1 in
  if s_max < 0 then [""] else
  let acc = ref [] in
  let sub_start = ref 0 in
  let k = ref 0 in 
  let i = ref 0 in 
  (* We build the substrings by running from the start of [s] to the
     end with [i] trying to match the first character of [sep] in
     [s]. If this matches, we verify that the whole [sep] is matched
     using [k]. If this matches we extract a substring from the start
     of the current substring [sub_start] to [!i - 1] (the position
     before the [sep] we found).  We then continue to try to match
     with [i] by starting after the [sep] we just found, this is also
     becomes the start position of the next substring. If [i] is such
     that no separator can be found we exit the loop and make a
     substring from [sub_start] until the end of the string. *)
  while (!i + sep_max <= s_max) do 
    if unsafe_get s !i <> unsafe_get sep 0 then incr i else 
    begin 
      (* Check remaining [sep] chars match, access to unsafe s (!i + !k) is
         guaranteed by loop invariant. *)
      k := 1; 
      while (!k <= sep_max && unsafe_get s (!i + !k) = unsafe_get sep !k) 
      do incr k done;
      if !k <= sep_max then (* no match *) incr i else begin 
        let new_sub_start = !i + sep_max + 1 in
        let sub_end = !i - 1 in 
        let sub_len = sub_end - !sub_start + 1 in 
        acc := sub s !sub_start sub_len :: !acc; 
        sub_start := new_sub_start; 
        i := new_sub_start;
      end
    end
  done;
  List.rev (sub s !sub_start (s_max - !sub_start + 1) :: !acc)

let rsplit sep s = 
  let sep_max = length sep - 1 in
  if sep_max < 0 then invalid_arg "String.rsplit: empty separator" else 
  let s_max = length s - 1 in
  if s_max < 0 then [""] else
  let acc = ref [] in
  let sub_end = ref s_max in
  let k = ref 0 in
  let i = ref s_max in
  (* We build the substrings by running from the end of [s] to the
     start with [i] trying to match the last character of [sep] in
     [s]. If this matches, we verify that the whole [sep] is matched
     using [k] (we do that backwards). If this matches we extract a
     substring from the end of the current substring [sub_end] to [!i
     + 1] (the position after the [sep] we found).  We then continue
     to try to match with [i] by starting before the [sep] we just
     found, this is also becomes the end position of the next
     substring. If [i] is such that no separator can be found we exit
     the loop and make a substring from the begining of the string
     until until [sub_end]. string. *)
  while (!i >= sep_max) do
    if unsafe_get s !i <> unsafe_get sep sep_max then decr i else begin
      (* Check remaining [sep] chars match, access to unsafe_get 
         s (sep_start + !k) is guaranteed by loop invariant. *)
      let sep_start = !i - sep_max in
      k := sep_max - 1;
      while (!k >= 0 && unsafe_get s (sep_start + !k) = unsafe_get sep !k)
      do decr k done;
      if !k >= 0 then (* no match *) decr i else begin 
        let new_sub_end = sep_start - 1 in
        let sub_start = !i + 1 in 
        let sub_len = !sub_end - sub_start + 1 in
        acc := sub s sub_start sub_len :: !acc; 
        sub_end := new_sub_end;
        i := new_sub_end;
      end
    end
  done;
  sub s 0 (!sub_end + 1) :: !acc

external is_printable: char -> bool = "caml_is_printable"

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
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> true
      | c when is_printable c -> needs_escape (i+1)
      | _ -> true
  in
  if needs_escape 0 then
    bts (B.escaped (bos s))
  else
    s

let index s c =
  B.index (bos s) c
let rindex s c =
  B.rindex (bos s) c
let index_from s i c=
  B.index_from (bos s) i c
let rindex_from s i c =
  B.rindex_from (bos s) i c
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
