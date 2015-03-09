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

let cut sep s =
  let sep_max = length sep - 1 in
  if sep_max < 0 then invalid_arg "String.cut: empty separator" else
  let s_max = length s - 1 in
  if s_max < 0 then None else
  let k = ref 0 in
  let i = ref 0 in 
  (* We run from the start of [s] to end with [i] trying to match the
     first character of [sep] in [s]. If this matches, we verify that
     the whole [sep] is matched using [k]. If it doesn't match we
     continue to look for [sep] with [i]. If it matches we exit the
     loop and extract a substring from the start of [s] to the
     position before the [sep] we found and another from the position
     after the [sep] we found to end of string. If [i] is such that no
     separator can be found we exit the loop and return the no match
     case. *)
  try 
    while (!i + sep_max <= s_max) do
      (* Check remaining [sep] chars match, access to unsafe s (!i + !k) is
           guaranteed by loop invariant. *)
      if unsafe_get s !i <> unsafe_get sep 0 then incr i else begin
        k := 1; 
        while (!k <= sep_max && unsafe_get s (!i + !k) = unsafe_get sep !k)
        do incr k done;
        if !k <= sep_max then (* no match *) incr i else raise Exit
      end
    done; 
    None (* no match in the whole string. *)
  with
  | Exit -> (* i is at the beginning of the separator *) 
      let left_end = !i - 1 in 
      let right_start = !i + sep_max + 1 in
      Some (sub s 0 (left_end + 1), 
            sub s right_start (s_max - right_start + 1))
        
let rcut sep s =
  let sep_max = length sep - 1 in
  if sep_max < 0 then invalid_arg "String.rcut: empty separator" else
  let s_max = length s - 1 in
  if s_max < 0 then None else
  let k = ref 0 in
  let i = ref s_max in 
  (* We run from the end of [s] to the beginning with [i] trying to
     match the last character of [sep] in [s]. If this matches, we
     verify that the whole [sep] is matched using [k] (we do that
     backwards).  If it doesn't match we continue to look for [sep]
     with [i].  If it matches we exit the loop and extract a
     substring from the start of [s] to the position before the
     [sep] we found and another from the position after the [sep] we
     found to end of string.  If [i] is such that no separator can
     be found we exit the loop and return the no match case. *)
  try 
    while (!i >= sep_max) do
      if unsafe_get s !i <> unsafe_get sep sep_max then decr i else begin 
        (* Check remaining [sep] chars match, access to unsafe_get 
             s (sep_start + !k) is guaranteed by loop invariant. *)
        let sep_start = !i - sep_max in
        k := sep_max - 1;
        while (!k >= 0 && unsafe_get s (sep_start + !k) = unsafe_get sep !k)
        do decr k done;
          if !k >= 0 then (* no match *) decr i else raise Exit
      end
    done; 
    None (* no match in the whole string. *)
  with
  | Exit -> (* i is at the end of the separator *) 
      let left_end = !i - sep_max - 1 in 
      let right_start = !i + 1 in
      Some (sub s 0 (left_end + 1), 
            sub s right_start (s_max - right_start + 1))

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
