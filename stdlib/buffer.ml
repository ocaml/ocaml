(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*   Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt  *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* Extensible buffers *)

type t =
 {mutable buffer : string;
  mutable position : int;
  mutable length : int;
  initial_buffer : string}

let create n =
 let n = if n < 1 then 1 else n in
 let n = if n > Sys.max_string_length then Sys.max_string_length else n in
 let s = String.create n in
 {buffer = s; position = 0; length = n; initial_buffer = s}

let contents b = String.sub b.buffer 0 b.position

let sub b ofs len =
  if ofs < 0 || len < 0 || ofs > b.position - len
  then invalid_arg "Buffer.sub"
  else begin
    let r = String.create len in
    String.unsafe_blit b.buffer ofs r 0 len;
    r
  end
;;

let blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || srcoff > src.position - len
             || dstoff < 0 || dstoff > (String.length dst) - len
  then invalid_arg "Buffer.blit"
  else
    String.blit src.buffer srcoff dst dstoff len
;;

let nth b ofs =
  if ofs < 0 || ofs >= b.position then
   invalid_arg "Buffer.nth"
  else String.unsafe_get b.buffer ofs
;;

let length b = b.position

let clear b = b.position <- 0

let reset b =
  b.position <- 0; b.buffer <- b.initial_buffer;
  b.length <- String.length b.buffer

let resize b more =
  let len = b.length in
  let new_len = ref len in
  while b.position + more > !new_len do new_len := 2 * !new_len done;
  if !new_len > Sys.max_string_length then begin
    if b.position + more <= Sys.max_string_length
    then new_len := Sys.max_string_length
    else failwith "Buffer.add: cannot grow buffer"
  end;
  let new_buffer = String.create !new_len in
  String.blit b.buffer 0 new_buffer 0 b.position;
  b.buffer <- new_buffer;
  b.length <- !new_len

let add_char b c =
  let pos = b.position in
  if pos >= b.length then resize b 1;
  String.unsafe_set b.buffer pos c;
  b.position <- pos + 1

let add_substring b s offset len =
  if offset < 0 || len < 0 || offset > String.length s - len
  then invalid_arg "Buffer.add_substring";
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  String.unsafe_blit s offset b.buffer b.position len;
  b.position <- new_position

let add_string b s =
  let len = String.length s in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  String.unsafe_blit s 0 b.buffer b.position len;
  b.position <- new_position

let add_buffer b bs =
  add_substring b bs.buffer 0 bs.position

let add_channel b ic len =
  if len < 0 || len > Sys.max_string_length then   (* PR#5004 *)
    invalid_arg "Buffer.add_channel";
  if b.position + len > b.length then resize b len;
  really_input ic b.buffer b.position len;
  b.position <- b.position + len

let output_buffer oc b =
  output oc b.buffer 0 b.position

let closing = function
  | '(' -> ')'
  | '{' -> '}'
  | _ -> assert false;;

(* opening and closing: open and close characters, typically ( and )
   k: balance of opening and closing chars
   s: the string where we are searching
   start: the index where we start the search. *)
let advance_to_closing opening closing k s start =
  let rec advance k i lim =
    if i >= lim then raise Not_found else
    if s.[i] = opening then advance (k + 1) (i + 1) lim else
    if s.[i] = closing then
      if k = 0 then i else advance (k - 1) (i + 1) lim
    else advance k (i + 1) lim in
  advance k start (String.length s);;

let advance_to_non_alpha s start =
  let rec advance i lim =
    if i >= lim then lim else
    match s.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> advance (i + 1) lim
    | _ -> i in
  advance start (String.length s);;

(* We are just at the beginning of an ident in s, starting at start. *)
let find_ident s start lim =
  if start >= lim then raise Not_found else
  match s.[start] with
  (* Parenthesized ident ? *)
  | '(' | '{' as c ->
     let new_start = start + 1 in
     let stop = advance_to_closing c (closing c) 0 s new_start in
     String.sub s new_start (stop - start - 1), stop + 1
  (* Regular ident *)
  | _ ->
     let stop = advance_to_non_alpha s (start + 1) in
     String.sub s start (stop - start), stop;;

(* Substitute $ident, $(ident), or ${ident} in s,
    according to the function mapping f. *)
let add_substitute b f s =
  let lim = String.length s in
  let rec subst previous i =
    if i < lim then begin
      match s.[i] with
      | '$' as current when previous = '\\' ->
         add_char b current;
         subst ' ' (i + 1)
      | '$' ->
         let j = i + 1 in
         let ident, next_i = find_ident s j lim in
         add_string b (f ident);
         subst ' ' next_i
      | current when previous == '\\' ->
         add_char b '\\';
         add_char b current;
         subst ' ' (i + 1)
      | '\\' as current ->
         subst current (i + 1)
      | current ->
         add_char b current;
         subst current (i + 1)
    end else
    if previous = '\\' then add_char b previous in
  subst ' ' 0;;
