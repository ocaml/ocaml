(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
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

let length b = b.position

let clear b = b.position <- 0

let reset b =
  b.position <- 0; b.buffer <- b.initial_buffer;
  b.length <- String.length b.buffer

let resize b more =
  let len = b.length in
  let new_len = ref len in
  while b.position + more > !new_len do new_len := 2 * !new_len done;
  let new_buffer = String.create !new_len in
  String.blit b.buffer 0 new_buffer 0 b.position;
  b.buffer <- new_buffer;
  b.length <- !new_len

let add_char b c =
  let pos = b.position in
  if pos >= b.length then resize b 1;
  b.buffer.[pos] <- c;
  b.position <- pos + 1

let add_substring b s offset len =
  if offset < 0 || len < 0 || offset + len > String.length s
  then invalid_arg "Buffer.add_substring";
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  String.blit s offset b.buffer b.position len;
  b.position <- new_position

let add_string b s =
  let len = String.length s in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  String.blit s 0 b.buffer b.position len;
  b.position <- new_position
  
let add_buffer b bs =
  add_substring b bs.buffer 0 bs.position

let add_channel b ic len =
  if b.position + len > b.length then resize b len;
  really_input ic b.buffer b.position len;
  b.position <- b.position + len

let output_buffer oc b =
  output oc b.buffer 0 b.position
