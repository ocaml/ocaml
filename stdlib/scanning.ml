(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The run-time library for scanf. *)

type scanbuf = {
  mutable eof : bool;
  mutable cur_char : char;
  mutable char_count : int;
  mutable token_count : int;
  mutable get_next_char : unit -> char;
  tokbuf : Buffer.t;
};;

let next_char ib =
  try
   ib.cur_char <- ib.get_next_char ();
   ib.char_count <- ib.char_count + 1
  with End_of_file ->
   ib.cur_char <- '\000';
   ib.eof <- true;;

let peek_char ib = ib.cur_char;;
let end_of_input ib = ib.eof && (ib.eof <- false; next_char ib; ib.eof);;
let char_count ib = ib.char_count;;
let reset_token ib = Buffer.reset ib.tokbuf;;

let token ib =
  let tokbuf = ib.tokbuf in
  let tok = Buffer.contents tokbuf in
  Buffer.clear tokbuf;
  ib.token_count <- 1 + ib.token_count;
  tok;;

let token_count ib = ib.token_count;;

let store_char ib c max =
  Buffer.add_char ib.tokbuf c;
  next_char ib;
  max - 1;;

let create next =
  let ib = {
    eof = true;
    cur_char = '\000';
    char_count = 0;
    get_next_char = next;
    tokbuf = Buffer.create 10;
    token_count = 0;
    } in
  ib;;

let from_string s =
  let i = ref 0 in
  let len = String.length s in
  let next () =
    if !i >= len then raise End_of_file else
    let c = s.[!i] in
    incr i;
    c in
  create next;;

let from_channel ic =
  let next () = input_char ic in
  create next;;

let from_function f = create f;;

