(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Message digest (MD5) *)

type t = string

external unsafe_string: string -> int -> int -> t = "md5_string"
external channel: in_channel -> int -> t = "md5_chan"

let string str ofs len =
  if ofs < 0 or ofs + len > String.length str
  then invalid_arg "Digest.string"
  else unsafe_string str ofs len

let file filename =
  let ic = open_in_bin filename in
  let d = channel ic (in_channel_length ic) in
  close_in ic;
  d

let output chan digest =
  output chan digest 0 16

let input chan =
  let digest = String.create 16 in
  really_input chan digest 0 16;
  digest
