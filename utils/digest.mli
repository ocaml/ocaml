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
val string: string -> int -> int -> t
external channel: in_channel -> int -> t = "md5_chan"
val file: string -> t
val output: out_channel -> t -> unit
val input: in_channel -> t



