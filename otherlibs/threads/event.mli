(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*  David Nowak and Xavier Leroy, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type 'a channel

val new_channel: unit -> 'a channel

type 'a event

val send: 'a channel -> 'a -> unit event
val receive: 'a channel -> 'a event
val choose: 'a event list -> 'a event
val guard: (unit -> 'a event) -> 'a event
val wrap: 'a event -> ('a -> 'b) -> 'b event

val sync: 'a event -> 'a
val select: 'a event list -> 'a
