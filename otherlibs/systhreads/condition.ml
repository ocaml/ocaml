(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type t
external create: unit -> t = "caml_condition_new"
external wait: t -> Mutex.t -> unit = "caml_condition_wait"
external signal: t -> unit = "caml_condition_signal"
external broadcast: t -> unit = "caml_condition_broadcast"
