(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type t
external new: unit -> t = "csl_condition_new"
external wait: t -> Mutex.t -> unit = "csl_condition_wait"
external signal: t -> unit = "csl_condition_signal"
external broadcast: t -> unit = "csl_condition_broadcast"
