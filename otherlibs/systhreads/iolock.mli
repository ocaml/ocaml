(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Locking on I/O channels. *)

type mutex

val add : 'a -> 'a
val remove : 'a -> unit
val find : 'a -> mutex
val lock : mutex -> unit
val unlock : mutex -> unit

