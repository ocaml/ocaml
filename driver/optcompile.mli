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

(* Compile a .ml or .mli file *)

val interface: string -> unit
val implementation: string -> unit
val c_file: string -> unit

val initial_env: unit -> Env.t
val init_path: unit -> unit

val pproc : string option ref
