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

(* From lambda to assembly code *)

val compile_implementation: string -> int -> Lambda.lambda -> unit
val compile_phrase: Cmm.phrase -> unit

type error = Assembler_error of string
exception Error of error
val report_error: error -> unit


