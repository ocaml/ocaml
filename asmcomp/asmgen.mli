(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* From lambda to assembly code *)

val compile_implementation :
    ?toplevel:(string -> bool) ->
    sourcefile:string ->
    string ->
    backend:(module Backend_intf.S) ->
    Format.formatter -> Flambda.program -> unit
val compile_phrase :
    Format.formatter -> Cmm.phrase -> unit

type error = Assembler_error of string
exception Error of error
val report_error: Format.formatter -> error -> unit


val compile_unit:
  sourcefile:string ->
  string(*prefixname*) ->
  string(*asm file*) -> bool(*keep asm*) ->
  string(*obj file*) -> (unit -> unit) -> unit
