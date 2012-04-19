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

(* $Id$ *)

(* From lambda to assembly code *)

val compile_implementation :
    ?toplevel:(string -> bool) ->
    string -> Format.formatter -> int * Lambda.lambda -> unit
val compile_phrase :
    Format.formatter -> Cmm.phrase -> unit

type error = Assembler_error of string
exception Error of error
val report_error: Format.formatter -> error -> unit

type 'a hook = ('a -> 'a)

val add_typedtree_hook :
  (Typedtree.structure * Typedtree.module_coercion) hook -> unit
val add_lambda_hook : (int * Lambda.lambda) hook -> unit
val add_clambda_hook : Clambda.ulambda hook -> unit
val add_cmm_hook : Cmm.phrase list hook -> unit

val eval_typedtree_hooks :
  (Typedtree.structure * Typedtree.module_coercion) hook
val eval_lambda_hooks : (int * Lambda.lambda) hook
