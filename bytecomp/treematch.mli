(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Compilation of pattern-matching *)

open Typedtree
open Lambda

val compile_matching:
    Location.t -> int ref option -> (unit -> Lambda.lambda) ->
      Lambda.lambda -> (Typedtree.pattern * Lambda.lambda) list	->
	partial -> Lambda.lambda

val for_multiple_match:
    Location.t -> lambda list -> (pattern * lambda) list -> partial ->
        lambda

val for_tupled_function:
    Location.t -> Ident.t list -> (pattern list * lambda) list ->
        partial -> lambda

