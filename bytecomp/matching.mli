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

(* Compilation of pattern-matching *)

open Typedtree
open Lambda

val for_function:
        Location.t -> lambda -> (pattern * lambda) list -> lambda
val for_trywith:
        lambda -> (pattern * lambda) list -> lambda
val for_let:
        Location.t -> lambda -> pattern -> lambda -> lambda
val for_multiple_match:
        Location.t -> lambda list -> (pattern * lambda) list -> lambda
val for_tupled_function:
        Location.t -> Ident.t list -> (pattern list * lambda) list -> lambda

exception Cannot_flatten

val flatten_pattern: int -> pattern -> pattern list
