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

(* Compilation of pattern-matching *)

open Typedtree
open Lambda

val for_function:
        Location.t -> lambda -> (pattern * lambda) list -> lambda
val for_trywith:
        Ident.t -> (pattern * lambda) list -> lambda
val for_let:
        Location.t -> Ident.t -> pattern -> lambda -> lambda
