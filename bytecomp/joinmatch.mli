(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*                Qin Ma, projet MOSCOVA, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* 
   Types of reaction rules after compilation of pattern matching in join patterns
   cf. Compiling Pattern Matching in Join-Patterns
*)

type match_clause =
    Reaction of Typedtree.joinpattern list list * guard
  | Dispatcher of dispatcher

and guard =
    (Ident.t * Typedtree.pattern) list * Typedtree.expression

(* c, z, [pat_i * c_i] list means reaction

   c(z) = match z with
          ...
         | pat_i -> c_i(z)
         ...
*)
and dispatcher =
    Ident.t * Ident.t * (Typedtree.pattern * Ident.t) list * Typedtree.partial

