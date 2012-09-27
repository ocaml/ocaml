(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*                Qin Ma, projet MOSCOVA, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(*
  Carry out the compilation from join definition with pattern matching
  of message contents to equivalent join definition without the requirement
  of pattern matching on message contents.

  cf. Compiling Pattern Matching in Join-Patterns
*)
open Typedtree

type 'a reaction = Location.t * joinpattern list * 'a

type dispatcher =
  Ident.t * (pattern * Ident.t) list * partial

type 'a guard =
  'a reaction  * (* old clause *)
  (joinpattern list list * (* new joinpattern *)
  (Ident.t * Typedtree.pattern) list) (* inserted matching *)

val compile :
    Location.t (* location of automaton *)->
    'a reaction  list (* clauses *) ->
      (dispatcher list * 'a guard list) * (* compiled clauses *)
      (Ident.t * Ident.t list) list  (* new channels *)
