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

(* Transformation of N-way integer branches *)

open Lambda

(* Input: a list of (key, action) pairs, where keys are integers. *)
(* Output: a table of (low, high, offset) triples for Ptranslate
           a list of actions for Lswitch *)

val make_decision_tree:
  (int * lambda) list -> (int * int * int) array * (int * lambda) list * int
