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

open Terms

type ordering = 
    Greater
  | Equal
  | NotGE

val ge_ord: ('a -> ordering) -> 'a -> bool
val gt_ord: ('a -> ordering) -> 'a -> bool
val eq_ord: ('a -> ordering) -> 'a -> bool
val rem_eq: ('a * 'b -> bool) -> 'a -> 'b list -> 'b list
val diff_eq: ('a * 'a -> bool) -> 'a list * 'a list -> 'a list * 'a list
val mult_ext: (term * term -> ordering) -> term * term -> ordering
val lex_ext: (term * term -> ordering) -> term * term -> ordering
val rpo: (string -> string -> ordering) ->
         ((term * term -> ordering) -> term * term -> ordering) ->
         term * term -> ordering
