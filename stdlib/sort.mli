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

(* Module [Sort]: sorting and merging lists *)

val list : ('a -> 'a -> bool) -> 'a list -> 'a list
        (* Sort a list in increasing order according to an ordering predicate.
           The predicate should return [true] if its first argument is
           less than or equal to its second argument. *)

val merge : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
        (* Merge two lists according to the given predicate.
           Assuming the two argument lists are sorted according to the
           predicate, [merge] returns a sorted list containing the elements
           from the two lists. The behavior is undefined if the two
           argument lists were not sorted. *)
