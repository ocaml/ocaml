(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Lazy]: deferred computations. *)

type 'a status =
  | Delayed of (unit -> 'a)
  | Value of 'a
  | Exception of exn
;;

type 'a t = 'a status ref;;
(* A value of type ['a Lazy.t] is a deferred computation (also called a
   suspension) that computes a result of type ['a].  The expression
   [lazy (expr)] returns a suspension that computes [expr].
*)

val force: 'a t -> 'a;;
(* [Lazy.force x] computes the suspension [x] and returns its result.
   If the suspension was already computed, [Lazy.force x] returns the
   same value again.  If it raised an exception, the same exception is
   raised again.
*)
