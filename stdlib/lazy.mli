(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Lazy]: deferred computations. *)

type 'a delayed;;
(* A value of type ['a delayed] is a deferred computation (called a
   suspension) that computes a result of type ['a].
*)

val _lazy : (unit -> 'a) -> 'a delayed;;
(* [_lazy f] is the suspension that computes [f ()].  The expression
   [lazy (expr)] is equivalent to [_lazy (fun () -> expr)].
*)

val force: 'a delayed -> 'a;;
(* [force x] computes the suspension [x] and returns its result.  If
   the suspension was already computed, [force x] returns the same
   value again.  If the suspension was already computed and raised an
   exception, the same exception is raised again.
*)
