(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(*
  From typedtree to lambda code, for some jocaml constructs
*)

(* Basic join operations from the module Jprims *)
val exit : unit -> Lambda.lambda
val create_location : unit -> Lambda.lambda
val create_process : Lambda.lambda -> Lambda.lambda
val create_process_location : Ident.t -> Lambda.lambda -> Lambda.lambda
val send_async : Ident.t -> int -> Lambda.lambda -> Lambda.lambda
val send_sync : Ident.t -> int -> Lambda.lambda -> Lambda.lambda
val create_automaton : Ident.t option -> int -> int -> Lambda.lambda
val patch_match : Ident.t -> int -> int list -> Lambda.lambda
val patch_guard : Ident.t -> int -> Lambda.lambda -> Lambda.lambda
val reply_to : Lambda.lambda -> Lambda.lambda -> Lambda.lambda
val do_spawn : Ident.t option -> Lambda.lambda -> Lambda.lambda

(* Is an expression simple enough (no exception, guaranteed to terminate) ? *)
val simple_exp : Typedtree.expression -> bool

(* Partition a proc expression into
   principal thread, simple, non_simple expressions *)
val as_procs :
  Ident.t option ->
  Typedtree.expression ->
  Typedtree.expression option * Typedtree.expression list * Typedtree.expression list

(* Building definitions and locations *)
val build_matches :
  Typedtree.joinautomaton ->
  Ident.t * int list array *
  (Location.t * Ident.t option * Typedtree.pattern list * Typedtree.expression) array
val build_auto :
  Ident.t option ->
  Ident.t * int list array * 'a array -> Lambda.lambda -> Lambda.lambda
val build_channels :
  Typedtree.joinautomaton -> Lambda.lambda -> Lambda.lambda
val build_guards :
  ('a -> 'b -> 'c -> 'd -> Lambda.lambda) ->
  Ident.t * 'e * ('a * 'b * 'c * 'd) array -> Lambda.lambda -> Lambda.lambda
