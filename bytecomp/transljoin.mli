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
  This module gathers some definitions specific to jocaml
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

(* Partition an proc expression into simple, non_simple expressions *)
val as_procs :
  Typedtree.expression ->
  Typedtree.expression list * Typedtree.expression list

(* Building definitions and locations *)
val build_matches :
  Typedtree.joinautomaton ->
  Ident.t * int list array *
  (Location.t * Typedtree.pattern list * Typedtree.expression) array
val build_auto :
  Ident.t option ->
  Ident.t * int list array * 'a array -> Lambda.lambda -> Lambda.lambda
val build_channels :
  Typedtree.joinautomaton -> Lambda.lambda -> Lambda.lambda
val build_guards :
  ('a -> 'b -> 'c -> Lambda.lambda) ->
  Ident.t * 'd * ('a * 'b * 'c) array -> Lambda.lambda -> Lambda.lambda
