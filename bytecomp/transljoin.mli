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

(**********)
(* Errors *)
(**********)
type error
exception Error of error
val report_error : Format.formatter -> error -> unit


(**********************************************)
(* Basic join operations from the module Join *)
(**********************************************)

val exit : unit -> Lambda.lambda
val create_location : unit -> Lambda.lambda
val create_process : Lambda.lambda -> Lambda.lambda
val create_process_location : Ident.t -> Lambda.lambda -> Lambda.lambda
val send_async : Lambda.lambda -> Lambda.lambda -> Lambda.lambda
val send_sync : Ident.t -> int -> int option -> Lambda.lambda -> Lambda.lambda
val direct_send_async :
  Ident.t -> int -> int option -> Lambda.lambda -> Lambda.lambda
val send_async : Lambda.lambda -> Lambda.lambda -> Lambda.lambda
val tail_direct_send_async :
  Ident.t -> int -> int option -> Lambda.lambda -> Lambda.lambda
val tail_send_async : Lambda.lambda -> Lambda.lambda -> Lambda.lambda

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

val principal : Typedtree.expression -> Ident.t option

(* Building definitions and locations *)

val create_auto : 
  Ident.t option -> 'a Typedtree.joinautomaton_gen ->
    Lambda.lambda -> Lambda.lambda

val create_channels :
  'a Typedtree.joinautomaton_gen -> Lambda.lambda -> Lambda.lambda


val create_table:
    Ident.t option ->
      Joinmatch.automaton ->
        (Ident.t * Ident.t option * 'a) array ->
          Lambda.lambda ->
            Lambda.lambda
            
