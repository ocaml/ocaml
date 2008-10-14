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


(**********************************************)
(* Basic join operations from the module Join *)
(**********************************************)

val create_process : Lambda.lambda -> Location.t -> Lambda.lambda
val send_async : Lambda.lambda -> Lambda.lambda ->  Location.t -> Lambda.lambda
val tail_send_async : Lambda.lambda -> Lambda.lambda ->  Location.t -> Lambda.lambda

(* Direct calls *)
val local_send_async :
    Ident.t -> int -> Lambda.lambda -> Location.t -> Lambda.lambda
val local_tail_send_async :
    Ident.t -> int -> Lambda.lambda -> Location.t -> Lambda.lambda
val local_send_sync :
    Ident.t -> int -> Lambda.lambda -> Location.t -> Lambda.lambda
val local_send_alone :
    Ident.t -> Lambda.lambda ->  Location.t -> Lambda.lambda
val local_tail_send_alone :
    Ident.t -> Lambda.lambda -> Location.t -> Lambda.lambda

(* Those two are used to generate calls in dispatchers rhs *)
val local_send_sync2 :
    Ident.t -> Lambda.lambda -> Lambda.lambda -> Lambda.lambda
val local_tail_send_async2 :
    Ident.t -> Lambda.lambda -> Lambda.lambda -> Lambda.lambda
(* Call reply to primitive *)
val reply_to : Lambda.lambda -> Lambda.lambda ->  Location.t -> Lambda.lambda

(* Insert reply_to_exn for the non-principal continuations *)
val get_replies :
    Ident.t option -> Typedtree.expression -> bool * Ident.t list 

val do_spawn : Lambda.lambda ->  Location.t -> Lambda.lambda

(* Is an expression simple enough (no exception, guaranteed to terminate) ? *)
val simple_prim :  (Primitive.description -> bool) ref
val simple_exp : Typedtree.expression -> bool

(* Insert handler in charge of reply_to_exn'ing *)

val lambda_reply_handler :
    Ident.t option ->  Typedtree.expression -> Lambda.lambda -> Lambda.lambda 

val reply_handler :
    Ident.t option ->  Typedtree.expression ->
      (Typedtree.expression -> Lambda.lambda) ->
      Typedtree.expression ->  Lambda.lambda

(* Partition a proc expression into
   principal thread, simple, non_simple expressions *)
val as_procs :
  Ident.t option ->
  Typedtree.expression ->
  Typedtree.expression option * Typedtree.expression list * Typedtree.expression list

val principal : Typedtree.expression -> Ident.t option

(* Building definitions and locations *)

val create_auto : 
  'a Typedtree.joinautomaton_gen ->
    Lambda.lambda -> Lambda.lambda

val create_channels :
  'a Typedtree.joinautomaton_gen -> Lambda.lambda -> Lambda.lambda

val create_dispatchers :
     (Ident.t * Typedtree.joinchannel * Lambda.lambda) list ->
       Lambda.lambda -> Lambda.lambda

val create_forwarders :
    'a Typedtree.joinautomaton_gen list ->
      (Ident.t * Typedtree.joinchannel * Lambda.lambda) list list ->
         (Ident.t * Lambda.lambda) list list ->
           Lambda.lambda -> Lambda.lambda

val create_table:
    Typedtree.joinautomaton ->
      (Ident.t * Ident.t option * 'a) list -> Lambda.lambda ->
      Lambda.lambda
            
(*********************)
(* Global exceptions *)
(*********************)

val transl_exn_global : Location.t -> Path.t -> Lambda.lambda
