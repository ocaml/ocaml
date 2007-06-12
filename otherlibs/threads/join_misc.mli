(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)


(****************************)
(* Readers/writer controler *)
(****************************)

type controler

val controler_create : unit -> controler
val protect_read : controler -> ('a -> 'b) -> 'a -> 'b
val protect_write : controler -> ('a -> 'b) -> 'a -> 'b

(*******************************************)
(* Locked counters, with detection of zero *)
(*******************************************)

type counter

val counter_create : unit -> counter
val incr : counter -> unit
val decr : counter -> unit
val wait_zero : counter -> unit

(*****************************)
(* Misc stuff for exceptions *)
(*****************************)

val prerr_exn : exn -> unit
val exn_to_string : exn -> string

(*****************************)
(* Wrapped socket primitives *)
(*****************************)

val local_name : string
val get_local_addr : unit -> Unix.inet_addr
val string_of_sockaddr : Unix.sockaddr -> string

(* Why not put this here ! *)
(* To commit suicide from join managed threads : raise JoinExit *)
exception JoinExit

(************************)
(* Join library options *)
(************************)

val globalize_flags : Marshal.extern_flags list
