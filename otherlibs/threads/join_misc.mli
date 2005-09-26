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

(* Support for concurrents readers and writers *)

type controler

val controler_create : unit -> controler

val protect_read : controler -> ('a -> 'b) -> 'a -> 'b

val protect_write : controler -> ('a -> 'b) -> 'a -> 'b

(* Wrapped socket primitives *)

val prerr_exn : exn -> unit

val exn_to_string : exn -> string

val local_name : string

val local_addr : Unix.inet_addr

val create_port : int -> int * Unix.file_descr

val force_accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr

val connect_to_server : Unix.inet_addr -> int -> Unix.file_descr

val force_connect : Unix.inet_addr -> int -> Unix.file_descr

(* Why not put this here ! *)
(* To commit suicide from join managed threads : raise JoinExit *)
exception JoinExit

