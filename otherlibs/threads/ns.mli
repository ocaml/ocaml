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

val local_addr : Unix.inet_addr

type server

val start_server : int -> server
val stop_server : server -> unit

type link

val register_client : Unix.inet_addr -> int -> link
val lookup : link -> string -> 'a

val register : link -> string -> 'a -> unit

val sync_register : link -> string -> 'a -> unit

val sync_register_once : link -> string -> 'a -> bool
