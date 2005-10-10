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

(* abstract type of running server *)
type server

val start_server : int -> server
val stop_server : server -> unit

(* abstract type of connection to name server *)
type link

(* open connection *)
val register_client : Unix.inet_addr -> int -> link

(* find value, raise Not_found when not present *)
val lookup : link -> string -> 'a

(* register binding, returns when done at server side *)
val register : link -> string -> 'a -> unit

(* additionaly register binding once,
   If a binding with the same key already exists,
   the binding is not changed and function returns false *)
val register_once : link -> string -> 'a -> bool
