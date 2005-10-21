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

(* abstract type of connection to name server *)
type link

(* the local name server *)
val local : link 

(* open connection with remote name server *)
val connect : Unix.sockaddr -> link

(* find value, raise Not_found when not present *)
val lookup : link -> string -> 'a

(* register binding, returns when done at server side *)
val register : link -> string -> 'a -> unit


