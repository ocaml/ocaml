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

exception Failed of string * exn

type server 

(* Setup of connection acceptor.
   Either some address is given, or a default inet socket is
   allocated (and returned as space_id) *)
val establish_server :
    Unix.sockaddr option ->
      (Join_link.t -> unit) -> Unix.sockaddr * server

(* raise Failed if server already killed *)
val kill_server : server -> unit

val connect : Unix.sockaddr -> Join_link.t
