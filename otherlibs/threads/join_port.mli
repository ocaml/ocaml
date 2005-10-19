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

exception Failed

type server 

val establish_server :
    int -> (Join_link.t -> unit) -> Join_types.space_id * server

(* raise Failed if server already killed *)
val kill_server : server -> unit

val connect : Unix.sockaddr -> Join_link.t
