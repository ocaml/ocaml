(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Join_types

type site

val local_addr : Unix.inet_addr

val here : unit -> site


(* Services provide RPC by name *)
type service 

val remote_service : Unix.sockaddr -> string -> service
val register_service : string -> ('a -> 'b) -> unit
val call_service : service -> 'a -> 'b


(* start to listen for connections *)
val listen : Unix.sockaddr -> unit

(* start with connected socket *)
val connect : Unix.file_descr -> unit

(* Hook for 'at_exit' will somehow control termination of program.
   More precisely, program terminates when they is no more
   work to achieve.
   This does not apply to program engaged in distribution. *)
val exit_hook : unit -> unit

(* register a channel to be sent to when site fails *)
val at_fail : site -> unit channel -> unit

(*
(* Give message to distant sites a chance to leave *)
val flush_space : unit -> unit
*)

(* Give the liste of the socket addresses of the name server *)
val get_sockaddrs : unit -> Unix.sockaddr list

(* Various levels of debuging as directed by the
   environment variable VERBOSE *)
type 'a debug = string -> (('a, unit, string, unit) format4 -> 'a)

val debug : 'a debug
val debug0 : 'a debug
val debug1 : 'a debug
val debug2 : 'a debug
val debug3 : 'a debug

