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

type - 'a chan

(* Convenience *)
val get_local_addr : unit -> Unix.inet_addr

(* Site identity *)
type site

(* here returns the local site *)
val here : site

(* Get identity of the remote site listening on sockadrr *)
val there : Unix.sockaddr -> site

(* Get identity of the remote site where reception on channel takes place *)
val where_from : 'a chan -> site

(* Test the equality of two sites *)
val same_site : site -> site -> bool

(* Raised when site fails, in response to synchronous calls *)
exception Exit

(* Register a channel to be sent to when site fails *)
val at_fail : site -> unit chan -> unit



(* start to listen for connections *)
val listen : Unix.sockaddr -> unit

(* start with connected socket *)
val connect : Unix.file_descr -> unit

(* Hook for 'at_exit' will somehow control termination of program.
   More precisely, program terminates when they is no more
   work to achieve.
   This does not apply to program engaged in distribution. *)
val exit_hook : unit -> unit

(*
(* Give message to distant sites a chance to leave *)
val flush_space : unit -> unit
*)

(* Give the liste of the socket addresses of the name server *)
val get_sockaddrs : unit -> Unix.sockaddr list

(* Print a message on standard error,
   Usage: debug tag fmt ...
      - fmt ... is in printf style.
      - tag is a string *)
type 'a debug = string -> (('a, unit, string, unit) format4 -> 'a)

val debug : 'a debug

