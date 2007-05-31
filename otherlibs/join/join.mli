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

(** The JoCaml core library.

    
*)


type - 'a chan
(** The type of asynchronous channels carrying values of type ['a]. *)

val get_local_addr : unit -> Unix.inet_addr
(** Convenience *)


exception Exit
(** Raised when site fails, in response to synchronous calls *)


val listen : Unix.sockaddr -> unit
(** start to listen for connections *)


(**/**)
val connect : Unix.file_descr -> unit
(** start with connected socket *)
(**/**)


val exit_hook : unit -> unit
(** Hook for [at_exit] will somehow control termination of program.
    More precisely, program terminates when they is no more
    work to achieve.
    This does not apply to program engaged in distribution. *)


type 'a debug = string -> (('a, unit, string, unit) format4 -> 'a)

val debug : 'a debug
(** Print a message on standard error,
    Usage: debug tag fmt ...
       - fmt ... is in printf style.
       - tag is a string 
*)


module Site : sig 
  (** Site definition *)

  type t
  (** the type of site identities *)

  val here : t
  (** [here] returns the local site *)

  val there : Unix.sockaddr -> t
  (** Get identity of the remote site listening on sockadrr *)


  val where_from : 'a chan -> t
  (** Get identity of the remote site where reception on channel takes place *)

  val same_site : t -> t -> bool
  (** Test the equality of two sites *)

  val at_fail : t -> unit chan -> unit
 (** Register a channel to be sent to when site fails *)

end

module Ns : sig
  type t
  (** abstract type for the name service *)

  val here : t
  (** the local name service *)

  val of_site : Site.t -> t
  (** get remote name service *)

  val of_sockaddr : Unix.sockaddr -> t
  (** get remote name service by socket address *)

  val lookup : t -> string -> 'a
  (** find value, raise Not_found when not present *)

  val register : t -> string -> 'a -> unit
  (** register binding, returns when done *)

end

