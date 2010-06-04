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

  This module offers basic functionalities for JoCaml.
    
*)


type - 'a chan
(** The type of asynchronous channels carrying values of type ['a]. *)


exception Exit
(** Raised by the JoCaml runtime system,
    when some remote synchronous call cannot be completed because of
    the failure of the remote site *)


val exit_hook : unit -> unit
(** Hook to be given as argument to {!Pervasives.at_exit}.
    This  will somehow control termination of program.
    More precisely, program terminates when they is no more
    work to achieve.
    This does not apply to program engaged in distribution.
    @see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#VALat_exit> [Pervasives.at_exit]. *)


type 'a debug = string -> (('a, unit, string, unit) format4 -> 'a)
(** The type of the  argument of {!Join.debug} *)

val debug : 'a debug
(** Print a message on standard error.
    Usage: debug tag fmt ...
       - fmt ... is in printf style.
       - tag is a string.
    A lock is taken so that messages do not interleave.
@see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Printf.html> The [Printf] module.
*)

(** Site definition *)

module Site : sig 

  (**
    Sites are abstractions of JoCaml runtimes. Sites have unique
    identities, which can be passed on channels, computed from Internet
    addresses or extracted from asynchronous channels.

    Sites must be compared with the [equal] and [compare] functions
    of this module. Sites are useful for performing crude failure detection
    (See  {!Site.at_fail} below).
  *)

  type t
  (** The type of site identities. *)

  val here : t
  (** Local site identity. *)

  val there : Unix.sockaddr -> t
  (** Get identity of the remote site listening on sockaddr.
      Raise [Failure] if the connection cannot be established. *)

  val listen : Unix.sockaddr -> unit
  (** Start to listen for connections on the socket address given as argument.
      Raises [Failure] in case of failure. *)


  (**/**)
  val connect : Unix.file_descr -> unit
  (** start with connected socket *)
  (**/**)


  val where_from : 'a chan -> t
  (** [where_from c] returns the identity of the remote site where reception
      on channel [c] takes place. *)

  val equal : t -> t -> bool
  (** Test the equality of two sites. *)

  val compare : t -> t -> int
  (** Compare two sites, order is arbitrary. *)

  val at_fail : t -> unit chan -> unit
 (** [at_fail s c] registers channel [c] as a guard on failure of site  [s].
     If [s] failure is detected, a message () is sent on channel [c]. 

     At the moment, site failure detection is
     a bit unsafe, due to naive routing.
     A failure may express the impossibility to contact a remote site for
    the first time. *)

  val get_local_addr : unit -> Unix.inet_addr
  (** Returns the default Internet address of the local site,
      never fails. At worst, [get_local_addr ()] returns the loopback
      address [Unix.inet_addr_loopback] *)

end


(** Dynamic, unsafe, value repository. *)

module Ns : sig
(** Dynamic, unsafe, value repository.

    Every site offers a name service. The name service provides
    a mapping from strings to values.
*)

  type t
  (** Abstract type for the name service. *)

  val here : t
  (** The local name service. *)

  val of_site : Site.t -> t
  (** Get remote name service by site identity. *)

  val to_site : t -> Site.t
  (** Converse operation *)

  val there : Unix.sockaddr -> t
  (** Get remote name service by socket address.
      Basically, [there addr] is [of_site (Site.there addr)]. *)

  val of_sockaddr : Unix.sockaddr -> t
  (** Synonym for [there] *)



  val lookup : t -> string -> 'a
  (** Find value, raise [Not_found] when not present, or {!Join.Exit} if
  attemping to lookup on a failed remote name service. *)

  val register : t -> string -> 'a -> unit
  (** Register binding, returns when done.
      Raise {!Join.Exit} if attempting to register on a failed remote name service. *)

end

