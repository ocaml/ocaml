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

(** Helper functions for client and server initialization. *)


(** {6 Configuration} *)

type fork_args =
  | No_argument (** No argument is passed to client. *)
  | Same_arguments of string array (** All clients will reveive the same arguments. *)
  | Argument_generator of (unit -> string array) (** The function will be called repeatedly to compute the arguments passed to the various clients. *)
(** Type of arguments passed to forked clients. *)

val do_forks : string -> fork_args -> int -> int list
(** [do_forks prog args n] does [n] fork (none if [n] is negative),
    using [prog] as the program name, and passing [args] to the
    forked programs. Returns the pid list of the forked programs. *)

type configuration = {
    mutable host : string; (** Hostname of server. *)
    mutable port : int; (** Listening port of server. *)
    mutable clients : int; (** Number of clients to fork. *)
    mutable client_name : string; (** Name of client to fork. *)
    mutable fork_args : fork_args; (** Arguments passed to forked clients. *)
    mutable magic_id : string; (** Identifier of magic value. *)
    mutable magic_value : string; (** Magic value. *)
}
(** Type of configuration for either client or server. *)

val default_configuration : unit -> configuration
(** [default_configuration ()] returns a configuration with default values. *)

val make_configuration :
    ?key_prefix:string ->
    unit ->
    configuration * (Arg.key * Arg.spec * Arg.doc) list
(** [make_configuration ~key_prefix ()] returns a couple with:
    - a configuration with default values;
    - a list of argument descriptors that will update the aforementioned
    configuration when parsed through [Arg.parse] (or equivalent).

    The current version defines the following arguments:
    - {i XXX-host} to set [host] and [port] using ["host:port"] notation;
    - {i XXX-clients} to set [clients];
    - {i XXX-client-name} to set [client_name];
    
    where {i XXX} is the value of [key_prefix] (defaulting to ["-join"]. *)

val parse_configuration :
    ?key_prefix:string ->
    ?merge_args:bool ->
    ?argv:string array ->
    ?start:int ->
    (Arg.key * Arg.spec * Arg.doc) list ->
    Arg.anon_fun ->
    Arg.usage_msg ->
    configuration
(** [parse_configuration ~key_prefix ~merge_args ~argv ~start speclist anon_fun usage_msg cfg]
    is equivalent to
    [Arg.parse_argv ~argv:argv ~current:(ref start) speclist' anon_fun usage_msg]
    where [speclist'] is [speclist] augmented with the argument descriptors
    produced by [make_configuration ~key_prefix:key_prefix ()] ;
    [merge_args] indicates whether [speclist'] is built by merging or appenging
    the lists (defaulting to the latter).
    Returns the client configuration corresponding to these arguments. *)


(** {6 Client-related functions} *)

type 'a lookup_function = Join.Ns.t -> string -> 'a
(** Type of function accessing a name service to retrieve the value associated
    with a name. *)

val lookup_once : 'a lookup_function
(* A lookup function that tries to retrieve the value only once,
   raising [Not_found] if value is not present and [Join.Exit] if
   remote name service is down. *)

val lookup_times : int -> float -> 'a lookup_function
(** [lookup_times n w] builds a lookup function that tries up to [n] times to
    retrieve the value, waiting [w] seconds between two attempts.
    Will try indefinitely if [n <= 0]. *)

type at_fail_chan = unit Join.chan
(** Type of channels receiving [()] on failure of a site. *)

val do_at_fail : (unit -> unit) -> at_fail_chan
(** [do_at_fail f] builds a channel that calls [f] upon failure.
    Exceptions raised by the function are silently ignored. *)

val do_nothing_at_fail : at_fail_chan
(** A channel that does noting upon failure. *)

val exit_at_fail_with_code : int -> at_fail_chan
(** [exit_at_fail_with_code c] builds a channel that terminates the current
    process with code [c] upon failure. *)

val exit_at_fail : at_fail_chan
(** Bare alias for [exit_at_fail_with_code 0]. *)

exception Invalid_magic of string * string
(** Raised when a client tries to connect to a server with a different magic number.
    The first component is the waited magic value while the second component is the 
    magic value retrieved from the server. *)

val init_client : ?at_fail:at_fail_chan -> ?lookup:'a lookup_function -> configuration -> Join.Ns.t
(** [init_client ~at_fail ~lookup cfg] initializes a client by connecting it to
    the server referenced by [cfg], and registering the channel [at_fail]
    with its failure. Returns the name service associated with the server.
    Also ensures that client and server have the same magic number (using
    [lookup] to retrieve it), raising [Invalid_magic] if not. *)

val init_client_with_lookup :
   ?at_fail:at_fail_chan ->
   ?lookup:'a lookup_function ->
   configuration ->
   string ->
   Join.Ns.t * 'a
(** [init_client_with_lookup ~at_fail ~lookup cfg name] executes
    [init_client ~at_fail:at_fail ~lookup:lookup cfg], fork clients,
    and uses the returned name service to look up for [name] using
    the function [lookup].
    Returns both the name service and the value associated with [name].
    Also ensures that client and server have the same magic number,
    raising [Invalid_magic] if not. *)


(** {6 Server-related functions} *)

val init_server : configuration -> Join.Ns.t * int list
(** [init_server cfg] initializes a server listening for connections,
    registers the magic value, and forks clients.
    Returns both the local name service, and the list of the client pids. *)

val init_server_with_register :
    configuration ->
    string ->
    'a ->
    Join.Ns.t * int list
(** [init_server_with_register cfg name value] executes [init_server cfg] and
    uses the returned name service to register [value] as [name].
    Returns both the local name service, and the list of the client pids. *)


(** {6 Miscellaneous functions} *)

val wait_forever : unit -> unit
(** Just waits forever; useful to guarantee that code will not exit prematurely. *)
