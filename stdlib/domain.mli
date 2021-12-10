(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                   Tom Kelly, OCaml Labs Consultancy                    *)
(*                                                                        *)
(*   Copyright 2019 Indian Institute of Technology, Madras                *)
(*   Copyright 2014 University of Cambridge                               *)
(*   Copyright 2021 OCaml Labs Consultancy Ltd                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a t
(** A domain of type ['a t] runs independently, eventually producing a
    result of type 'a, or an exception *)

val spawn : (unit -> 'a) -> 'a t
(** [spawn f] creates a new domain that runs in parallel with the
    current domain. *)

val join : 'a t -> 'a
(** [join d] blocks until domain [d] runs to completion.
    If [d] results in a value, then that is returned by [join d].
    If [d] raises an uncaught exception, then that is thrown by [join d].
    Domains may only be joined once: subsequent uses of [join d]
    raise Invalid_argument. *)

type id = private int
(** Domains have unique integer identifiers *)

val get_id : 'a t -> id
(** [get_id d] returns the identifier of the domain [d] *)

val self : unit -> id
(** [self ()] is the identifier of the currently running domain *)

val at_first_spawn : (unit -> unit) -> unit
(** Register the given function to be called before the first domain is
    spawned.

    @raise Invalid_argument if the first domain has already been spawned. *)

val at_exit : (unit -> unit) -> unit
(** Register the given function to be called at when a domain exits. This
    function is also registered with {!Stdlib.at_exit}. If the registered
    function raises an exception, the exceptions are ignored. *)

val at_startup : (unit -> unit) -> unit
(** Register the given function to be called when a domain starts. This
    function is called before the callback specified to [spawn f] is
    executed. *)

val cpu_relax : unit -> unit
(** If busy-waiting, calling cpu_relax () between iterations
    will improve performance on some CPU architectures *)

val set_name : string -> unit
(** [set_name s] set the domain's thread name to [s]. [s] should not be longer
    than 15 characters. If [s] is longer than 15 characters,
    raise Invalid_argument. *)

val is_main_domain : unit -> bool
(** [is_main_domain ()] returns true if called from the initial domain. *)

module DLS : sig
(** Domain-local Storage *)

    type 'a key
    (** Type of a DLS key *)

    val new_key : (unit -> 'a) -> 'a key
    (** [new_key f] returns a new key bound to initialiser [f] for accessing
        domain-local variable. *)

    val set : 'a key -> 'a -> unit
    (** [set k v] updates the calling domain's domain-local state to associate
        the key [k] with value [v]. It overwrites any previous values associated
        to [k], which cannot be restored later. *)

    val get : 'a key -> 'a
    (** [get k] returns [v] if a value [v] is associated to the key [k] on
        the calling domain's domain-local state. Sets [k]'s value with its
        initialiser and returns it otherwise. *)

  end
