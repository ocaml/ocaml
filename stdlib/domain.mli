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

type !'a t
(** A domain of type ['a t] runs independently, eventually producing a
    result of type 'a, or an exception *)

val spawn : (unit -> 'a) -> 'a t
(** [spawn f] creates a new domain that runs in parallel with the
    current domain. *)

val join : 'a t -> 'a
(** [join d] blocks until domain [d] runs to completion. If [d] results in a
    value, then that is returned by [join d]. If [d] raises an uncaught
    exception, then that is re-raised by [join d]. *)

type id = private int
(** Domains have unique integer identifiers *)

val get_id : 'a t -> id
(** [get_id d] returns the identifier of the domain [d] *)

val self : unit -> id
(** [self ()] is the identifier of the currently running domain *)

val at_first_spawn : (unit -> unit) -> unit
(** Register the given function to be called before any domain is
    spawned (except the initial domain) and before any function registered
    with {!val:at_startup} is called. The functions registered with
    [at_first_spawn] are called in 'last in, first out' order: the function
    most recently added with [at_first_spawn] is called first.

    @raise Invalid_argument if the first domain has already been spawned. *)

val at_exit : (unit -> unit) -> unit
(** Register the given function to be called at when a spawned domain exits.
    This function is also registered with {!Stdlib.at_exit}. If the registered
    function raises an exception, the exceptions are ignored. The registered
    functions are called in 'last in, first out' order: the function most
    recently added with [at_exit] is called first. *)

val at_startup : (unit -> unit) -> unit
(** Register the given function to be called when a domain starts. This
    function is called before the callback specified to [spawn f] is
    executed. The registered functions are called in 'last in, first out'
    order: the function most recently added with [at_startup] is
    called first. *)

val cpu_relax : unit -> unit
(** If busy-waiting, calling cpu_relax () between iterations
    will improve performance on some CPU architectures *)

val is_main_domain : unit -> bool
(** [is_main_domain ()] returns true if called from the initial domain. *)

module DLS : sig
(** Domain-local Storage *)

    type 'a key
    (** Type of a DLS key *)

    val new_key : ?split_from_parent:('a -> 'a) -> (unit -> 'a) -> 'a key
    (** [new_key f] returns a new key bound to initialiser [f] for accessing
        domain-local variables.

        If [split_from_parent] is provided, spawning a domain will derive the
        child value (for this key) from the parent value.

        Note that the [split_from_parent] call is computed in the parent
        domain, and is always computed regardless of whether the child domain
        will use it. If the splitting function is expensive or requires
        client-side computation, consider using ['a Lazy.t key].
    *)

    val get : 'a key -> 'a
    (** [get k] returns [v] if a value [v] is associated to the key [k] on
        the calling domain's domain-local state. Sets [k]'s value with its
        initialiser and returns it otherwise. *)

    val set : 'a key -> 'a -> unit
    (** [set k v] updates the calling domain's domain-local state to associate
        the key [k] with value [v]. It overwrites any previous values associated
        to [k], which cannot be restored later. *)

  end
