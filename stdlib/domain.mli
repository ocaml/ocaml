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

[@@@alert unstable
    "The Domain interface may change in incompatible ways in the future."
]

(** Domains.

    See 'Parallel programming' chapter in the manual.

    @since 5.0 *)

type !'a t
(** A domain of type ['a t] runs independently, eventually producing a
    result of type 'a, or an exception *)

val spawn : (unit -> 'a) -> 'a t
(** [spawn f] creates a new domain that runs in parallel with the
    current domain.

    @raise Failure if the program has insufficient resources to create another
    domain. *)

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

val before_first_spawn : (unit -> unit) -> unit
(** [before_first_spawn f] registers [f] to be called before the first domain
    is spawned by the program. The functions registered with
    [before_first_spawn] are called on the main (initial) domain. The functions
    registered with [before_first_spawn] are called in 'first in, first out'
    order: the oldest function added with [before_first_spawn] is called first.

    @raise Invalid_argument if the program has already spawned a domain. *)

val at_exit : (unit -> unit) -> unit
(** [at_exit f] registers [f] to be called when the current domain exits. Note
    that [at_exit] callbacks are domain-local and only apply to the calling
    domain. The registered functions are called in 'last in, first out' order:
    the function most recently added with [at_exit] is called first. An example:

    {[
let temp_file_key = Thread_local_storage.new_key (fun _ ->
  let tmp = snd (Filename.open_temp_file "" "") in
  Domain.at_exit (fun () -> close_out_noerr tmp);
  tmp)
    ]}

    The snippet above creates a key that when retrieved for the first
    time will open a temporary file and register an [at_exit] callback
    to close it, thus guaranteeing the descriptor is not leaked in
    case the current domain exits. *)

val cpu_relax : unit -> unit
(** If busy-waiting, calling cpu_relax () between iterations
    will improve performance on some CPU architectures *)

val is_main_domain : unit -> bool
(** [is_main_domain ()] returns true if called from the initial domain. *)

val recommended_domain_count : unit -> int
(** The recommended maximum number of domains which should be running
    simultaneously (including domains already running).

    The value returned is at least [1]. *)

module DLS = Thread_local_storage
[@@deprecated "use Thread_local_storage now"]
