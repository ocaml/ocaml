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

val self : unit -> id
(** [self ()] is the identifier of the currently running domain *)

type nanoseconds = int64
val timer_ticks : unit -> nanoseconds
(** Returns the number of nanoseconds elapsed since the OCaml
    runtime started. *)

module Sync : sig
  (** Low-level Domain related primitives. **)

  val cpu_relax : unit -> unit
  (** If busy-waiting, calling cpu_relax () between iterations
      will improve performance on some CPU architectures *)

  external poll : unit -> unit = "%poll"
  (** poll for interrupts *)
end

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
