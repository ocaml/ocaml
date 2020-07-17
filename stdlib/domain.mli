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
  (** Low-level synchronisation primitives.

      The general usage pattern for these primitives is to test a
      predicate of atomic variables in a critical section and call
      [wait] if the predicate does not hold. The domain that causes
      the predicate to become true must then call [notify].

      For example, here one domain waits for another to complete work:

          let done = Atomic.make false

          let rec await_completion () =
            let success =
              critical_section (fun () ->
                if Atomic.get done then true else (wait (); false)) in
            if success then () else await_completion ()

          let signal_completion waiting_domain =
            Atomic.set success true;
            notify waiting_domain

      Semantically, the primitives are similar to having a single
      monitor (or mutex + condition variable) per domain. That is,
      [critical_section f] acquires the current domain's mutex and
      runs [f], [wait ()] waits on the current domain's condition
      variable (releasing the mutex during the wait), and [notify d]
      acquires domain [d]'s mutex and signals its condition variable.
      The only difference from standard monitors is that [notify d]
      waits for any in-progress critical section to complete.

      However, the actual implementation is somewhat different. In
      particular, [critical_section f] is cheaper than acquiring a mutex,
      and performs no more atomic operations than [f] does. *)

  exception Retry

  val critical_section : (unit -> 'a) -> 'a
  (** [critical_section f] runs [f], but blocks notifications until
      [f] returns. See [notify] below.
      If [f] raises [Retry], then the critical section is restarted. *)

  val notify : id -> unit
  (** If the domain [d] is within a critical section (i.e. is evaluating
      [critical_section f]), then [notify d] marks this critical section
      as "notified" (causing any call to [wait] to return, see below),
      and waits for the critical section to complete before returning.
      If [d] is not in a critical section, then [notify d] does nothing. *)


  val wait : unit -> unit
  (** [wait] must be called from within a critical section, and returns
      only when that critical section is notified by a call to [notify].
      It does not matter whether [notify] is called before or after
      [wait] begins: it is the critical section that is being notified,
      not the call to wait. If wait is called and finds that the current
      critical has already been notified, it returns immediately.

      Calling [wait ()] twice within the same critical section is not
      useful: the first call to [wait ()] returns when the critical
      section is notified, so the second call to [wait] will always
      return immediately, as the critical section is already notified. *)

  type timeout_or_notified = Timeout | Notified

  val wait_for : nanoseconds -> timeout_or_notified
  (** As with [wait], must be called from within a critical section.
      Same as [wait], but returns once the specified number of
      nanoseconds has elapsed, regardless of whether [notify]
      is called *)

  val wait_until : nanoseconds -> timeout_or_notified
  (** As with [wait] and [wait_for], must be called from within a critical section.
      [wait_until t] is the same as [wait ()], but returns once
      [timer_ticks () > t], regardless of whether [notify] is
      called *)

  val cpu_relax : unit -> unit
  (** If busy-waiting, calling cpu_relax () between iterations
      will improve performance on some CPU architectures *)

  external poll : unit -> unit = "%poll"
  (** poll for interrupts *)
end

module TLS : sig
(** Thread-local Storage *)

    type 'a key
    (** Type of a TLS key *)

    val new_key : unit -> 'a key
    (** Returns a new key for accessing thread-local variable. The type of
        the variable which will be stored with the key needs to be specified
        during invocation. For example, to generate a key associated to an
        integer:
        let k1 : int Domain.TLS.key = Domain.TLS.new_key () *)

    val set : 'a key -> 'a -> unit
    (** [set k v] updates the calling domain's thread-local state to
        associate the key [k] with value [v] *)

    val get : 'a key -> 'a option
    (** [get k] returns [Some v] if a value was previously associated with
        the key [k] in the calling domain's thread-local state. Otherwise returns [None].  *)

  end
