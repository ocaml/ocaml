(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Deferred computations. *)

type 'a t = 'a CamlinternalLazy.t
(** A value of type ['a Lazy.t] is a deferred computation, called a suspension,
    that has a result of type ['a]. The special expression syntax [lazy (expr)]
    makes a suspension of the computation of [expr], without computing
    [expr] itself yet. "Forcing" the suspension will then compute [expr] and
    return its result. Matching a suspension with the special pattern syntax
    [lazy(pattern)] also computes the underlying expression and tries to bind
    it to [pattern]:

    {[
      let lazy_option_map f x =
      match x with
      | lazy (Some x) -> Some (Lazy.force f x)
      | _ -> None
    ]}

    Note: If lazy patterns appear in multiple cases in a pattern-matching, lazy
    expressions may be forced even outside of the case ultimately selected by
    the pattern matching. In the example above, the suspension [x] is always
    computed.

    Note: [lazy_t] is the built-in type constructor used by the compiler for the
    [lazy] keyword.  You should not use it directly.  Always use [Lazy.t]
    instead.

    Note: [Lazy.force] is not concurrency-safe. If you use this module with
    multiple fibers, systhreads or domains, then you will need to add some
    locks. The module however ensures memory-safety, and hence, concurrently
    accessing this module will not lead to a crash but the behaviour is
    unspecified.

    Note: if the program is compiled with the [-rectypes] option,
    ill-founded recursive definitions of the form [let rec x = lazy x]
    or [let rec x = lazy(lazy(...(lazy x)))] are accepted by the type-checker
    and lead, when forced, to ill-formed values that trigger infinite
    loops in the garbage collector and other parts of the run-time system.
    Without the [-rectypes] option, such ill-founded recursive definitions
    are rejected by the type-checker.
*)


exception Undefined
(** Raised when forcing a suspension concurrently from multiple fibers,
    systhreads or domains, or when the suspension tries to force itself
    recursively.
*)

external force : 'a t -> 'a = "%lazy_force"
(** [force x] forces the suspension [x] and returns its result. If [x] has
    already been forced, [Lazy.force x] returns the same value again without
    recomputing it.  If it raised an exception, the same exception is raised
    again.

    @raise Undefined (see {!Undefined}).
*)

(** {1 Iterators} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f x] returns a suspension that, when forced,
    forces [x] and applies [f] to its value.

    It is equivalent to [lazy (f (Lazy.force x))].

    @since 4.13
*)

(** {1 Reasoning on already-forced suspensions} *)

val is_val : 'a t -> bool
(** [is_val x] returns [true] if [x] has already been forced and
    did not raise an exception.
    @since 4.00 *)

val from_val : 'a -> 'a t
(** [from_val v] evaluates [v] first (as any function would) and returns
    an already-forced suspension of its result.
    It is the same as [let x = v in lazy x], but uses dynamic tests
    to optimize suspension creation in some cases.
    @since 4.00 *)

val map_val : ('a -> 'b) -> 'a t -> 'b t
(** [map_val f x] applies [f] directly if [x] is already forced,
   otherwise it behaves as [map f x].

   When [x] is already forced, this behavior saves the construction of
   a suspension, but on the other hand it performs more work eagerly
   that may not be useful if you never force the function result.

   If [f] raises an exception, it will be raised immediately when
   [is_val x], or raised only when forcing the thunk otherwise.

   If [map_val f x] does not raise an exception, then
   [is_val (map_val f x)] is equal to [is_val x].

    @since 4.13 *)


(** {1 Advanced}

   The following definitions are for advanced uses only; they require
   familiarity with the lazy compilation scheme to be used appropriately. *)

val from_fun : (unit -> 'a) -> 'a t
(** [from_fun f] is the same as [lazy (f ())] but slightly more efficient.

    It should only be used if the function [f] is already defined.
    In particular it is always less efficient to write
    [from_fun (fun () -> expr)] than [lazy expr].

    @since 4.00 *)

val force_val : 'a t -> 'a
(** [force_val x] forces the suspension [x] and returns its result.  If [x]
    has already been forced, [force_val x] returns the same value again
    without recomputing it.

    If the computation of [x] raises an exception, it is unspecified
    whether [force_val x] raises the same exception or {!Undefined}.
    @raise Undefined if the forcing of [x] tries to force [x] itself
    recursively.

    @raise Undefined (see {!Undefined}).
*)

module Mutexed : sig
  (** Simple mutex-protected lazy thunks, which can be accessed from
      several domains or several threads.

      This implementation has two downsides:
      - It is less optimized than [Lazy.t].
      - It uses a standard library {!Mutex} to wait on concurrent
        initialization, so it does not interoperate well with user-level
        fiber/task abstractions (see the {!Lazy.Mutexed.force} documentation
        for a dangerous example).

      A typical use-case is optional library initialization code that
      is moderately expensive, or acquires resources. The library
      author does not want to do this work on startup, because it may
      not be needed, but using ['a Lazy.t] is incorrect if the library
      may be used in concurrent settings. ['a Lazy.Mutexed.t]
      can be used, as long as the blocking behavior is acceptable.

     {b Note}: ['a Lazy.t] contains a protection against recursively
     forcing a thunk, it will raise {!Undefined}. On the other hand,
     ['a Lazy.Mutexed.t] will try to lock the mutex
     recursively, which will raise [Sys_error].

     See {{!examples} the examples} below.

     @since 5.5
  *)

  type !'a t
  (* A value of type ['a Lazy.Mutexed.t] is similar to a value
     of type ['a Lazy.t], it represents a deferred computation, but it can
     safely be used in concurrent settings as it is protected by a mutex
     during forcing. *)

  val is_val : 'a t -> bool
  (** [is_val x] returns [true] if the deferred computation [x] has
      already been forced and its result is a value, not an
      exception. *)

  val from_val : 'a -> 'a t
  (** [from_val v] is a deferred computation which is already
      finished and whose result is the value [v]. *)

  val from_fun : (unit -> 'a) -> 'a t
  (** [from_fun f] is a deferred computation that will take a mutex
      and call [f] when forced. *)

  val force : 'a t -> 'a
  (** [force x] forces the suspension [x]. If [x] has already been
      forced, [Lazy.force x] returns the same value again without
      recomputing it. If it raised an exception, the same exception is
      raised again.

      If a concurrent call to [force] happens while the result is
      being computed, the caller will block on a {!Mutex}.

      @raise Sys_error if the suspension is forced on a thread where
        it is already being forced. This can happen if the thunk tries
        to force itself recursively, but it can also happen if the
        thunk code contains a `yield()` operation for a user-level
        fiber/task abstraction, and two fibers/tasks try to force the
        thunk in parallel on the same domain:
      {[
        (* This is wrong, a mutex-protected thunk should not yield control. *)
        let thunk = Lazy.Mutexed.from_fun (fun () -> ... Fiber.yield () ...)

        (* It may result in a same-thread [Sys_error] exception below. *)
        let wrong =
          Fiber.both
            (fun () -> Lazy.Mutexed.force thunk)
            (fun () -> Lazy.Mutexed.force thunk)
      ]}

      To avoid such errors, you should ensure that mutex-protected
      thunks never yield control outside of their sub-computation --
      by not using a user-level thread library within it, or by using
      appropriate blocking/masking functions if available. *)


  (** {1:examples Examples}

      A typical use-case is to initialize some library-local
      state that is used by library functions.

      {[
        let config = Lazy.Mutexed.from_fun (fun () ->
          match Sys.getenv_opt "MYLIB_CONFIG_PATH" with
          | None | Some "" -> Config.default ()
          | Some path -> Config.read_from_path path
        )
      ]}

      {[
        let entropy =
          (* we use a mibibyte of random data from /dev/urandom *)
          Lazy.Mutexed.from_fun (fun () ->
            In_channel.with_open_bin "/dev/urandom" (fun chan ->
              In_channel.really_input_string chan (1024 * 1024)
            )
          )
      ]}
 *)
end
