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

module Atomic_repeating : sig
  (** Atomic, repeating deferred computations.

     This implementation is less optimized than [Lazy.t], but it can
     be used in a concurrent setting.

     OCaml domains do not provide a common abstraction to block on
     another computation. Forcing an [Atomic_repeating.t] thunk does
     not block when races happen, instead it may repeat the
     computation of the result several times. We do guarantee that
     if two calls to {!force} on the same suspended computation return
     a value, then they return the same value, even in presence of
     forcing races.

     A typical use-case for atomic, repeating deferred computations is
     optional library initialization code that is moderately
     expensive, or acquires resources. The library author does not
     want to do this work on startup, because it may not be needed,
     but using ['a Lazy.t] is incorrect if the library may be used in
     concurrent settings. ['a Lazy.Atomic_repeating.t] can be used, as
     long as the fact that duplications are repeated is acceptable.

     {b Warning}: ['a Lazy.t] contains a protection against recursively
     forcing a thunk, it will raise {!Undefined}. On the other hand,
     ['a Lazy.Atomic_repeating.t] will recursively repeat the computation,
     which may loop.

     See {{!examples} the examples} below.
  *)

  type 'a t
  (* A value of type ['a Lazy.Atomic_repeating.t] is similar to a value
     of type ['a Lazy.t], it represents a deferred computation, but it can
     safely be used in concurrent settings.

     If a calling domain attempts to {!force} a value that is already
     being forced, the calling domain is not suspended. Instead, the
     computation of the value will be repeated on the calling
     domain. In other words, [Atomic_repeating.t] can duplicate
     computations.

     The implementation ensures that all call to {!force} return the
     same value or raise the same exception: if a repeated terminates
     on a result, its value or exception will be discarded.
  *)

  val from_val : 'a -> 'a t
  (** [from_val v] is a deferred computation which is already
      finished and whose result is the value [v]. *)

  val from_fun : ?discard:('a -> unit) -> (unit -> 'a) -> 'a t
  (** [from_fun ?discard f] is a deferred computation that will call
      [f] when forced. Note that [f] may be called several times
      in the case of concurrent races.

      If [f] is called several times, one result will be stored as the
      result of this computation. Other values computed concurrently
      will be discarded, after being passed to the [discard] function
      (a no-op by default). On the other hand, exceptions raised by
      concurrent computations will be re-raised, as well as exceptions
      raised by [discard].
*)

  val force : 'a t -> 'a
  (** [force x] forces the suspension [x]. If [x] has already been
      forced, [Lazy.force x] returns the same value again without
      recomputing it. If it raised an exception, the same exception is
      raised again.

      If there is a race between several calls to [force], the
      computation may be repeated several times. If some of them fail
      with an exception, they will re-raise it; but all those that
      return a value will return the same value. *)


  (** {1:examples Examples}

      A typical use-case is to initialize some library-local
      state that is used by library functions.

      {[
        let config = Lazy.Atomic_repeating.from_fun (fun () ->
          match Sys.getenv "MYLIB_CONFIG_PATH" with
          | exception _ -> Config.default ()
          | path -> Config.read_from_path path
        )
      ]}

      The environment access and file read may be repeated several
      times in the case of concurrent forcing, but the "first"
      configuration to be computed will be returned by all callers.

      {3:examples_discard Using the [?discard] parameter.}

      The [?discard] argument is useful to release resources if
      a repeated result is discarded.

      {[
        let log_file_and_channel =
          let acquire () =
            match Sys.getenv "MYLIB_LOG_PATH" with
            | exception _ ->
                let path, chan = Filename.open_temp_file "mylib" ".log" in
                (`Temp path), chan
            | path ->
                let chan = Out_channel.open_bin path in
                (`User path), chan
          in
          let discard (source, chan) =
            Out_channel.close chan;
            match source with
            | `User _ -> ()
            | `Temp path -> Sys.remove path
          in
          Lazy.Atomic_repeating.from_fun ~discard acquire
      ]}

      {3:examples_sync User synchronization}

      Users of this module can add their own synchronization logic to
      avoid repeated computations. For example, in an application
      which uses threads and mutex:

      {[
        let entropy =
          (* we use a mibibyte of random data from /dev/urandom *)
          let init_mutex = Mutex.create () in
          let result = ref None in
          Lazy.Atomic_repeating.from_fun (fun () ->
            Mutex.protect init_mutex (fun () ->
              match !result with
              | Some v -> v
              | None ->
                  let v =
                    In_channel.with_open_bin "/dev/urandom" (fun chan ->
                      In_channel.really_input_string chan (1024 * 1024)
                    )
                  in
                  result := Some v;
                  v
            )
          )
      ]}

      A program using this definition will open "/dev/urandom" at most
      once. Note that the mutex is only taken on [force] calls that
      happen while the initialization is not yet finished -- typically
      the first call, or possibly several concurrent first calls. Once
      initialization is finished, the value will be returned directly.
 *)
end
