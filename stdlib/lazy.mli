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
   familiary with the lazy compilation scheme to be used appropriately. *)

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
