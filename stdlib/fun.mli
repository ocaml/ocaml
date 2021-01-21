(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Function manipulation.

    @since 4.08 *)

(** {1:combinators Combinators} *)

external id : 'a -> 'a = "%identity"
(** [id] is the identity function. For any argument [x], [id x] is [x]. *)

val const : 'a -> (_ -> 'a)
(** [const c] is a function that always returns the value [c]. For any
    argument [x], [(const c) x] is [c]. *)

val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
(** [flip f] reverses the argument order of the binary function
    [f]. For any arguments [x] and [y], [(flip f) x y] is [f y x]. *)

val negate : ('a -> bool) -> ('a -> bool)
(** [negate p] is the negation of the predicate function [p]. For any
    argument [x], [(negate p) x] is [not (p x)]. *)

(** {1:exception Exception safety} *)

val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
(** [protect ~finally work] invokes [work ()] and then [finally ()]
    before [work ()] returns with its value or an exception. In the
    latter case the exception is re-raised after [finally ()]. If
    [finally ()] raises an exception, then the exception
    {!Finally_raised} is raised instead.

    [protect] can be used to enforce local invariants whether [work
    ()] returns normally or raises an exception. However, it does not
    protect against unexpected exceptions raised inside [finally ()]
    such as {!Stdlib.Out_of_memory}, {!Stdlib.Stack_overflow}, or
    asynchronous exceptions raised by signal handlers (e.g.
    {!Sys.Break}). For resource-safety, see instead {!with_resource}.

    Note: It is a {e programming error} if other kinds of exceptions
    are raised by [finally], as any exception raised in [work ()] will
    be lost in the event of a {!Finally_raised} exception. Therefore,
    one should make sure to handle those inside the finally. *)

exception Finally_raised of exn
(** [Finally_raised exn] is raised by [protect ~finally work] when
    [finally] raises an exception [exn]. This exception denotes either
    an unexpected exception or a programming error. As a general rule,
    one should not catch a [Finally_raised] exception except as part of
    a catch-all handler. *)

val with_resource :
  acquire:('a -> 'r) -> 'a -> scope:('r -> 'b) -> release:('r -> unit) -> 'b
(** [with_resource ~acquire x work ~release] invokes [acquire x], then
    invokes [work] on the resulting value, and then invokes [release]
    on the value, whether [work] returns or raises an exception. The
    result of [work] is then produced, whether it is a value or an
    exception.

    It is guaranteed that [release] is called upon return of
    [with_resource] on the result of [acquire] if and only if the
    latter returned normally. During the execution of acquire and
    release, asynchronous callbacks that can raise exceptions are
    delayed.

    If [release] raises an exception, then it is treated as a fatal
    error similarly to an uncaught exception. It executes [at_exit]
    handlers and brings the program to an early [exit(2)].

    The purpose of [with_resource] is to offer guarantees about the
    release of system or custom resources. It can therefore be used to
    ensure consistency of state, even in the event of unexpected and
    asynchronous exceptions.

    To achieve this, it is sufficient to fulfill the following
    conditions:

    1) the acquisition either succeeds, or if it fails, it does so by
       raising an exception, without having acquired the resource, for
       instance by undoing changes (strong exception safety);

    2) the release of the resource never fails, in particular it never
       raises (no-raise guarantee).

    To help write correct acquisition and release functions,
    asynchronous exceptions (e.g. {!Sys.Break}) are guaranteed not to
    occur during acquisition and release. Care must still be taken
    regarding other unexpected exceptions such as
    {!Stdlib.Out_of_memory} or {!Stdlib.Stack_overflow}.

    For an implementation of an "unwind-protect" combinator, which
    instead allows arbitrary code during clean-up, to enforce local
    invariants, see {!protect}.

    @since 4.XX
*)
