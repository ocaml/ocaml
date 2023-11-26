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

val curry2 : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val curry3 : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val curry4 : ('a * 'b * 'c * 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val curry5 : ('a * 'b * 'c * 'd * 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val curry6 : ('a * 'b * 'c * 'd * 'e * 'f -> 'g) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val curry7 : ('a * 'b * 'c * 'd * 'e * 'f * 'g -> 'h) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h
val curry8 : ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> 'i) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i
val curry9 : ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i -> 'j) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j
(** [curryN f] is a function that takes N arguments one at a time
    and calls [f] with them packaged as an N-tuple.
    For any arguments [f], [x], and [y], [curry2 f x y] is [f (x, y)].

    @since 5.2 *)

val uncurry2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val uncurry3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd
val uncurry4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a * 'b * 'c * 'd -> 'e
val uncurry5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a * 'b * 'c * 'd * 'e -> 'f
val uncurry6 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'a * 'b * 'c * 'd * 'e * 'f -> 'g
val uncurry7 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g -> 'h
val uncurry8 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> 'i
val uncurry9 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i -> 'j
(** [uncurryN f] is a function that takes an N-tuple and calls [f]
    with those arguments one at a time.
    For any arguments [f], [x], and [y], [uncurry2 f (x, y)] is [f x y].

    @since 5.2 *)

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
(** Synonym for [curry2].

    @since 5.2 *)

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
(** Synonym for [uncurry2].

    @since 5.2 *)

(** {1:exception Exception handling} *)

val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
(** [protect ~finally work] invokes [work ()] and then [finally ()]
    before [work ()] returns with its value or an exception. In the
    latter case the exception is re-raised after [finally ()]. If
    [finally ()] raises an exception, then the exception
    {!Finally_raised} is raised instead.

    [protect] can be used to enforce local invariants whether [work ()]
    returns normally or raises an exception. However, it does not
    protect against unexpected exceptions raised inside [finally ()]
    such as {!Stdlib.Out_of_memory}, {!Stdlib.Stack_overflow}, or
    asynchronous exceptions raised by signal handlers
    (e.g. {!Sys.Break}).

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
