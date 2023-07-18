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
(** [id] is the identity function. For any argument [x], [id x] is [x].

    example:
    [List.init 3 id] is [[0; 1; 2]] *)

val const : 'a -> (_ -> 'a)
(** [const c] is a function that always returns the value [c]. For any
    argument [x], [(const c) x] is [c].

    example:
    [List.init 3 (const 0)] is [[0; 0; 0]]

    One important difference between [const x] and [fun _ -> x] in the above
    example is that [x] is evaluated only once, not three times. *)

val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
(** [flip f] reverses the argument order of the binary function
    [f]. For any arguments [x] and [y], [(flip f) x y] is [f y x].

    example:
    [List.map (flip (-) 2) [4; 6; 8]] is [[2; 4; 6]] *)

val on : ('a -> 'b) -> (('b -> 'b -> 'c) -> ('a -> 'a -> 'c))
(** [on f] is a function that takes binary function and adjusts its operands on
    the unary function [f].
    For any arguments [x] and [y], [(on f) op x y] is [op (f x) (f y)].

    example:
    [List.sort (on String.length compare) ["OCaml"; "hi"]] is [["hi"; "OCaml"]] *)

val negate : ('a -> bool) -> ('a -> bool)
(** [negate p] is the negation of the predicate function [p]. For any
    argument [x], [(negate p) x] is [not (p x)].

    example:
    [Fun.negate List.is_empty []] is [false] *)

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
