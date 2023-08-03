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

    See {{!examples} the examples} below.

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

(** {1:examples Examples}

    {2 Combinators}

    {{!combinators}Combinators} provide a lightweight and sometimes more
    readable way to create anonymous functions, best used as short-lived
    arguments rather than standalone definitions. The examples below will
    demonstrate this mainly with the {!module:List} module.

    {!val:id}
    {[
    # List.init 3 Fun.id;;
    - : int list = [0; 1; 2]

    # List.filter_map Fun.id [None; Some 2; Some 3; None; Some 5];;
    - : int list = [2; 3; 5]

    # let to_flat = Float.Array.map_from_array Fun.id
    val to_flat : float array -> Float.Array.t
    ]}
    Dispatching functions of type [foo -> foo] conditionally is another place
    where [id] may be useful
    {[
    if Sys.win32 then String.map (function '\\' -> '/' | c -> c) else Fun.id
    ]}

    {!val:const}
    {[
    # List.init 3 (Fun.const 0);;
    - : int list = [0; 0; 0]

    # let last xs = List.fold_left (Fun.const Option.some) None xs;;
    val last : 'a list -> 'a option
    # last [1; 2; 3];;
    - : int option = Some 3
    # last [];;
    - : int option = None
    ]}
    Note that applying [const (...)] evaluates the expression [(...)] once, and
    returns a function that only has the result of this evaluation. To
    demonstrate this, consider if [(...)] was a call to {!val:Random.bool}[()]:

    [List.init n (Fun.const (Random.bool()))] for any [n > 0] will have
    {i exactly two} possible outcomes,
    - [[true; true; ...; true]] or
    - [[false; false; ...; false]].

    whereas [List.init n (fun _ -> Random.bool())] will have 2{^n} possible
    outcomes, because the randomness effect is performed with every element.

    {!val:flip}
    {[
    # List.sort (Fun.flip Int.compare) [5; 3; 9; 0; 1; 6; 8];;
    - : int list = [9; 8; 6; 5; 3; 1; 0]

    # let subtract = Fun.flip (-) in
      List.map (subtract 2) [4; 6; 8];;
    - : int list = [2; 4; 6]

    # List.fold_left (Fun.flip List.cons) [] [1; 2; 3];;
    - : int list = [3; 2; 1]
    ]}
    Thanks to currying, [flip] can work with functions that take more than two
    arguments, by flipping the first two and leaving the rest in order. Given a
    function [f : a -> b -> c -> d]:
    - [flip f m] will have type [a -> c -> d], whereas
    - [flip (f n)] will have type [c -> b -> d]

    {!val:negate}
    {[
    # List.find_all (Fun.negate List.is_empty) [[0]; [1; 2; 3]; []; [4; 5]];;
    - : int list list = [[0]; [1; 2; 3]; [4; 5]]

    # let is_free_path = Fun.negate Sys.file_exists
    val is_free_path : string -> bool = <fun>
    ]}
*)
