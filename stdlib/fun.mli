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

val id : 'a -> 'a
(** [id] is the identity function. For any argument [x], [id x] is [x]. *)

val const : 'a -> (_ -> 'a)
(** [const c] is a function that always returns the value [c]. For any
    argument [x], [(const c) x] is [c]. *)

val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** [compose f g] is a function composition of applying [g] then [f].
    For any arguments [f], [g], and [x], [compose f g x] is [f (g x)].

    @since 5.2 *)

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

{{!combinators}Combinators} provide a lightweight and sometimes more readable
way to create anonymous functions, best used as short-lived arguments rather
than standalone definitions. The examples below will demonstrate this mainly
with the {!module:List} module.


{3:hid {{!val:id}id}}

{!val:List.init} with the index itself
{[
  # List.init 3 Fun.id;;
  - : int list = [0; 1; 2]
]}

Using {!val:List.filter_map} on an [int option list] to filter out
{{!const:Option.t.None}[None]} elements
{[
  # List.filter_map Fun.id [None; Some 2; Some 3; None; Some 5];;
  - : int list = [2; 3; 5]
]}

Conditionally dispatching functions of type [foo -> foo] or taking them as
arguments is another place where [id] may be useful. Consider a primitive
logging function which prints a string but gives its user the option to
preformat the string before printing, e.g. to insert a time-stamp
{[
  let log ?(preformat : string -> string = Fun.id) message =
    print_endline (preformat message)
]}
Whenever we may build up closures, [id] is often used for the base-case as a
no-op. Consider a function which chains a list of unary functions:
{[
  let rec chain = function
    | [] -> Fun.id
    | f :: fs -> fun x -> f (chain fs x)
]}


{3:hconst {{!val:const}const}}

{!val:List.init} a list of zeros
{[
  # List.init 3 (Fun.const 0);;
  - : int list = [0; 0; 0]
]}

An allow-all predicate that could be passed to any filtering function e.g.
{!val:List.filter} to disable filtration and get back all values
{[
  # List.filter (Fun.const true) [1; 2; 3];;
  - : int list = [1; 2; 3]
]}

Note that applying [const (...)] evaluates the expression [(...)] once, and
returns a function that only has the result of this evaluation. To demonstrate
this, consider if [(...)] was a call to {!val:Random.bool}[()]:

[List.init n (Fun.const (Random.bool()))] for any [n > 0] will have {e exactly
two} possible outcomes,
{ul {- [[true; true; ...; true]]} {- [[false; false; ...; false]]}}
whereas [List.init n (fun _ -> Random.bool())] will have 2{^n} possible
outcomes, because the randomness effect is performed with every element.

For more real-world uses, consider {!val:String.spellcheck} with a constant max
distance of 2, instead of the default variable max distance
{[
  let spellcheck known_words word =
    let dict_iter yield = List.iter yield known_words in
    String.spellcheck ~max_dist:(Fun.const 2) dict_iter word
]}


{3:hflip {{!val:flip}flip}}

Useing [flip] to reverse the comparator passed to {!val:List.sort}, which sorts
in the opposite order
{[
  # List.sort (Fun.flip Int.compare) [5; 3; 9; 0; 1; 6; 8];;
  - : int list = [9; 8; 6; 5; 3; 1; 0]
]}

Reversing a list by accumulating a new list using {!val:List.fold_left}, which
expects the accumulator to be the first argument of the function passed to it.
We pass {!val:List.cons} which has the list as the second argument, so [flip]
is useful here
{[
  # List.fold_left (Fun.flip List.cons) [] [1; 2; 3];;
  - : int list = [3; 2; 1]
]}

Interestingly, [flip] can work with functions that aren't binary, by flipping
the first two arguments and leaving the rest in order. This is because a
function that takes [n+2] arguments is, conceptually, a binary function which
returns a function that takes [n] arguments.
Given a function [f : a -> b -> c -> d]:
{ul
{- [flip f] will have type [b -> a -> c -> d]}
{- [fun x -> flip (f x)] will have type [a -> c -> b -> d]}}
Using [flip] with non-binary functions like this is discouraged, for its
negative impact on readability and reasoning.


{3:hnegate {{!val:negate}negate}}

Mainly used for reversing a predicate in a function which expects one, like
{!val:List.find_all} and similar functions

Finding all lists which are {e not} empty using {!val:List.is_empty}
{[
  # List.find_all (Fun.negate List.is_empty) [[0]; [1; 2; 3]; []; [4; 5]];;
  - : int list list = [[0]; [1; 2; 3]; [4; 5]]
]}

From a given list of paths, finding all paths which are {e not} occupied using
{!val:Sys.file_exists}
{[
  # List.find_all (Fun.negate Sys.file_exists);;
  - : string list -> string list = <fun>
]}


{3:hcompose {{!val:compose}compose}}

{!val:List.map} on pair elements with a function on the second element
{[
  # List.map (Fun.compose String.length snd) [1, "one"; 2, "two"; 3, "three"];;
  - : int list = [3; 3; 5]
]}

A potential implementation of {!val:negate}
{[
  let negate f = Fun.compose not f
]}

From the {{!hid}[chain] example}, [compose] could have been used in the
recursive branch
{[
  let rec chain = function
    | [] -> Fun.id
    | f :: fs -> Fun.compose f (chain fs)
]}
Or even more concisely
{[
  let chain fs = List.fold_right Fun.compose fs Fun.id
]}

From the {{!hflip}[spellcheck] example}, [compose] and [flip] could be used to
condense the function definition so it becomes
{[
  # Fun.compose
      (String.spellcheck ~max_dist:(Fun.const 2))
      (Fun.flip List.iter)
    ;;
  - : string list -> string -> string list = <fun>
]}
As can be seen here, this heavily impacts readability and the ability to reason
about the function. Both [String.spellcheck] and [Fun.flip] are not unary, so
there is a non-trivial interaction with partial-application in this definition.

Heavy use of these combinators in OCaml is generally discouraged, not only
because they can quickly impact readability and reasoning, but also because the
produced functions are often in value form, thus subject to the Value
Restriction (see the manual section 6.1.2).
*)
