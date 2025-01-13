(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Gabriel Scherer, projet Parsifal, INRIA Saclay                 *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Either type.

    Either is the simplest and most generic sum/variant type:
    a value of [('a, 'b) Either.t] is either a [Left (v : 'a)]
    or a [Right (v : 'b)].

    It is a natural choice in the API of generic functions where values
    could fall in two different cases, possibly at different types,
    without assigning a specific meaning to what each case should be.

    For example:

{[List.partition_map:
    ('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list]}

    If you are looking for a parametrized type where
    one alternative means success and the other means failure,
    you should use the more specific type {!Result.t}.

    @since 4.12
*)

(* Unlike [result], no [either] type is made available in Stdlib,
   one needs to access [Either.t] explicitly:

   - This type is less common in typical OCaml codebases,
     which prefer domain-specific variant types whose constructors
     carry more meaning.
   - Adding this to Stdlib would raise warnings in existing codebases
     that already use a constructor named Left or Right:
     + when opening a module that exports such a name,
       warning 45 is raised
     + adding a second constructor of the same name in scope kicks
       in the disambiguation mechanisms, and warning 41 may now
       be raised by existing code.

   If the use becomes more common in the future we can always
   revisit this choice.
*)

type ('a, 'b) t = Left of 'a | Right of 'b (**)
(** A value of [('a, 'b) Either.t] contains
    either a value of ['a]  or a value of ['b] *)

val left : 'a -> ('a, 'b) t
(** [left v] is [Left v]. *)

val right : 'b -> ('a, 'b) t
(** [right v] is [Right v]. *)

val is_left : ('a, 'b) t -> bool
(** [is_left (Left v)] is [true], [is_left (Right v)] is [false]. *)

val is_right : ('a, 'b) t -> bool
(** [is_right (Left v)] is [false], [is_right (Right v)] is [true]. *)

val find_left : ('a, 'b) t -> 'a option
(** [find_left (Left v)] is [Some v], [find_left (Right _)] is [None] *)

val find_right : ('a, 'b) t -> 'b option
(** [find_right (Right v)] is [Some v], [find_right (Left _)] is [None] *)

val map_left : ('a1 -> 'a2) -> ('a1, 'b) t -> ('a2, 'b) t
(** [map_left f e] is [Left (f v)] if [e] is [Left v]
    and [e] if [e] is [Right _]. *)

val map_right : ('b1 -> 'b2) -> ('a, 'b1) t -> ('a, 'b2) t
(** [map_right f e] is [Right (f v)] if [e] is [Right v]
    and [e] if [e] is [Left _]. *)

val map :
  left:('a1 -> 'a2) -> right:('b1 -> 'b2) -> ('a1, 'b1) t -> ('a2, 'b2) t
(** [map ~left ~right (Left v)] is [Left (left v)],
    [map ~left ~right (Right v)] is [Right (right v)]. *)

val fold : left:('a -> 'c) -> right:('b -> 'c) -> ('a, 'b) t -> 'c
(** [fold ~left ~right (Left v)] is [left v], and
    [fold ~left ~right (Right v)] is [right v]. *)

val retract : ('a, 'a) t -> 'a
(** [retract (Left v)] is [v], and [retract (Right v)] is [v].

    @since 5.4 *)

val iter : left:('a -> unit) -> right:('b -> unit) -> ('a, 'b) t -> unit
(** [iter ~left ~right (Left v)] is [left v], and
    [iter ~left ~right (Right v)] is [right v]. *)

val for_all : left:('a -> bool) -> right:('b -> bool) -> ('a, 'b) t -> bool
(** [for_all ~left ~right (Left v)] is [left v], and
    [for_all ~left ~right (Right v)] is [right v]. *)

val equal :
  left:('a -> 'a -> bool) -> right:('b -> 'b -> bool) ->
  ('a, 'b) t -> ('a, 'b) t -> bool
(** [equal ~left ~right e0 e1] tests equality of [e0] and [e1] using [left]
    and [right] to respectively compare values wrapped by [Left _] and
    [Right _]. *)

val compare :
  left:('a -> 'a -> int) -> right:('b -> 'b -> int) ->
  ('a, 'b) t -> ('a, 'b) t -> int
(** [compare ~left ~right e0 e1] totally orders [e0] and [e1] using [left] and
    [right] to respectively compare values wrapped by [Left _ ] and [Right _].
    [Left _] values are smaller than [Right _] values. *)
