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

(** Result values.

    Result values handle computation results and errors in an explicit
    and declarative manner without resorting to exceptions.

    @since 4.08 *)

(** {1:results Results} *)

type ('a, 'e) t = ('a, 'e) result = Ok of 'a | Error of 'e (**)
(** The type for result values. Either a value [Ok v] or an error [Error e]. *)

val ok : 'a -> ('a, 'e) result
(** [ok v] is [Ok v]. *)

val error : 'e -> ('a, 'e) result
(** [error e] is [Error e]. *)

val value : ('a, 'e) result -> default:'a -> 'a
(** [value r ~default] is [v] if [r] is [Ok v] and [default] otherwise. *)

val get_ok : ('a, 'e) result -> 'a
(** [get_ok r] is [v] if [r] is [Ok v] and raise otherwise.

    @raise Invalid_argument if [r] is [Error _]. *)

val get_error : ('a, 'e) result -> 'e
(** [get_error r] is [e] if [r] is [Error e] and raise otherwise.

    @raise Invalid_argument if [r] is [Ok _]. *)

val bind : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
(** [bind r f] is [f v] if [r] is [Ok v] and [r] if [r] is [Error _]. *)

val join : (('a, 'e) result, 'e) result -> ('a, 'e) result
(** [join rr] is [r] if [rr] is [Ok r] and [rr] if [rr] is [Error _]. *)

val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
(** [map f r] is [Ok (f v)] if [r] is [Ok v] and [r] if [r] is [Error _]. *)

val map_error : ('e -> 'f) -> ('a, 'e) result -> ('a, 'f) result
(** [map_error f r] is [Error (f e)] if [r] is [Error e] and [r] if
    [r] is [Ok _]. *)

val fold : ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) result -> 'c
(** [fold ~ok ~error r] is [ok v] if [r] is [Ok v] and [error e] if [r]
    is [Error e]. *)

val iter : ('a -> unit) -> ('a, 'e) result -> unit
(** [iter f r] is [f v] if [r] is [Ok v] and [()] otherwise. *)

val iter_error : ('e -> unit) -> ('a, 'e) result -> unit
(** [iter_error f r] is [f e] if [r] is [Error e] and [()] otherwise. *)

(** {1:preds Predicates and comparisons} *)

val is_ok : ('a, 'e) result -> bool
(** [is_ok r] is [true] if and only if [r] is [Ok _]. *)

val is_error : ('a, 'e) result -> bool
(** [is_error r] is [true] if and only if [r] is [Error _]. *)

val equal :
  ok:('a -> 'a -> bool) -> error:('e -> 'e -> bool) -> ('a, 'e) result ->
  ('a, 'e) result -> bool
(** [equal ~ok ~error r0 r1] tests equality of [r0] and [r1] using [ok]
    and [error] to respectively compare values wrapped by [Ok _] and
    [Error _]. *)

val compare :
  ok:('a -> 'a -> int) -> error:('e -> 'e -> int) -> ('a, 'e) result ->
  ('a, 'e) result -> int
(** [compare ~ok ~error r0 r1] totally orders [r0] and [r1] using [ok] and
    [error] to respectively compare values wrapped by [Ok _ ] and [Error _].
    [Ok _] values are smaller than [Error _] values. *)

(** {1:convert Converting} *)

val to_option : ('a, 'e) result -> 'a option
(** [to_option r] is [r] as an option, mapping [Ok v] to [Some v] and
    [Error _] to [None]. *)

val to_list : ('a, 'e) result -> 'a list
(** [to_list r] is [[v]] if [r] is [Ok v] and [[]] otherwise. *)

val to_seq : ('a, 'e) result -> 'a Seq.t
(** [to_seq r] is [r] as a sequence. [Ok v] is the singleton sequence
    containing [v] and [Error _] is the empty sequence. *)
