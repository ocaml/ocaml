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

(** Boolean values.

    @since 4.08 *)

(** {1:bools Booleans} *)

type t = bool = false | true (**)
(** The type of booleans (truth values).

    The constructors [false] and [true] are included here so that they have
    paths, but they are not intended to be used in user-defined data types.
 *)

val not : bool -> bool
(** [not b] is the boolean negation of [b]. *)

external ( && ) : bool -> bool -> bool = "%sequand"
(** [e0 && e1] is the lazy boolean conjunction of expressions [e0] and [e1].
    If [e0] evaluates to [false], [e1] is not evaluated. Right-associative
    operator at precedence level 3/11. *)

external ( || ) : bool -> bool -> bool = "%sequor"
(** [e0 || e1] is the lazy boolean disjunction of expressions [e0] and [e1].
    If [e0] evaluates to [true], [e1] is not evaluated. Right-associative
    operator at precedence level 2/11. *)

(** {1:preds Predicates and comparisons} *)

val equal : bool -> bool -> bool
(** [equal b0 b1] is [true] if and only if [b0] and [b1] are both [true]
    or both [false]. *)

val compare : bool -> bool -> int
(** [compare b0 b1] is a total order on boolean values. [false] is smaller
    than [true]. *)

(** {1:convert Converting} *)

val to_int : bool -> int
(** [to_int b] is [0] if [b] is [false] and [1] if [b] is [true]. *)

val to_float : bool -> float
(** [to_float b] is [0.] if [b] is [false] and [1.] if [b] is [true]. *)

(*
val of_string : string -> bool option
(** [of_string s] is [Some true] if [s] is ["true"], [Some false] if [s]
    is ["false"] and [None] otherwise. *)
*)

val to_string : bool -> string
(** [to_string b] is ["true"] if [b] is [true] and ["false"] if [b] is
    [false]. *)

val seeded_hash : int -> bool -> int
(** A seeded hash function for booleans, with the same output value as
    {!Hashtbl.seeded_hash}. This function allows this module to be passed as
    argument to the functor {!Hashtbl.MakeSeeded}.

    @since 5.1 *)

val hash : bool -> int
(** An unseeded hash function for booleans, with the same output value as
    {!Hashtbl.hash}. This function allows this module to be passed as argument
    to the functor {!Hashtbl.Make}.

    @since 5.1 *)
