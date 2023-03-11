(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Detection of partial matches and unused match cases. *)

open Asttypes
open Typedtree
open Types

val const_compare : constant -> constant -> int
(** [const_compare c1 c2] compares the actual values represented by [c1] and
    [c2], while simply using [Stdlib.compare] would compare the
    representations.

    cf. MPR#5758 *)

val le_pat : pattern -> pattern -> bool
(** [le_pat p q]  means: forall V,  V matches q implies V matches p *)

val le_pats : pattern list -> pattern list -> bool
(** [le_pats (p1 .. pm) (q1 .. qn)] means: forall i <= m, [le_pat pi qi] *)

(** Exported compatibility functor, abstracted over constructor equality *)
module Compat :
  functor
    (_ : sig
      val equal :
          Types.constructor_description ->
            Types.constructor_description ->
              bool
     end) -> sig
       val compat : pattern -> pattern -> bool
       val compats : pattern list -> pattern list -> bool
     end

exception Empty

val lub : pattern -> pattern -> pattern
(** [lub p q] is a pattern that matches all values matched by [p] and [q].
    May raise [Empty], when [p] and [q] are not compatible. *)

val lubs : pattern list -> pattern list -> pattern list
(** [lubs [p1; ...; pn] [q1; ...; qk]], where [n < k], is
    [[lub p1 q1; ...; lub pk qk]].  *)

val get_mins : ('a -> 'a -> bool) -> 'a list -> 'a list

(** Those two functions recombine one pattern and its arguments:
    For instance:
      (_,_)::p1::p2::rem -> (p1, p2)::rem
    The second one will replace mutable arguments by '_'
*)
val set_args : pattern -> pattern list -> pattern list
val set_args_erase_mutable : pattern -> pattern list -> pattern list

val pat_of_constr : pattern -> constructor_description -> pattern
val complete_constrs :
    constructor_description pattern_data ->
    constructor_description list ->
    constructor_description list

(** [pats_of_type] builds a list of patterns from a given expected type,
    for explosion of wildcard patterns in Typecore.type_pat.

    There are four interesting cases:
    - the type is empty ([])
    - no further explosion is necessary ([Pat_any])
    - a single pattern is generated, from a record or tuple type
      or a single-variant type ([tp])
    - a list of patterns, in the case that all branches
      are GADT constructors ([tp1; ..; tpn]).
 *)
val pats_of_type : Env.t -> type_expr -> pattern list

val pressure_variants:
  Env.t -> pattern list -> unit
val pressure_variants_in_computation_pattern:
  Env.t -> computation general_pattern list -> unit

(** [check_partial pred loc caselist] and [check_unused refute pred caselist]
    are called with a function [pred] which will be given counter-example
    candidates: they may be partially ill-typed, and have to be type-checked
    to extract a valid counter-example.
    [pred] returns a valid counter-example or [None].
    [refute] indicates that [check_unused] was called on a refutation clause.
 *)
val check_partial:
    (pattern -> pattern option) -> Location.t -> value case list -> partial
val check_unused:
    (bool -> pattern -> pattern option) -> value case list -> unit

(* Irrefutability tests *)
val irrefutable : pattern -> bool

(** An inactive pattern is a pattern, matching against which can be duplicated,
    erased or delayed without change in observable behavior of the program.
    Patterns containing (lazy _) subpatterns or reads of mutable fields are
    active. *)
val inactive : partial:partial -> pattern -> bool

(* Ambiguous bindings *)
val check_ambiguous_bindings : value case list -> unit

(* The tag used for open polymorphic variant types with an abstract row *)
val some_private_tag : label
