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

val omega : pattern
(** aka. "Tpat_any" or "_"  *)

val omegas : int -> pattern list
(** [List.init (fun _ -> omega)] *)

val omega_list : 'a list -> pattern list
(** [List.map (fun _ -> omega)] *)

val normalize_pat : pattern -> pattern
(** Keep only the "head" of a pattern: all arguments are replaced by [omega], so
    are variables. *)

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
    (Constr: sig
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
    pattern -> constructor_tag list -> constructor_description  list

(** [ppat_of_type] builds an untyped or-pattern from its expected type.
     May raise [Empty] when [type_expr] is an empty variant *)
val ppat_of_type :
    Env.t -> type_expr ->
    Parsetree.pattern *
    (string, constructor_description) Hashtbl.t *
    (string, label_description) Hashtbl.t

val pressure_variants: Env.t -> pattern list -> unit
val check_partial:
    ((string, constructor_description) Hashtbl.t ->
     (string, label_description) Hashtbl.t ->
     Parsetree.pattern -> pattern option) ->
    Location.t -> case list -> partial
val check_unused:
    (bool ->
     (string, constructor_description) Hashtbl.t ->
     (string, label_description) Hashtbl.t ->
     Parsetree.pattern -> pattern option) ->
    case list -> unit

(* Irrefutability tests *)
val irrefutable : pattern -> bool

(** An inactive pattern is a pattern, matching against which can be duplicated,
    erased or delayed without change in observable behavior of the program.
    Patterns containing (lazy _) subpatterns or reads of mutable fields are
    active. *)
val inactive : partial:partial -> pattern -> bool

(* Ambiguous bindings *)
val check_ambiguous_bindings : case list -> unit

(* The tag used for open polymorphic variant types with an abstract row *)
val some_private_tag : label
