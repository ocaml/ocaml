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

(* Detection of partial matches and unused match cases. *)
open Asttypes
open Typedtree
open Types

val pretty_const : constant -> string
val top_pretty : Format.formatter -> pattern -> unit
val pretty_pat : pattern -> unit
val pretty_line : pattern list -> unit
val pretty_matrix : pattern list list -> unit

val omega : pattern
val omegas : int -> pattern list
val omega_list : 'a list -> pattern list
val normalize_pat : pattern -> pattern
val all_record_args :
    (Longident.t loc * label_description * pattern) list ->
    (Longident.t loc * label_description * pattern) list
val const_compare : constant -> constant -> int

val le_pat : pattern -> pattern -> bool
val le_pats : pattern list -> pattern list -> bool
val compat : pattern -> pattern -> bool
val compats : pattern list -> pattern list -> bool
exception Empty
val lub : pattern -> pattern -> pattern
val lubs : pattern list -> pattern list -> pattern list

val get_mins : ('a -> 'a -> bool) -> 'a list -> 'a list

(* Those two functions recombine one pattern and its arguments:
   For instance:
     (_,_)::p1::p2::rem -> (p1, p2)::rem
   The second one will replace mutable arguments by '_'
*)
val set_args : pattern -> pattern list -> pattern list
val set_args_erase_mutable : pattern -> pattern list -> pattern list

val pat_of_constr : pattern -> constructor_description -> pattern
val complete_constrs :
    pattern -> constructor_tag list -> constructor_description  list
val ppat_of_type :
    Env.t -> type_expr ->
    Parsetree.pattern *
    (string, constructor_description) Hashtbl.t *
    (string, label_description) Hashtbl.t

val pressure_variants: Env.t -> pattern list -> unit
val check_partial_gadt:
    ((string, constructor_description) Hashtbl.t ->
     (string, label_description) Hashtbl.t ->
     Parsetree.pattern -> pattern option) ->
    Location.t -> case list -> partial
val check_unused:
    (bool ->
     (string, constructor_description) Hashtbl.t ->
     (string, label_description) Hashtbl.t ->
     Parsetree.pattern -> pattern option) ->
    Env.t -> case list -> unit

(* Irrefutability tests *)
val irrefutable : pattern -> bool
val fluid : pattern -> bool

(* Ambiguous bindings *)
val check_ambiguous_bindings : case list -> unit
