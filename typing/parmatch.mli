(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Detection of partial matches and unused match cases. *)
open Types
open Typedtree

val top_pretty : Format.formatter -> pattern -> unit
val pretty_pat : pattern -> unit
val pretty_line : pattern list -> unit
val pretty_matrix : pattern list list -> unit

val omega : pattern
val omegas : int -> pattern list
val omega_list : 'a list -> pattern list
val normalize_pat : pattern -> pattern
val all_record_args :
    (label_description * pattern) list -> (label_description * pattern) list

val le_pat : pattern -> pattern -> bool
val le_pats : pattern list -> pattern list -> bool
val compat : pattern -> pattern -> bool
val compats : pattern list -> pattern list -> bool
exception Empty
val lub : pattern -> pattern -> pattern
val lubs : pattern list -> pattern list -> pattern list

val get_mins : ('a -> 'a -> bool) -> 'a list -> 'a list

(* Those to functions recombine one pattern and its arguments:
   For instance:
     (_,_)::p1::p2::rem -> (p1, p2)::rem
   The second one will replace mutable arguments by '_'
*)
val set_args : pattern -> pattern list -> pattern list
val set_args_erase_mutable : pattern -> pattern list -> pattern list

val pat_of_constr : pattern -> constructor_description -> pattern
val complete_constrs :
    pattern -> constructor_tag list -> constructor_description  list

val pressure_variants: Env.t -> pattern list -> unit
val check_partial: Location.t -> (pattern * expression) list -> Env.t -> partial
val check_unused: Env.t -> (pattern * expression) list -> unit

(* Irrefutability tests *)
val irrefutable : pattern -> bool
val fluid : pattern -> bool
