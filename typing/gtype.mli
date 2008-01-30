(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                  Jun Furuse, University of Tokyo                    *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Types
open Typedtree

(* anti-unification *)
val anti_unify :
  Env.t -> type_expr -> type_expr -> type_expr
val anti_unify_types : Env.t -> type_expr list -> type_expr

val normalize_konst_type :  konstraint -> type_expr -> konstraint

exception Not_generic_primitive_type
val type_of_generic_primitive_compilation :
  Env.t -> konstraint -> type_expr -> type_expr
(*
val type_abstraction : konstraint -> type_expr -> type_expr list
val type_abstraction_of_value : value_description -> type_expr list
*)
val ident_of_type_variable : type_expr -> Ident.t
val find_ident_of_type_variable : type_expr -> Ident.t

exception Not_instance of type_expr * type_expr
val is_instance : Env.t -> type_expr -> type_expr -> flow
val resolve_konstraint : Env.t -> konstraint -> flow_record

val make_toverload : Env.t -> type_expr list -> type_expr

val print_flow : Format.formatter -> flow -> unit
val print_flow_record : Format.formatter -> flow_record -> unit

val index_of_flow_record : Env.t -> konst_elem -> type_expr -> int
