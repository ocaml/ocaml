(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml + CDuce                    *)
(*                                                                     *)
(*            Alain Frisch, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Typechecking for the CDuce extension *)

(* Errors *)

type error
exception Error of Location.t * error
val report_error: Format.formatter -> error -> unit

val register_exttypes: unit -> unit

(* CDuce type-checker *)

open Types
type t = Cduce_types.Types.t

val type_expression: 
  Env.t -> Location.t -> Parsetree.ext_exp -> 
  Typedtree.ext_exp * type_expr

val type_expect: 
  ?in_function:(Location.t * Types.type_expr) -> Env.t -> 
  Parsetree.expression -> type_expr -> Typedtree.expression

val annot: 
  Env.t -> Parsetree.expression -> Typedtree.expression -> Typedtree.expression

val flush_ext_annot: Env.t -> unit

val transl_ext_type: 
  Env.t -> Parsetree.ext_pattern -> type_expr

val transl_type_decl:
  Env.t -> (string * Parsetree.ext_pattern) list -> t list


(* Constraint solver *)

val solve: Env.t -> unit


(* Forward definitions *)

val real_type_expect: 
  (?in_function:(Location.t * Types.type_expr) -> Env.t -> 
    Parsetree.expression -> type_expr -> Typedtree.expression) ref

val transl_simple_type:
  (Env.t -> bool -> Parsetree.core_type -> Types.type_expr) option ref


val subtype_loc: Location.t ref
