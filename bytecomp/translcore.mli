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

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Asttypes
open Types
open Typedtree
open Lambda

val name_pattern: string -> (pattern * 'a) list -> Ident.t

val transl_exp: expression -> lambda
val transl_apply: lambda -> (expression option * optional) list
                  -> Location.t -> lambda
val transl_let:
      rec_flag -> (pattern * expression) list -> lambda -> lambda
val transl_primitive: Primitive.description -> lambda
val transl_exception:
      Ident.t -> Path.t option -> exception_declaration -> lambda

val check_recursive_lambda: Ident.t list -> lambda -> bool

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr
  | Free_super_var
  | Illegal_tuple_expr
  | Illegal_contracted_expr

exception Error of Location.t * error

open Format

val report_error: formatter -> error -> unit

(* Forward declaration -- to be filled in by Translmod.transl_module *)
val transl_module :
      (module_coercion -> Path.t option -> module_expr -> lambda) ref
val transl_object :
      (Ident.t -> string list -> class_expr -> lambda) ref

(* Translate wrapped expression e |><| c to unwrapped expression 
val transl_contract: Typedtree.core_contract -> 
                     Typedtree.expression -> 
                     Typedtree.expression -> Typedtree.expression -> 
                     Typedtree.expression 
*)
val contract_id_in_expr :
           Typedtree.core_contract Ident.tbl ->
           Typedtree.contract_declaration list ->
           (Path.t * Types.contract_declaration) Ident.tbl ->
           Path.t option -> Typedtree.expression -> Typedtree.expression

val contract_id_in_contract :
           Typedtree.core_contract Ident.tbl ->
           Typedtree.contract_declaration list ->
           (Path.t * Types.contract_declaration) Ident.tbl ->
           Path.t option ->
           Typedtree.core_contract -> Typedtree.core_contract

val deep_transl_contract: Typedtree.expression -> Typedtree.expression 
