(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Typechecking of type expressions for the core language *)

val transl_simple_type:
        Env.t -> bool -> Parsetree.core_type -> Typedtree.type_expr
val transl_type_scheme:
        Env.t -> Parsetree.core_type -> Typedtree.type_expr
val reset_type_variables: unit -> unit
val enter_type_variable: string -> Typedtree.type_expr
val type_variable : Location.t -> string -> Typedtree.type_expr
val type_variable_list : unit -> Typedtree.type_expr list

exception Already_bound

type error =
    Unbound_type_variable of string
  | Unbound_type_constructor of Longident.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Unbound_class of Longident.t
  | Unbound_row_variable of Longident.t
  | Type_mismatch of Typedtree.type_expr * Typedtree.type_expr

exception Error of Location.t * error

val report_error: error -> unit
