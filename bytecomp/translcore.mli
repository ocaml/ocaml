(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Asttypes
open Typedtree
open Lambda

val transl_exp: expression -> lambda
val transl_let:
      rec_flag -> (pattern * expression) list -> lambda -> lambda
val transl_primitive: Primitive.description -> lambda
val transl_exception: Ident.t -> exception_declaration -> lambda

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr

exception Error of Location.t * error

val report_error: error -> unit


