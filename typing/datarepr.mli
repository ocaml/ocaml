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

(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Asttypes
open Typedtree

val constructor_descrs:
  type_expr -> (string * type_expr list) list ->
    (string * constructor_description) list
val exception_descr:
  Path.t -> type_expr list -> constructor_description
val label_descrs:
  type_expr -> (string * mutable_flag * type_expr) list ->
    (string * label_description) list
