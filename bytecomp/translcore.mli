(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: translcore.mli 12959 2012-09-27 13:12:51Z maranget $ *)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Asttypes
open Typedtree
open Lambda

val name_pattern: string -> (pattern * 'a) list -> Ident.t

val transl_exp: expression -> lambda
val transl_apply: lambda -> (label * expression option * optional) list
                  -> Location.t -> lambda
val transl_let:
      rec_flag -> (pattern * expression) list -> lambda -> lambda
(*> JOCAML *)
val transl_def: joinautomaton list -> lambda -> lambda
(*< JOCAML *)
val transl_primitive: Primitive.description -> lambda
val transl_exception:
      Ident.t -> Path.t option -> exception_declaration -> lambda

val check_recursive_lambda: Ident.t list -> lambda -> bool

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr
  | Free_super_var
  | Unknown_builtin_primitive of string

exception Error of Location.t * error

open Format

val report_error: formatter -> error -> unit

(* Forward declaration -- to be filled in by Translmod.transl_module *)
val transl_module :
      (module_coercion -> Path.t option -> module_expr -> lambda) ref
val transl_object :
      (Ident.t -> string list -> class_expr -> lambda) ref
