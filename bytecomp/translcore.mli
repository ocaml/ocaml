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

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Asttypes
open Typedtree
open Lambda

val transl_exp: expression -> lambda
val transl_apply: ?should_be_tailcall:bool
                  -> ?inlined:inline_attribute
                  -> ?specialised:specialise_attribute
                  -> lambda -> (arg_label * expression option) list
                  -> Location.t -> lambda
val transl_let: rec_flag -> value_binding list -> lambda -> lambda
val transl_primitive: Location.t -> Primitive.description -> Env.t
                      -> Types.type_expr -> Path.t option -> lambda

val check_recursive_lambda: Ident.t list -> lambda -> bool

val used_primitives: (Path.t, Location.t) Hashtbl.t

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr
  | Free_super_var
  | Unknown_builtin_primitive of string
  | Unreachable_reached

exception Error of Location.t * error

open Format

val report_error: formatter -> error -> unit

(* Forward declaration -- to be filled in by Translmod.transl_module *)
val transl_module :
      (module_coercion -> Path.t option -> module_expr -> lambda) ref
val transl_object :
      (Ident.t -> string list -> class_expr -> lambda) ref
