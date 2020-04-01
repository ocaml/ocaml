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
   for the module language *)

open Typedtree
open Lambda

val transl_implementation:
      string -> structure * module_coercion -> Lambda.program
val transl_store_phrases: string -> structure -> int * lambda
val transl_store_implementation:
      string -> structure * module_coercion -> Lambda.program

val transl_implementation_flambda:
  string -> structure * module_coercion -> Lambda.program

val transl_toplevel_definition: structure -> lambda
val transl_package:
      Ident.t option list -> Ident.t -> module_coercion -> lambda
val transl_store_package:
      Ident.t option list -> Ident.t -> module_coercion -> int * lambda

val transl_package_flambda:
      Ident.t option list -> module_coercion -> int * lambda

val toplevel_name: Ident.t -> string
val nat_toplevel_name: Ident.t -> Ident.t * int

val primitive_declarations: Primitive.description list ref

type unsafe_component =
  | Unsafe_module_binding
  | Unsafe_functor
  | Unsafe_non_function
  | Unsafe_typext

type unsafe_info =
  | Unsafe of { reason:unsafe_component; loc:Location.t; subid:Ident.t }
  | Unnamed

type error =
  Circular_dependency of (Ident.t * unsafe_info) list
| Conflicting_inline_attributes

exception Error of Location.t * error

val report_error: Location.t -> error -> Location.error

val reset: unit -> unit
