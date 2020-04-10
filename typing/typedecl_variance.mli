(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Parsifal, INRIA Saclay                       *)
(*   Rodolphe Lepigre, projet Deducteam, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Types
open Typedecl_properties

type surface_variance = bool * bool * bool

val variance_of_params :
  (Parsetree.core_type * Asttypes.variance) list -> surface_variance list
val variance_of_sdecl :
  Parsetree.type_declaration -> surface_variance list

type prop = Variance.t list
type req = surface_variance list
val property : (Variance.t list, req) property

type error =
| Bad_variance of int * surface_variance * surface_variance
| Varying_anonymous

exception Error of Location.t * error

val check_variance_extension :
  Env.t -> type_declaration ->
  Typedtree.extension_constructor -> req * Location.t -> unit

val compute_decl :
  Env.t -> check:bool -> type_declaration -> req -> prop

val update_decls :
  Env.t -> Parsetree.type_declaration list ->
  (Ident.t * type_declaration) list ->
  (Ident.t * type_declaration) list

val update_class_decls :
  Env.t ->
  (Ident.t * Typedecl_properties.decl * Types.type_declaration *
   Types.class_declaration * Types.class_type_declaration *
   'a Typedtree.class_infos) list ->
  (Typedecl_properties.decl * Types.type_declaration *
   Types.class_declaration * Types.class_type_declaration) list
(* FIXME: improve this horrible interface *)
