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

type error = Bad_immediacy_attribute of Type_immediacy.Violation.t
exception Error of Location.t * error

val compute_decl : Env.t -> Types.type_declaration -> Type_immediacy.t

val property : (Type_immediacy.t, unit) Typedecl_properties.property

val update_decls :
  Env.t ->
  (Ident.t * Typedecl_properties.decl) list ->
  (Ident.t * Typedecl_properties.decl) list
