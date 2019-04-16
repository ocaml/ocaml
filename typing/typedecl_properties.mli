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

type decl = Types.type_declaration

(** An abstract interface for properties of type definitions, such as
   variance and immediacy, that are computed by a fixpoint on
   mutually-recursive type declarations. This interface contains all
   the operations needed to initialize and run the fixpoint
   computation, and then (optionally) check that the result is
   consistent with the declaration or user expectations. *)

type ('prop, 'req) property = {
  eq : 'prop -> 'prop -> bool;
  merge : prop:'prop -> new_prop:'prop -> 'prop;

  default : decl -> 'prop;
  compute : Env.t -> decl -> 'req -> 'prop;
  update_decl : decl -> 'prop -> decl;

  check : Env.t -> Ident.t -> decl -> 'req -> unit;
}
(** ['prop] represents the type of property values
    ({!Types.Variance.t}, just 'bool' for immediacy, etc).

    ['req] represents the property value required by the author of the
    declaration, if they gave an expectation: [type +'a t = ...].

    Some properties have no natural notion of user requirement, or
    their requirement is global, or already stored in
    [type_declaration]; they can just use [unit] as ['req] parameter. *)


(** [compute_property prop env decls req] performs a fixpoint computation
    to determine the final values of a property on a set of mutually-recursive
    type declarations. The [req] argument must be a list of the same size as
    [decls], providing the user requirement for each declaration. *)
val compute_property : ('prop, 'req) property -> Env.t ->
  (Ident.t * decl) list -> 'req list -> (Ident.t * decl) list

val compute_property_noreq : ('prop, unit) property -> Env.t ->
  (Ident.t * decl) list -> (Ident.t * decl) list
