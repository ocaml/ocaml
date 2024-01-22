(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Ulysse Gérard, Thomas Refis, Tarides                   *)
(*                    Nathanaëlle Courant, OCamlPro                       *)
(*              Gabriel Scherer, projet Picube, INRIA Paris               *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The result of reducing a shape and looking for its uid *)
type result =
  | Resolved of Shape.Uid.t (** Shape reduction succeeded and a uid was found *)
  | Resolved_alias of Shape.Uid.t list (** Reduction led to an alias chain *)
  | Unresolved of Shape.t (** Result still contains [Comp_unit] terms *)
  | Approximated of Shape.Uid.t option
    (** Reduction failed: it can arrive with first-clsss modules for example *)
  | Internal_error_missing_uid
    (** Reduction succeeded but no uid was found, this should never happen *)

val print_result : Format.formatter -> result -> unit

(** The [Make] functor is used to generate a reduction function for
    shapes.

    It is parametrized by:
    - a function to load the shape of an external compilation unit
    - some fuel, which is used to bound recursion when dealing with recursive
      shapes introduced by recursive modules. (FTR: merlin currently uses a
      fuel of 10, which seems to be enough for most practical examples)

    Usage warning: To ensure good performances, every reduction made with the
    same instance of that functor share the same ident-based memoization tables.
    Such an instance should only be used to perform reduction inside a unique
    compilation unit to prevent conflicting entries in these memoization tables.
*)
module Make(_ : sig
    val fuel : int

    val read_unit_shape : unit_name:string -> Shape.t option
  end) : sig
  val reduce : Env.t -> Shape.t -> Shape.t

  (** Perform weak reduction and return the head's uid if any. If reduction was
    incomplete the partially reduced shape is returned. *)
  val reduce_for_uid : Env.t -> Shape.t -> result
end

(** [local_reduce] will not reduce shapes that require loading external
  compilation units. *)
val local_reduce : Env.t -> Shape.t -> Shape.t

(** [local_reduce_for_uid] will not reduce shapes that require loading external
  compilation units. *)
val local_reduce_for_uid : Env.t -> Shape.t -> result
