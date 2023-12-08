(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Ulysse Gérard, Thomas Refis, Tarides                    *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Shape

(** The result of reducing a shape and looking for its uid.content *)
type result =
  | Resolved of Uid.t (** Shape reduction succeeded and a uid was found *)
  | Resolved_alias of Uid.t list (** Reduction led to one or several aliases *)
  | Unresolved of t (** Result still contains [Comp_unit] terms *)
  | Approximated of Uid.t option (** Reduction failed *)
  | Internal_error_missing_uid
    (** Reduction succedded but no uid was found, this should never happen *)

val print_result : Format.formatter -> result -> unit

(** This function returns the shape corresponding to a given path. It requires a
    callback to find shapes in the environment. It is generally more useful to
    rely directly on the [Env.shape_of_path] function to get the shape
    associated with a given path. *)

(** The [Make] functor is used to generate a reduction function for
    shapes.

    It is parametrized by:
    - a function to load the shape of an external compilation unit
    - some fuel, which is used to bound recursion when dealing with recursive
      shapes introduced by recursive modules. (FTR: merlin currently uses a
      fuel of 10, which seems to be enough for most practical examples)
*)
module Make(_ : sig
    val fuel : int

    val read_unit_shape : unit_name:string -> t option
  end) : sig
  val reduce : Env.t -> t -> t

  (** Perform weak reduction and return the head's uid if any. If reduction was
    incomplete the partially reduced shape is returned. *)
  val reduce_for_uid : Env.t -> t -> result
end

(** [local_reduce] will not reduce shapes that require loading external
  compilation units. *)
val local_reduce : Env.t -> t -> t

(** [local_reduce_for_uid] will not reduce shapes that require loading external
  compilation units. *)
val local_reduce_for_uid : Env.t -> t -> result
