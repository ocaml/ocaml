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
(* val of_path :
  find_shape:(Sig_component_kind.t -> Ident.t -> t) ->
  namespace:Sig_component_kind.t -> Path.t -> t *)

(** The [Make] functor is used to generate a reduction function for
    shapes.

    It is parametrized by:
    - an environment and a function to find shapes by path in that environment
    - a function to load the shape of an external compilation unit
    - some fuel, which is used to bound recursion when dealing with recursive
      shapes introduced by recursive modules. (FTR: merlin currently uses a
      fuel of 10, which seems to be enough for most practical examples)
*)
module Make(Context : sig
    type env

    val fuel : int

    val read_unit_shape : unit_name:string -> t option

    val find_shape : env -> Ident.t -> t
  end) : sig
  val reduce : Context.env -> t -> t

  (** Perform weak reduction and return the head's uid if any. If reduction was
    incomplete the partially reduced shape is returned. *)
  val reduce_for_uid : Context.env -> t -> result
end

(** [toplevel_local_reduce] is only suitable to reduce toplevel shapes (shapes
  of compilation units). Use the [Make] functor for other cases that
  require access to the environment.*)
val toplevel_local_reduce : t -> t
