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

(** Shapes are an abstract representation of modules' implementations which
    allow the tracking of definitions through functor applications and other
    module-level operations.

    The Shape of a compilation unit is elaborated during typing, partially
    reduced (without loading external shapes) and written to the [cmt] file.

    External tools can retrieve the definition of any value (or type, or module,
    etc) by following this procedure:

    - Build the Shape corresponding to the value's path:
      [let shape = Env.shape_of_path ~namespace env path]

    - Instantiate the [Make_reduce] functor with a way to load shapes from
      external units and to looks for shapes in the environment (usually using
      [Env.shape_of_path]).

    - Completely reduce the shape:
      [let shape = My_reduce.(weak_)reduce env shape]

    - The [Uid.t] stored in the reduced shape should be the one of the
      definition. However, if the [approximate] field of the reduced shape is
      [true] then the [Uid.t] will not correspond to the definition, but to the
      closest parent module's uid. This happens when Shape reduction gets stuck,
      for example when hitting first-class modules.

    - The location of the definition can be easily found with the
      [cmt_format.cmt_uid_to_decl] talbe of the corresponding compilation unit.

  See:
  - {{: https://icfp22.sigplan.org/details/mlfamilyworkshop-2022-papers/10/Module-Shapes-for-Modern-Tooling }
    the design document}
  - {{: https://www.lix.polytechnique.fr/Labo/Gabriel.Scherer/research/shapes/2022-ml-workshop-shapes-talk.pdf }
    a talk about the reduction strategy
*)

module Uid : sig
  type t = private
    | Compilation_unit of string
    | Item of { comp_unit: string; id: int }
    | Internal
    | Predef of string

  val reinit : unit -> unit

  val mk : current_unit:string -> t
  val of_compilation_unit_id : Ident.t -> t
  val of_predef_id : Ident.t -> t
  val internal_not_actually_unique : t

  val for_actual_declaration : t -> bool

  include Identifiable.S with type t := t
end

module Sig_component_kind : sig
  type t =
    | Value
    | Type
    | Constructor
    | Label
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  val to_string : t -> string

  (** Whether the name of a component of that kind can appear in a type. *)
  val can_appear_in_types : t -> bool
end

(** Shape's items are elements of a structure modeling module components. *)
module Item : sig
  type t = string * Sig_component_kind.t
  val name : t -> string
  val kind : t -> Sig_component_kind.t

  val make : string -> Sig_component_kind.t -> t

  val value : Ident.t -> t
  val type_ : Ident.t -> t
  val constr : Ident.t -> t
  val label : Ident.t -> t
  val module_ : Ident.t -> t
  val module_type : Ident.t -> t
  val extension_constructor : Ident.t -> t
  val class_ : Ident.t -> t
  val class_type : Ident.t -> t

  val print : Format.formatter -> t -> unit

  module Map : Map.S with type key = t
end

type var = Ident.t
type t = { uid: Uid.t option; desc: desc; approximated: bool }
and desc =
  | Var of var
  | Abs of var * t
  | App of t * t
  | Struct of t Item.Map.t
  | Alias of t
  | Leaf
  | Proj of t * Item.t
  | Comp_unit of string
  | Error of string

(** The result of reducing a shape and looking for its uid.content *)
type reduction_result =
  | Resolved of Uid.t (** Shape reduction succeeded and a uid was found *)
  | Resolved_alias of Uid.t list (** Reduction led to one or several aliases *)
  | Unresolved of t (** Result still contains [Comp_unit] terms *)
  | Approximated of Uid.t option (** Reduction failed *)
  | Internal_error_missing_uid
    (** Reduction succedded but no uid was found, this should never happen *)

val print_reduction_result : Format.formatter -> reduction_result -> unit

val print : Format.formatter -> t -> unit

val strip_head_aliases : t -> t

(* Smart constructors *)

val for_unnamed_functor_param : var
val fresh_var : ?name:string -> Uid.t -> var * t

val var : Uid.t -> Ident.t -> t
val abs : ?uid:Uid.t -> var -> t -> t
val app : ?uid:Uid.t -> t -> arg:t -> t
val str : ?uid:Uid.t -> t Item.Map.t -> t
val alias : ?uid:Uid.t -> t -> t
val proj : ?uid:Uid.t -> t -> Item.t -> t
val leaf : Uid.t -> t

val decompose_abs : t -> (var * t) option

val for_persistent_unit : string -> t
val leaf_for_unpack : t

module Map : sig
  type shape = t
  type nonrec t = t Item.Map.t

  val empty : t

  val add : t -> Item.t -> shape -> t

  val add_value : t -> Ident.t -> Uid.t -> t
  val add_value_proj : t -> Ident.t -> shape -> t

  val add_type : t -> Ident.t -> shape -> t
  val add_type_proj : t -> Ident.t -> shape -> t

  val add_constr : t -> Ident.t -> shape -> t
  val add_constr_proj : t -> Ident.t -> shape -> t

  val add_label : t -> Ident.t -> Uid.t -> t
  val add_label_proj : t -> Ident.t -> shape -> t

  val add_module : t -> Ident.t -> shape -> t
  val add_module_proj : t -> Ident.t -> shape -> t

  val add_module_type : t -> Ident.t -> Uid.t -> t
  val add_module_type_proj : t -> Ident.t -> shape -> t

  val add_extcons : t -> Ident.t -> shape -> t
  val add_extcons_proj : t -> Ident.t -> shape -> t

  val add_class : t -> Ident.t -> Uid.t -> t
  val add_class_proj : t -> Ident.t -> shape -> t

  val add_class_type : t -> Ident.t -> Uid.t -> t
  val add_class_type_proj : t -> Ident.t -> shape -> t
end

val dummy_mod : t

(** This function returns the shape corresponding to a given path. It requires a
    callback to find shapes in the environment. It is generally more useful to
    rely directly on the [Env.shape_of_path] function to get the shape
    associated with a given path. *)
val of_path :
  find_shape:(Sig_component_kind.t -> Ident.t -> t) ->
  namespace:Sig_component_kind.t -> Path.t -> t

val set_uid_if_none : t -> Uid.t -> t

(** The [Make_reduce] functor is used to generate a reduction function for
    shapes.

    It is parametrized by:
    - an environment and a function to find shapes by path in that environment
    - a function to load the shape of an external compilation unit
    - some fuel, which is used to bound recursion when dealing with recursive
      shapes introduced by recursive modules. (FTR: merlin currently uses a
      fuel of 10, which seems to be enough for most practical examples)
*)
module Make_reduce(Context : sig
    type env

    val fuel : int

    val read_unit_shape : unit_name:string -> t option

    val find_shape : env -> Ident.t -> t
  end) : sig
  val reduce : Context.env -> t -> t

  (** Perform weak reduction and return the head's uid if any. If reduction was
    incomplete the partially reduced shape is returned. *)
  val reduce_for_uid : Context.env -> t -> reduction_result
end

(** [toplevel_local_reduce] is only suitable to reduce toplevel shapes (shapes
  of compilation units). Use the [Make_reduce] functor for other cases that
  require access to the environment.*)
val toplevel_local_reduce : t -> t
