(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Partout, INRIA Saclay                        *)
(*   Nicolas Chataing, ENS Paris                                          *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Over-approximation of the possible values of a type,
   used to verify and compile unboxed head constructors.

   We consider the "head" of a value: the value itself if it is an
   immediate, or its tag and size if is a block. A head "shape" is a set of
   possible heads.

   The "head" of a value is an abstraction or approximation of the value,
   the "head shape" is an abstraction or approximation of a (closed) type.
*)

type imm = Imm of int [@@unboxed]
type tag = Tag of int [@@unboxed]

type head =
  | Immediate of imm
  | Block of tag

module ImmSet : Set.S with type elt = imm
module TagSet : Set.S with type elt = tag

module Or_any : sig
  type 'a t = Those of 'a | Any
  val mon_map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val mon_bind2 : ('a -> 'b -> 'c t) -> 'a t -> 'b t -> 'c t
end
type 'a or_any = 'a Or_any.t = Those of 'a | Any

type imm_set = ImmSet.t or_any
type block_set = TagSet.t or_any

type shape = {
  imms: imm_set; (* set of immediates the head can be *)
  blocks: block_set; (* set of block shapes the head can be *)
}

module Shape : sig
  type t = shape
  val empty : t
  val any : t

  val mem : head -> shape -> bool

  val union : t -> t -> t
  val inter : t -> t -> t
  val is_empty : t -> bool
  val is_any : t -> bool
  val subset : t -> t -> bool

  val any_immediate : t
  val imm : imm list -> t
  val block : tag list -> t
  val float : t
  val string : t
  val tuple : t
  val array : t
  val floatarray : t
  val \#function : t
  val \#object : t
  val \#lazy : t -> t
  val continuation : t
  val extensible_variant : t
  val polymorphic_variant : has_consts:bool -> has_nonconsts:bool -> t
  val abstract : t
  val custom : t
end

type unboxed_cstr_data = Types.type_declaration * Types.type_expr
type unboxed_cstr_description = (unboxed_cstr_data, shape) Misc.Cached.t
(* Remark on the life cycle of shape information.

   The [Data_types.constructor_description] data that contains
   [unboxed_cstr_description] information (in the [cstr_tag] field,
   case [Cstr_unboxed]) is created un-initialized by Datarepr, when
   entering a type declaration inside the current typing
   environment. We cannot compute the head-shape at this point,
   for two reasons:

   1. Env depends on Datarepr, so Datarepr functions cannot depend on Env.t.

   2. Shape computation may need to access mutually-recursive type
      declarations, which are not yet present in the environment at
      the time where the Datarepr module is called to compute datatype
      descriptions.

   Type declarations coming from the user code are "checked" after
   being entered in the environment by the Typedecl module; at this
   point the [Head_shape.check_typedecl] function below is called,
   and the [shape] information for their unboxed constructors is
   computed and cached at this point. Conflicts are turned into
   proper user-facing errors.

   However, the environment can also be extended by type
   declarations coming from other compilation units (signatures in
   .cmi files), and the head-shape information is not present or
   computed at this point -- we are still within Env, and cannot
   call [Head_shape.of_type*] without creating cyclic
   dependencies. Note that these type-declarations have already been
   checked when compiling their own module, so they must not contain
   head-shape conflicts. In this case a type declaration can leave
   the type-checking phase with its head shape information still
   un-initialized. It will be computed on-demand by other parts of
   the compiler that need the information, such as pattern-matching
   compilation.
*)
