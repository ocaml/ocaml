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
