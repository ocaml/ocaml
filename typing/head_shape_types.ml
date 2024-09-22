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

type imm = Imm of int [@@unboxed]
type tag = Tag of int [@@unboxed]

type head =
  | Immediate of imm
  | Block of tag

module ImmSet = Set.Make(struct type t = imm let compare = Stdlib.compare end)
module TagSet = Set.Make(struct type t = tag let compare = Stdlib.compare end)

module Or_any = struct
  type 'a t = Those of 'a | Any

  let mon_bind2 f a b =
    match a, b with
    | Any, _ | _, Any -> Any
    | Those va, Those vb -> f va vb

  let mon_map2 f a b =
    mon_bind2 (fun va vb -> Those (f va vb)) a b
end
type 'a or_any = 'a Or_any.t = Those of 'a | Any

type imm_set = ImmSet.t or_any
type block_set = TagSet.t or_any

type shape = {
  imms: imm_set;
  blocks: block_set;
}

module Shape = struct
  type t = shape

  let mem head shape =
    match head with
    | Immediate imm ->
      begin match shape.imms with
      | Any -> true
      | Those set -> ImmSet.mem imm set
      end
    | Block tag ->
      begin match shape.blocks with
      | Any -> true
      | Those set -> TagSet.mem tag set
      end

  let inter sh1 sh2 =
    let inter_on_any inter any1 any2 =
      match any1, any2 with
      | Any, other | other, Any ->
          other
      | Those a, Those b -> Those (inter a b)
    in
    {
      imms = inter_on_any ImmSet.inter sh1.imms sh2.imms;
      blocks = inter_on_any TagSet.inter sh1.blocks sh2.blocks;
    }

  let is_any_on_any is_any = function
    | Any -> true
    | Those a -> is_any a

  let is_any_imm _ = false
  let is_any_block tag_map =
    TagSet.cardinal tag_map = 256

  let is_any sh =
    is_any_on_any is_any_imm sh.imms
    && is_any_on_any is_any_block sh.blocks

  let subset sh1 sh2 =
    let subset_on_any subset is_any any1 any2 =
      match any1, any2 with
      | Any, Any -> true
      | _, Any -> true
      | Any, Those b -> is_any b
      | Those a, Those b -> subset a b
    in
    subset_on_any ImmSet.subset is_any_imm sh1.imms sh2.imms
    &&
    subset_on_any TagSet.subset is_any_block sh1.blocks sh2.blocks

  let is_empty sh =
    let empty_on_any is_empty = function
      | Any -> false
      | Those s -> is_empty s
    in
    empty_on_any ImmSet.is_empty sh.imms
    && empty_on_any TagSet.is_empty sh.blocks

  let union sh1 sh2 =
    let imms = Or_any.mon_map2 ImmSet.union sh1.imms sh2.imms in
    let blocks = Or_any.mon_map2 TagSet.union sh1.blocks sh2.blocks in
    { imms; blocks; }

  let any =
    {
      imms = Any;
      blocks = Any;
    }

  let empty =
    {
      imms = Those ImmSet.empty;
      blocks = Those TagSet.empty;
    }

  let any_immediate =
    { empty with imms = Any }

  let imm li =
    let imms = Or_any.Those (ImmSet.of_list li) in
    { empty with imms }

  let block tags =
    let blocks : block_set = Those (TagSet.of_list tags) in
    { empty with blocks }

  let float =
    block [Tag Obj.double_tag]

  let string =
    block [Tag Obj.string_tag]

  let tuple =
    block [Tag 0]

  let array =
    block
      ([Tag 0]
       @ if Config.flat_float_array
         then [Tag Obj.double_array_tag]
         else [])

  let floatarray =
    block [Tag Obj.double_array_tag]

  let \#function =
    block [Tag Obj.closure_tag; Tag Obj.infix_tag]

  let \#object =
    block [Tag Obj.object_tag]

  let \#lazy arg_shape =
    (* Once lazy values are forced, their Forward_tag block can be 'cut
       short', and then they are represented exactly like the underlying
       type.

       Note that this union may be non-disjoint: it is okay if the
       underlying type can itself have lazy tags, shortcutting will be
       disabled by a runtime check.  *)
    union arg_shape
      (block [Tag Obj.lazy_tag; Tag Obj.forcing_tag; Tag Obj.forward_tag])

  let continuation =
    block [Tag Obj.cont_tag]

  let extensible_variant =
    (* constant extensible constructors have tag Obj.object_tag,
       non-constant constructors have tag 0. *)
    block [Tag 0; Tag Obj.object_tag]

  let polymorphic_variant ~has_consts ~has_nonconsts =
    (* constant polymorphic variants are immediates,
       non-constant variants have tag 0. *)
    union
      (if has_consts then any_immediate else empty)
      (if has_nonconsts then block [Tag 0] else empty)

  let abstract =
    block [Tag Obj.abstract_tag]

  let custom =
    block [Tag Obj.custom_tag]
end

type unboxed_cstr_description = TODO
