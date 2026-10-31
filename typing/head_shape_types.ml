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
type size = Size of int [@@unboxed]

type head =
  | Immediate of imm
  | Block of tag * size

module ImmSet = Set.Make(struct type t = imm let compare = Stdlib.compare end)
module SizeSet = Set.Make(struct type t = size let compare = Stdlib.compare end)
module TagMap = Map.Make(struct type t = tag let compare = Stdlib.compare end)

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
type size_set = SizeSet.t or_any
type block_set = size_set TagMap.t or_any

type shape = {
  imms: imm_set;
  blocks: block_set;
  separated: bool;
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
    | Block (tag, size) ->
      begin match shape.blocks with
      | Any -> true
      | Those tag_map ->
        match TagMap.find tag tag_map with
        | exception Not_found -> false
        | Any -> true
        | Those sizes -> SizeSet.mem size sizes
      end

  let is_empty_on_any is_empty = function
    | Any -> false
    | Those s -> is_empty s

  let is_empty_imms imms =
    is_empty_on_any ImmSet.is_empty imms

  let is_empty_sizes sizes =
    is_empty_on_any SizeSet.is_empty sizes

  let is_empty_blocks blocks =
    is_empty_on_any (TagMap.for_all (fun _ -> is_empty_sizes)) blocks

  let is_empty sh =
    is_empty_imms sh.imms
    && is_empty_blocks sh.blocks

  let has_no_float sh =
    is_empty_on_any (fun blocks ->
      match TagMap.find (Tag Obj.double_tag) blocks with
      | exception Not_found -> true
      | sizes -> is_empty_sizes sizes
    ) sh.blocks

  let has_only_float sh =
    is_empty_imms sh.imms
    &&
    match sh.blocks with
    | Any -> false
    | Those blocks ->
        TagMap.for_all (fun (Tag tag) sizes ->
          tag = Obj.double_tag || is_empty_sizes sizes
        ) blocks

  let has_float sh = not (has_no_float sh)
  let has_nonfloat sh = not (has_only_float sh)

  let inter sh1 sh2 =
    let inter_on_any inter any1 any2 =
      match any1, any2 with
      | Any, other | other, Any ->
          other
      | Those a, Those b -> Those (inter a b)
    in
    let imms = inter_on_any ImmSet.inter sh1.imms sh2.imms in
    let blocks =
      inter_on_any (fun tm1 tm2 ->
        TagMap.merge (fun _tag s1 s2 ->
          match s1, s2 with
          | None, _ | _, None -> None
          | Some s1, Some s2 -> Some (inter_on_any SizeSet.inter s1 s2)
        ) tm1 tm2
      ) sh1.blocks sh2.blocks
    in
    let separated =
      (* Shapes have a relational interpretation as sets of sets of values.
         If either [sh1] or [sh2] are separated, they only contains separated
         sets of values, so the intersection is also separated. *)
      sh1.separated || sh2.separated
    in
    { imms; blocks; separated; }

  let is_any_on_any is_any = function
    | Any -> true
    | Those a -> is_any a

  let is_any_imm _ = false
  let is_any_size _ = false

  let is_any_block tag_map =
    TagMap.cardinal tag_map = 256
    &&
    TagMap.for_all (fun _ -> function
      | Any -> true
      | Those s -> is_any_size s
    ) tag_map

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
    let subset_imm = ImmSet.subset in
    let subset_size = SizeSet.subset in
    let subset_block tm1 tm2 =
      TagMap.for_all (fun tag sizes1 ->
        let sizes2 =
          try TagMap.find tag tm2
          with Not_found -> Those SizeSet.empty in
        subset_on_any subset_size is_any_size sizes1 sizes2
      ) tm1
    in
    subset_on_any subset_imm is_any_imm sh1.imms sh2.imms
    &&
    subset_on_any subset_block is_any_block sh1.blocks sh2.blocks

  let union sh1 sh2 =
    let imms = Or_any.mon_map2 ImmSet.union sh1.imms sh2.imms in
    let blocks =
      Or_any.mon_bind2 (fun bs1 bs2 ->
        let any_count = ref 0 in
        let tag_map =
          TagMap.union (fun _tag s1 s2 ->
            let s = Or_any.mon_map2 SizeSet.union s1 s2 in
            (if s = Any then incr any_count);
            Some s
          ) bs1 bs2
        in
        if !any_count = 256 then begin
          assert (
            List.init 256 Fun.id
            |> List.for_all (fun i ->
              TagMap.find_opt (Tag i) tag_map = Some Any)
          );
          Any
        end
        else Those tag_map
      ) sh1.blocks sh2.blocks
    in
    let separated =
      sh1.separated
      && sh2.separated
      && not (has_float sh1 && has_nonfloat sh2)
      && not (has_nonfloat sh2 && has_float sh1)
    in
    { imms; blocks; separated }

  let any =
    {
      imms = Any;
      blocks = Any;
      separated = true;
    }

  let poison = { any with separated = false }

  let empty =
    {
      imms = Those ImmSet.empty;
      blocks = Those TagMap.empty;
      separated = true;
    }

  let any_immediate =
    { empty with imms = Any }

  let imm li =
    let imms = Or_any.Those (ImmSet.of_list li) in
    { empty with imms }

  let block ?size tags =
    let blocks : block_set =
      let size : size_set = match size with
        | None -> Any
        | Some fixed -> Those (SizeSet.singleton (Size fixed))
      in
      Those (TagMap.of_list (List.map (fun tags -> (tags, size)) tags))
    in
    { empty with blocks }

  let float =
    (* Float values have size 1 or 2 depending
       on the architecture. *)
    union
      (block ~size:1 [Tag Obj.double_tag])
      (block ~size:2 [Tag Obj.double_tag])

  let string =
    block [Tag Obj.string_tag]

  let tuple ?size () =
    block ?size [Tag 0]

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
       disabled by a runtime check. Shortcutting is also disabled on float
       values, to preserve separatedness. *)
    (* Note: currently lazy/forcing/forward blocks have size 1, but we
       decided to not reflect this in the type as the representation
       might change in the future. *)
    { (union arg_shape
         (block [Tag Obj.lazy_tag; Tag Obj.forcing_tag; Tag Obj.forward_tag]))
      with separated = true;
    }

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

  let abstract ?size () =
    block ?size [Tag Obj.abstract_tag]

  let custom ?size () =
    block ?size [Tag Obj.custom_tag]
end

type unboxed_cstr_data = Types.type_declaration * Types.type_expr
type unboxed_cstr_description = (unboxed_cstr_data, shape) Misc.Cached.t
