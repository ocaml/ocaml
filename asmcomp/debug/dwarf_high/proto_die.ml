(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module A = Dwarf_attributes.Attribute
module AS = Dwarf_attributes.Attribute_specification
module ASS = Dwarf_attributes.Attribute_specification.Sealed
module AV = Dwarf_attribute_values.Attribute_value
module F = Dwarf_attributes.Form
module Int = Numbers.Int
module V = Dwarf_attribute_values.Value

type reference = Asm_label.t
let create_reference () = Asm_label.create ()

type t = {
  parent : t option;
  mutable children_by_sort_priority : t list Int.Map.t;
  tag : Dwarf_tag.t;
  mutable attribute_values : AV.t ASS.Map.t;
  label : Asm_label.t;
  (* for references between DIEs within a single unit *)
  (* CR-someday mshinwell: consider combining [label] and [name] into one
     "how to reference this DIE" value. *)
  mutable name : Asm_symbol.t option;
  (* for references between DIEs across units *)
  (* CR mshinwell: Is this being used at the moment?  Check *)
}

(* CR-someday mshinwell: tidy up (very similar to Dwarf_attribute_helpers,
   but cannot use that due to circular dependency). *)
let reference_proto_die attribute proto_die =
  let spec = AS.create attribute F.Ref_addr in
  let label = proto_die.label in
  AV.create spec (V.offset_into_debug_info ~comment:"ref. to DIE" label)

let attribute_values_map attribute_values =
  List.fold_left (fun map attribute_value ->
      ASS.Map.add (AV.attribute_spec attribute_value) attribute_value map)
    ASS.Map.empty
    attribute_values

let create_sibling ~proto_die = reference_proto_die A.Sibling proto_die

let create ?reference ?(sort_priority = -1) ~parent ~tag ~attribute_values () =
  begin match parent with
  | None ->
    if tag <> Dwarf_tag.Compile_unit then begin
      failwith "only compilation unit proto-DIEs may be without parents"
    end
  | Some parent ->
    match Dwarf_tag.child_determination parent.tag with
    | Yes -> ()
    | No ->
      failwith "attempt to attach proto-DIE to proto-DIE that \
                never has children"
  end;
  (* Insert DW_AT_sibling to point at any next sibling of [t], if this new
     node might have children.  (Section 2.3, DWARF-4 spec; and it seems
     pointless if the node can never have children).  The order of siblings
     probably matters; we make sure that it is preserved by the use of ::
     below and within [depth_first_fold]. *)
  let attribute_values =
    match Dwarf_tag.child_determination tag with
    | No -> attribute_values
    | Yes ->
      match parent with
      | None -> attribute_values
      | Some parent ->
        match Int.Map.min_binding_opt parent.children_by_sort_priority with
        | None -> attribute_values
        | Some (sort_priority, []) ->
          Misc.fatal_errorf "Empty sort priority %d" sort_priority
        | Some (_sort_priority, (next_sibling_of_t ::_)) ->
          (create_sibling ~proto_die:next_sibling_of_t) :: attribute_values
  in
  let reference =
    match reference with
    | None -> Asm_label.create ()
    | Some reference -> reference
  in
  let attribute_values = attribute_values_map attribute_values in
  let t =
    { parent;
      children_by_sort_priority = Int.Map.empty;
      tag;
      attribute_values;
      label = reference;
      name = None;
    }
  in
  begin match parent with
  | None -> ()
  | Some parent ->
    let with_same_sort_priority =
      match Int.Map.find sort_priority parent.children_by_sort_priority with
      | exception Not_found -> []
      | children -> children
    in
    let with_same_sort_priority = t :: with_same_sort_priority in
    parent.children_by_sort_priority
      <- Int.Map.add sort_priority with_same_sort_priority
           parent.children_by_sort_priority
  end;
  t

let create_ignore ?reference ?sort_priority ~parent ~tag ~attribute_values () =
  let (_ : t) =
    create ?reference ?sort_priority ~parent ~tag ~attribute_values ()
  in
  ()

let add_or_replace_attribute t attribute_value =
  let attribute_values =
    ASS.Map.add (AV.attribute_spec attribute_value) attribute_value
      t.attribute_values
  in
  t.attribute_values <- attribute_values

let set_name t name = t.name <- Some name

type fold_arg =
  | DIE of Dwarf_tag.t * Child_determination.t
      * AV.t ASS.Map.t
      * Asm_label.t * Asm_symbol.t option (* optional name *)
  | End_of_siblings

let rec depth_first_fold t ~init ~f =
  let has_children : Child_determination.t =
    if Int.Map.is_empty t.children_by_sort_priority then No
    else Yes
  in
  let acc =
    f init (DIE (t.tag, has_children, t.attribute_values, t.label, t.name))
  in
  if Int.Map.is_empty t.children_by_sort_priority then
    acc
  else
    let acc =
      Int.Map.fold (fun _sort_priority children acc ->
          List.fold_left (fun acc child ->
              depth_first_fold child ~init:acc ~f)
            acc
            children)
        t.children_by_sort_priority
        acc
    in
    f acc End_of_siblings

let reference t = t.label
