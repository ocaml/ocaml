(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module A = Dwarf_attributes.Attribute
module AS = Dwarf_attributes.Attribute_specification
module AV = Dwarf_attribute_values.Attribute_value
module F = Dwarf_attributes.Form
module V = Dwarf_attribute_values.Value

type t = {
  parent : t option;
  mutable children : t list;
  tag : Dwarf_tag.t;
  mutable attribute_values : AV.t list;
  label : Linearize.label;
  (* for references between DIEs within a single unit *)
  (* CR-someday mshinwell: consider combining [label] and [name] into one
     "how to reference this DIE" value. *)
  mutable name : Symbol.t option;
  (* for references between DIEs across units *)
  mutable sort_priority : int;
}

(* CR-someday mshinwell: tidy up (very similar to Dwarf_attribute_helpers,
   but cannot use that due to circular dependency). *)
let reference_proto_die attribute proto_die =
  let spec = AS.create attribute F.Ref_addr in
  let label = proto_die.label in
  AV.create spec (V.offset_into_debug_info label)

let create_sibling ~proto_die = reference_proto_die A.Sibling proto_die

let create ~parent ~tag ~attribute_values =
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
        match parent.children with
        | [] -> attribute_values
        | next_sibling_of_t::_ ->
          (create_sibling ~proto_die:next_sibling_of_t)
            :: attribute_values
  in
  let t =
    { parent;
      children = [];
      tag;
      attribute_values;
      label = Linearize.new_label ();
      name = None;
      sort_priority = -1;
    }
  in
  begin match parent with
  | None -> ()
  | Some parent -> parent.children <- t :: parent.children
  end;
  t

let create_ignore ~parent ~tag ~attribute_values =
  let (_ : t) = create ~parent ~tag ~attribute_values in
  ()

let set_name t name = t.name <- Some name
let set_sort_priority t priority = t.sort_priority <- priority

let sort_children ts =
  ListLabels.sort ts
    ~cmp:(fun t1 t2 -> compare t1.sort_priority t2.sort_priority)

let rec depth_first_fold t ~init ~f =
  let children : Child_determination.t =
    match t.children with
    | [] -> No
    | _ -> Yes
  in
  let acc =
    f init (`DIE (t.tag, children, t.attribute_values, t.label, t.name))
  in
  match t.children with
  | [] -> acc
  | _ ->
    let rec traverse_children ts ~acc =
      let ts = sort_children ts in
      match ts with
      | [] -> f acc `End_of_siblings
      | t::ts -> traverse_children ts ~acc:(depth_first_fold t ~init:acc ~f)
    in
    traverse_children t.children ~acc

let reference t = t.label
