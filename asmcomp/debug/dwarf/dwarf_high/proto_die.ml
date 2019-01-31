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

module ASS = Dwarf_attributes.Attribute_specification.Sealed
module AV = Dwarf_attribute_values.Attribute_value
module Int = Numbers.Int

type reference = Asm_label.t
let create_reference () = Asm_label.create (DWARF Debug_info)

type t = {
  parent : t option;
  mutable children_by_sort_priority : t list Int.Map.t;
  tag : Dwarf_tag.t;
  mutable attribute_values : AV.t ASS.Map.t;
  label : Asm_label.t;
  (* For references between DIEs within a single unit *)
  (* CR-someday mshinwell: consider combining [label] and [name] into one
     "how to reference this DIE" value. *)
  mutable name : Asm_symbol.t option;
  (* For references between DIEs across units *)
  location_list_in_debug_loc_table : Dwarf_4_location_list.t option;
}

let attribute_values_map attribute_values =
  List.fold_left (fun map attribute_value ->
      ASS.Map.add (AV.attribute_spec attribute_value) attribute_value map)
    ASS.Map.empty
    attribute_values

(* CR-someday mshinwell: Resurrect support for sibling links. *)

let create ?reference ?(sort_priority = -1) ?location_list_in_debug_loc_table
      ~parent ~tag ~attribute_values () =
  begin match parent with
  | None ->
    if tag <> Dwarf_tag.Compile_unit then begin
      failwith "only compilation unit proto-DIEs may be without parents"
    end
  | Some _parent -> ()
  end;
  let reference =
    match reference with
    | None -> Asm_label.create (DWARF Debug_info)
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
      location_list_in_debug_loc_table;
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

let create_ignore ?reference ?sort_priority ?location_list_in_debug_loc_table
      ~parent ~tag ~attribute_values () =
  let (_ : t) =
    create ?reference ?sort_priority ?location_list_in_debug_loc_table
      ~parent ~tag ~attribute_values ()
  in
  ()

let add_or_replace_attribute_value t attribute_value =
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
      * Dwarf_4_location_list.t option
  | End_of_siblings

let rec depth_first_fold t ~init ~f =
  let has_children : Child_determination.t =
    if Int.Map.is_empty t.children_by_sort_priority then No
    else Yes
  in
  let acc =
    f init (DIE (t.tag, has_children, t.attribute_values, t.label, t.name,
      t.location_list_in_debug_loc_table))
  in
  if Int.Map.is_empty t.children_by_sort_priority then
    acc
  else
    let acc =
      Int.Map.fold (fun _sort_priority children_rev acc ->
          List.fold_left (fun acc child ->
              depth_first_fold child ~init:acc ~f)
            acc
            (List.rev children_rev))
        t.children_by_sort_priority
        acc
    in
    f acc End_of_siblings

let reference t = t.label

let location_list_in_debug_loc_table t = t.location_list_in_debug_loc_table
