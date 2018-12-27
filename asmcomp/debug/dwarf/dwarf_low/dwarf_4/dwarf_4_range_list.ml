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

module A = Asm_directives

type t = {
  name : Asm_label.t;
  entries : Dwarf_4_range_list_entry.t list;
}

(* Same note as in location_list.ml. *)
let sort entries =
  List.sort Dwarf_4_range_list_entry.compare_ascending_vma entries

let create ~range_list_entries =
  { name = Asm_label.create (DWARF Debug_ranges);
    entries = sort range_list_entries;
  }

let label t = t.name

let end_marker () =
  Dwarf_value.absolute_address ~comment:"end marker" Targetint.zero

let size t =
  let (+) = Dwarf_int.add in
  let body_size =
    List.fold_left (fun size entry ->
        size + (Dwarf_4_range_list_entry.size entry))
      (Dwarf_int.zero ())
      t.entries
  in
  let end_marker = end_marker () in
  body_size + Dwarf_value.size end_marker + Dwarf_value.size end_marker

let compare_increasing_vma t1 t2 =
  match t1.entries, t2.entries with
  | t1_entry::_, t2_entry::_ ->
    Dwarf_4_range_list_entry.compare_ascending_vma t1_entry t2_entry
  | _ -> failwith "Range_list.compare on empty range list(s)"

let emit t =
  A.new_line ();
  A.comment "Range list:";
  A.define_label t.name;
  List.iter (fun entry -> Dwarf_4_range_list_entry.emit entry) t.entries;
  (* DWARF-4 spec, section 2.17.3 (p.39). *)
  let end_marker = end_marker () in
  Dwarf_value.emit end_marker;
  Dwarf_value.emit end_marker
