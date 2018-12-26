(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module A = Asm_directives

module Range_list_entry = struct
  type t = {
    start_of_code_symbol : Asm_symbol.t;
    beginning_address_label : Asm_label.t;
    ending_address_label : Asm_label.t;
    ending_address_offset : int option;
  }

  let create ~start_of_code_symbol
             ~first_address_when_in_scope
             ~first_address_when_not_in_scope
             ~first_address_when_not_in_scope_offset =
    { start_of_code_symbol;
      beginning_address_label = first_address_when_in_scope;
      ending_address_label = first_address_when_not_in_scope;
      ending_address_offset = first_address_when_not_in_scope_offset;
    }

  let beginning_value t =
    Dwarf_value.code_address_from_label_symbol_diff
      ~comment:"beginning address"
      ~upper:t.beginning_address_label
      ~lower:t.start_of_code_symbol
      ~offset_upper:Targetint.zero
      ()

  let ending_value t =
    let offset_upper =
      match t.ending_address_offset with
      | None ->
        (* See note in location_list_entry.ml. *)
        Targetint.zero
      | Some offset -> Targetint.of_int_exn offset
    in
    Dwarf_value.code_address_from_label_symbol_diff
      ~comment:"ending address"
      ~upper:t.ending_address_label
      ~lower:t.start_of_code_symbol
      ~offset_upper
      ()

  let size t =
    let v1 = beginning_value t in
    let v2 = ending_value t in
    let (+) = Dwarf_int.add in
    Dwarf_value.size v1 + Dwarf_value.size v2

  let emit t =
    Dwarf_value.emit (beginning_value t);
    Dwarf_value.emit (ending_value t)
end

module Base_address_selection_entry = struct
  type t = Asm_symbol.t

  let create ~base_address_symbol = base_address_symbol

  let to_dwarf_values t =
    [Dwarf_value.absolute_address
       ~comment:"largest representable addr. offset"
       Targetint.minus_one;  (* all "1"s *)
     Dwarf_value.code_address_from_symbol ~comment:"base address" t;
    ]

  let size t =
    List.fold_left (fun acc v -> Dwarf_int.add acc (Dwarf_value.size v))
      (Dwarf_int.zero ())
      (to_dwarf_values t)

  let emit t =
    List.iter (fun v -> Dwarf_value.emit v) (to_dwarf_values t)
end

type t =
  | Range_list_entry of Range_list_entry.t
  | Base_address_selection_entry of Base_address_selection_entry.t

let create_range_list_entry ~start_of_code_symbol
      ~first_address_when_in_scope
      ~first_address_when_not_in_scope
      ~first_address_when_not_in_scope_offset =
  Range_list_entry (
    Range_list_entry.create ~start_of_code_symbol
      ~first_address_when_in_scope
      ~first_address_when_not_in_scope
      ~first_address_when_not_in_scope_offset)

let create_base_address_selection_entry ~base_address_symbol =
  Base_address_selection_entry (
    Base_address_selection_entry.create ~base_address_symbol)

let size = function
  | Range_list_entry entry ->
    Range_list_entry.size entry
  | Base_address_selection_entry entry ->
    Base_address_selection_entry.size entry

let emit t =
  match t with
  | Range_list_entry entry ->
    A.new_line ();
    A.comment "Range list entry:";
    Range_list_entry.emit entry
  | Base_address_selection_entry entry ->
    A.comment "Base address selection entry:";
    Base_address_selection_entry.emit entry

let compare_ascending_vma t1 t2 =
  (* This relies on a certain ordering on labels.  See [Compute_ranges]. *)
  match t1, t2 with
  | Base_address_selection_entry _, Base_address_selection_entry _ ->
    failwith "Range_list_entry.compare_ascending_vma: unsupported"
  | Base_address_selection_entry _, Range_list_entry _ -> -1
  | Range_list_entry _, Base_address_selection_entry _ -> 1
  | Range_list_entry entry1, Range_list_entry entry2 ->
    compare entry1.beginning_address_label entry2.beginning_address_label
