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

module Location_list_entry = struct
  type t = {
    start_of_code_symbol : Asm_symbol.t;
    beginning_address_label : Asm_label.t;
    beginning_address_offset : int option;
    ending_address_label : Asm_label.t;
    ending_address_offset : int option;
    expr : Single_location_description.t;
  }

  let create ~start_of_code_symbol
             ~first_address_when_in_scope
             ~first_address_when_in_scope_offset
             ~first_address_when_not_in_scope
             ~first_address_when_not_in_scope_offset
             ~single_location_description =
    { start_of_code_symbol;
      beginning_address_label = first_address_when_in_scope;
      beginning_address_offset = first_address_when_in_scope_offset;
      ending_address_label = first_address_when_not_in_scope;
      ending_address_offset = first_address_when_not_in_scope_offset;
      expr = single_location_description;
    }

  let expr_size t =
    let size = Single_location_description.size t.expr in
    let size = Dwarf_int.to_int64 size in
    (* CR-someday mshinwell: maybe this size should be unsigned? *)
    assert (Int64.compare size 0xffffL < 0);
    Numbers.Int16.of_int64_exn size

  let beginning_value t =
    let offset_upper =
      match t.beginning_address_offset with
      | None -> Targetint.zero
      | Some offset -> Targetint.of_int_exn offset
    in
    Dwarf_value.code_address_from_label_symbol_diff
      ~comment:"beginning address"
      ~upper:t.beginning_address_label
      ~lower:t.start_of_code_symbol
      ~offset_upper
      ()

  let ending_value t =
    let offset_upper =
      match t.ending_address_offset with
      | None ->
        (* It might seem as if this should be "-1", but actually not.
           DWARF-4 spec p.30 (point 2):
           "...the first address past the end of the address range over
            which the location is valid." *)
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
    let v3 = Dwarf_value.int16 (expr_size t) in
    let (+) = Dwarf_int.add in
    Dwarf_value.size v1 + Dwarf_value.size v2 + Dwarf_value.size v3
      + Single_location_description.size t.expr

  let emit t =
    Dwarf_value.emit (beginning_value t);
    Dwarf_value.emit (ending_value t);
    A.int16 ~comment:"expression size" (expr_size t);
    A.comment "Single location description (DWARF expression):";
    Single_location_description.emit t.expr
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
  | Location_list_entry of Location_list_entry.t
  | Base_address_selection_entry of Base_address_selection_entry.t

let create_location_list_entry ~start_of_code_symbol
      ~first_address_when_in_scope
      ~first_address_when_in_scope_offset
      ~first_address_when_not_in_scope
      ~first_address_when_not_in_scope_offset
      ~single_location_description =
  Location_list_entry (
    Location_list_entry.create ~start_of_code_symbol
      ~first_address_when_in_scope
      ~first_address_when_in_scope_offset
      ~first_address_when_not_in_scope
      ~first_address_when_not_in_scope_offset
      ~single_location_description)

let create_base_address_selection_entry ~base_address_symbol =
  Base_address_selection_entry (
    Base_address_selection_entry.create ~base_address_symbol)

let size = function
  | Location_list_entry entry ->
    Location_list_entry.size entry
  | Base_address_selection_entry entry ->
    Base_address_selection_entry.size entry

let emit t =
  match t with
  | Location_list_entry entry ->
    A.new_line ();
    A.comment "Location list entry:";
    Location_list_entry.emit entry
  | Base_address_selection_entry entry ->
    A.comment "Base address selection entry:";
    Base_address_selection_entry.emit entry

let compare_ascending_vma t1 t2 =
  (* This relies on a certain ordering on labels.  See [Compute_ranges]. *)
  match t1, t2 with
  | Base_address_selection_entry _, Base_address_selection_entry _ ->
    failwith "Location_list_entry.compare_ascending_vma: unsupported"
  | Base_address_selection_entry _, Location_list_entry _ -> -1
  | Location_list_entry _, Base_address_selection_entry _ -> 1
  | Location_list_entry entry1, Location_list_entry entry2 ->
    compare entry1.beginning_address_label entry2.beginning_address_label
