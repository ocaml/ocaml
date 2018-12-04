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

type entry =
  | End_of_list
  | Base_addressx of Address_index.t
  | Startx_endx of {
      start_inclusive : Address_index.t;
      end_exclusive : Address_index.t;
      loc_desc : Counted_location_description.t;
    }
  | Startx_length of {
      start_inclusive : Address_index.t;
      length : Targetint.t;
      loc_desc : Counted_location_description.t;
    }
  | Offset_pair of {
      start_offset_inclusive : Targetint.t;
      end_offset_exclusive : Targetint.t;
      loc_desc : Counted_location_description.t;
    }
  | Default_location of Counted_location_description.t
  | Base_address of Targetint.t
  | Start_end of {
      start_inclusive : Asm_label.t;
      end_exclusive : Asm_label.t;
      end_adjustment : int;
      loc_desc : Counted_location_description.t;
    }
  | Start_length of {
      start_inclusive : Asm_label.t;
      length : Targetint.t;
      loc_desc : Counted_location_description.t;
    }

type t = {
  entry : entry;
  start_of_code_symbol : Asm_symbol.t;
}

let create entry ~start_of_code_symbol =
  { entry;
    start_of_code_symbol;
  }

let code t =
  (* DWARF-5 spec page 227. *)
  match t.entry with
  | End_of_list -> 0x00
  | Base_addressx _ -> 0x01
  | Startx_endx _ -> 0x02
  | Startx_length _ -> 0x03
  | Offset_pair _ -> 0x04
  | Default_location _ -> 0x05
  | Base_address _ -> 0x06
  | Start_end _ -> 0x07
  | Start_length _ -> 0x08

let label_address t label ~adjustment =
  let adjustment = Targetint.of_int_exn adjustment in
  Dwarf_value.code_address_from_label_symbol_diff
    ~comment:"ending address"
    ~upper:label
    ~lower:t.start_of_code_symbol
    ~offset_upper:adjustment

let size t =
  ...

let emit t =
  (* DWARF-5 spec page 44 lines 14--15. *)
  A.int8 (Int8.of_int_exn (code t));
  match t with
  | End_of_list -> ()
  | Base_addressx addr_index ->
    Address_index.emit addr_index
  | Startx_endx {
      start_inclusive;
      end_exclusive;
      loc_desc;
    } ->
    Address_index.emit start_inclusive;
    Address_index.emit end_exclusive;
    Counted_location_description.emit loc_desc
  | Startx_length {
      start_inclusive;
      length;
      loc_desc;
    } ->
    Address_index.emit start_inclusive;
    A.targetint length;
    Counted_location_description.emit loc_desc
  | Offset_pair of {
      start_offset_inclusive;
      end_offset_exclusive;
      loc_desc;
    } ->
    Dwarf_value.emit (Dwarf_value.leb128 (
      Targetint.to_int64 start_offset_inclusive));
    Dwarf_value.emit (Dwarf_value.leb128 (
      Targetint.to_int64 end_offset_exclusive));
    Counted_location_description.emit loc_desc
  | Default_location loc_desc ->
    Counted_location_description.emit loc_desc
  | Base_address addr ->

  | Start_end {
      start_inclusive;
      end_exclusive;
      end_adjustment;
      loc_desc;
    } ->
    Dwarf_value.emit (label_address start_inclusive ~adjustment:0);
    Dwarf_value.emit (label_address end_exclusive ~adjustment:end_adjustment);
    Counted_location_description.emit loc_desc
  | Start_length {
      start_inclusive;
      length;
      loc_desc;
    } ->
    Dwarf_value.emit (label_address start_inclusive ~adjustment:0);
    Dwarf_value.emit (Dwarf_value.uleb128 (Targetint.to_int64 length));
    Counted_location_description.emit loc_desc




(* Old DWARF-4 code:

module Location_list_entry = struct
  type t = {
    start_of_code_symbol : Asm_symbol.t;
    beginning_address_label : Asm_label.t;
    ending_address_label : Asm_label.t;
    ending_address_offset : int option;
    expr : Single_location_description.t;
  }

  let create ~start_of_code_symbol
             ~first_address_when_in_scope
             ~first_address_when_not_in_scope
             ~first_address_when_not_in_scope_offset
             ~single_location_description =
    { start_of_code_symbol;
      beginning_address_label = first_address_when_in_scope;
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
    Dwarf_value.code_address_from_label_symbol_diff
      ~comment:"beginning address"
      ~upper:t.beginning_address_label
      ~lower:t.start_of_code_symbol
      ~offset_upper:Targetint.zero

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
      ~first_address_when_not_in_scope
      ~first_address_when_not_in_scope_offset
      ~single_location_description =
  Location_list_entry (
    Location_list_entry.create ~start_of_code_symbol
      ~first_address_when_in_scope
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
*)
