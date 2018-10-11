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

(** Values written into DWARF sections.
    (For attribute values, see [Dwarf_attribute_values].)
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module A = Asm_directives

module Int8 = Numbers.Int8
module Int16 = Numbers.Int16

type t =
  | Flag_true
  | Bool of bool
  | Int8 of Int8.t
  | Int16 of Int16.t
  | Int32 of Int32.t
  | Int64 of Int64.t
  | Uleb128 of Int64.t
  | Sleb128 of Int64.t
  | String of string
  | Indirect_string of string
  | Absolute_code_address of Targetint.t
  | Code_address_from_label of Asm_label.t
  | Code_address_from_symbol of Backend_sym.t
  | Code_address_from_label_symbol_diff of {
      upper : Asm_label.t;
      lower : Backend_sym.t;
      offset_upper : Targetint.t;
    }
  | Code_address_from_symbol_diff of {
      upper : Backend_sym.t;
      lower : Backend_sym.t;
    }
  | Code_address_from_symbol_plus_bytes of Backend_sym.t * Targetint.t
  | Offset_into_debug_info of Asm_label.t
  | Offset_into_debug_info_from_symbol of Backend_sym.t
  | Offset_into_debug_line of Asm_label.t
  | Offset_into_debug_line_from_symbol of Backend_sym.t
  | Offset_into_debug_loc of Asm_label.t
  | Offset_into_debug_abbrev of Asm_label.t
  | Distance_between_labels_16bit of {
      upper : Asm_label.t;
      lower : Asm_label.t;
    }
  | Distance_between_labels_32bit of {
      upper : Asm_label.t;
      lower : Asm_label.t;
    }
  | Distance_between_labels_64bit of {
      upper : Asm_label.t;
      lower : Asm_label.t;
    }

let print ppf t =
  match t with
  | Flag_true -> Format.pp_print_string ppf "true"
  | Bool b -> Format.fprintf ppf "%b" b
  | Int8 n -> Numbers.Int8.print ppf n
  | Int16 n -> Numbers.Int16.print ppf n
  | Int32 n -> Format.fprintf ppf "%ld" n
  | Int64 n -> Format.fprintf ppf "%Ld" n
  | Uleb128 n -> Format.fprintf ppf "(uleb128 %Ld)" n
  | Sleb128 n -> Format.fprintf ppf "(sleb128 %Ld)" n
  | String s -> Format.fprintf ppf "\"%S\"" s
  | Indirect_string s -> Format.fprintf ppf "\"%S\" [indirect]" s
  | Absolute_code_address addr ->
    Format.fprintf ppf "0x%Lx" (Targetint.to_int64 addr)
  | Code_address_from_label lbl -> Format.fprintf ppf "L%d" lbl
  | Code_address_from_symbol sym -> Backend_sym.print ppf sym
  | Code_address_from_label_symbol_diff { upper; lower; offset_upper; } ->
    Format.fprintf ppf "(L%d + %a) - %a"
      upper
      Targetint.print offset_upper
      Backend_sym.print lower
  | Code_address_from_symbol_diff { upper; lower; } ->
    Format.fprintf ppf "%a - %a"
      Backend_sym.print upper
      Backend_sym.print lower
  | Code_address_from_symbol_plus_bytes (sym, offset) ->
    Format.fprintf ppf "%a + %a"
      Backend_sym.print sym
      Targetint.print offset
  | Offset_into_debug_info lbl ->
    Format.fprintf ppf "%d - .debug_info" lbl
  | Offset_into_debug_info_from_symbol sym ->
    Format.fprintf ppf "%a - .debug_info" Backend_sym.print sym
  | Offset_into_debug_line lbl ->
    Format.fprintf ppf "%d - .debug_line" lbl
  | Offset_into_debug_line_from_symbol sym ->
    Format.fprintf ppf "%a - .debug_line" Backend_sym.print sym
  | Offset_into_debug_loc lbl ->
    Format.fprintf ppf "%d - .debug_loc" lbl
  | Offset_into_debug_abbrev lbl ->
    Format.fprintf ppf "%a - .debug_abbrev" Asm_label.print lbl
  | Distance_between_labels_16bit { upper; lower; } ->
    Format.fprintf ppf "L%d - L%d (16)" upper lower
  | Distance_between_labels_32bit { upper; lower; } ->
    Format.fprintf ppf "L%d - L%d (32)" upper lower
  | Distance_between_labels_64bit { upper; lower; } ->
    Format.fprintf ppf "L%d - L%d (64)" upper lower

(* DWARF-4 standard section 7.6. *)
let rec uleb128_size i =
  assert (Int64.compare i 0L >= 0);
  if Int64.compare i 128L < 0 then Dwarf_int.one ()
  else Dwarf_int.succ (uleb128_size (Int64.shift_right_logical i 7))

let rec sleb128_size i =
  if Int64.compare i (-64L) >= 0 && Int64.compare i 64L < 0 then
    Dwarf_int.one ()
  else
    Dwarf_int.succ (sleb128_size (Int64.shift_right i 7))

let size t =
  match t with
  | Flag_true -> Dwarf_int.zero ()  (* see comment below *)
  | Bool _ -> Dwarf_int.one ()
  | Int8 _ -> Dwarf_int.one ()
  | Int16 _ -> Dwarf_int.two ()
  | Int32 _ -> Dwarf_int.four ()
  | Int64 _ -> Dwarf_int.eight ()
  | Uleb128 i -> uleb128_size i
  | Sleb128 i -> sleb128_size i
  | Absolute_code_address _
  | Code_address_from_label _
  | Code_address_from_symbol _
  | Code_address_from_label_symbol_diff _
  | Code_address_from_symbol_diff _
  | Code_address_from_symbol_plus_bytes _ ->
    begin match Targetint.size with
    | 32 -> Dwarf_int.four ()
    | 64 -> Dwarf_int.eight ()
    | bits -> Misc.fatal_errorf "Unsupported Targetint.size %d" bits
    end
  | String str ->
    Dwarf_int.of_targetint_exn (Targetint.of_int (String.length str + 1))
  | Indirect_string _
  | Offset_into_debug_line _
  | Offset_into_debug_line_from_symbol _
  | Offset_into_debug_info _
  | Offset_into_debug_info_from_symbol _
  | Offset_into_debug_loc _
  | Offset_into_debug_abbrev _ -> Dwarf_int.size (Dwarf_int.zero ())
  | Distance_between_labels_16bit _ -> Dwarf_int.two ()
  | Distance_between_labels_32bit _ -> Dwarf_int.four ()
  | Distance_between_labels_64bit _ -> Dwarf_int.eight ()

let emit t =
  let width_for_ref_addr_or_sec_offset () : Target_system.machine_width =
    (* DWARF-4 specification p.142. *)
    match Dwarf_format.get () with
    | Thirty_two -> Thirty_two
    | Sixty_four -> Sixty_four
  in
  match t with
  | Flag_true -> ()  (* DWARF-4 specification p.148 *)
  | Bool b -> A.int8 (if b then Int8.one else Int8.zero)
  | Int8 i -> A.int8 i
  | Int16 i -> A.int16 i
  | Int32 i -> A.int32 i
  | Int64 i -> A.int64 i
  | Uleb128 i -> A.uleb128 i
  | Sleb128 i -> A.sleb128 i
  | String str -> A.string str
  | Indirect_string s ->
    (* "Indirect" strings are collected together into ".debug_str". *)
    let label = A.cache_string s in
    A.offset_into_section_label (DWARF Debug_str) label
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Absolute_code_address addr -> A.targetint addr
  | Code_address_from_label label -> A.label (Asm_label.create_int label)
  | Code_address_from_symbol symbol -> A.symbol (Asm_symbol.create symbol)
  | Code_address_from_label_symbol_diff { upper; lower; offset_upper; } ->
    let upper = Asm_label.create_int upper in
    let lower = Asm_symbol.create lower in
    A.between_symbol_and_label_offset ~upper ~lower ~offset_upper
  | Code_address_from_symbol_diff { upper; lower; } ->
    let upper = Asm_symbol.create upper in
    let lower = Asm_symbol.create lower in
    A.between_symbols ~upper ~lower
  | Code_address_from_symbol_plus_bytes (symbol, offset_in_bytes) ->
    let symbol = Asm_symbol.create symbol in
    A.symbol_plus_offset symbol ~offset_in_bytes
  | Offset_into_debug_line label ->
    let label = Asm_label.create_int label in
    A.offset_into_section_label (DWARF Debug_line) label
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Offset_into_debug_line_from_symbol symbol ->
    let symbol = Asm_symbol.create symbol in
    A.offset_into_section_symbol (DWARF Debug_line) symbol
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Offset_into_debug_info label ->
    let label = Asm_label.create_int label in
    A.offset_into_section_label (DWARF Debug_info) label
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Offset_into_debug_info_from_symbol symbol ->
    let symbol = Asm_symbol.create symbol in
    A.offset_into_section_symbol (DWARF Debug_info) symbol
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Offset_into_debug_loc label ->
    let label = Asm_label.create_int label in
    A.offset_into_section_label (DWARF Debug_loc) label
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Offset_into_debug_abbrev label ->
    A.offset_into_section_label (DWARF Debug_abbrev) label
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Distance_between_labels_16bit { upper; lower; } ->
    let upper = Asm_label.create_int upper in
    let lower = Asm_label.create_int lower in
    (* CR-someday mshinwell: This should really be checked for overflow, but
       seems hard... *)
    A.between_labels_16bit ~upper ~lower
  | Distance_between_labels_32bit { upper; lower; } ->
    let upper = Asm_label.create_int upper in
    let lower = Asm_label.create_int lower in
    (* CR-someday mshinwell: Same comment as for the 16 bit case. *)
    A.between_labels_32bit ~upper ~lower
  | Distance_between_labels_64bit { upper; lower; } ->
    let upper = Asm_label.create_int upper in
    let lower = Asm_label.create_int lower in
    A.between_labels_64bit ~upper ~lower
