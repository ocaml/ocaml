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
  | Flag_true of { comment : string option; }
  | Bool of { b : bool; comment : string option; }
  | Int8 of { i : Int8.t; comment : string option; }
  | Int16 of { i : Int16.t; comment : string option; }
  | Int32 of { i : Int32.t; comment : string option; }
  | Int64 of { i : Int64.t; comment : string option; }
  | Uleb128 of { i : Int64.t; comment : string option; }
  | Sleb128 of { i : Int64.t; comment : string option; }
  | String of { str : string; comment : string option; }
  | Indirect_string of { str : string; comment : string option; }
  | Absolute_address of { addr : Targetint.t; comment : string option; }
  | Code_address_from_label of { lbl : Asm_label.t; comment : string option; }
  | Code_address_from_symbol of { sym : Asm_symbol.t; comment : string option; }
  | Code_address_from_label_symbol_diff of {
      upper : Asm_label.t;
      lower : Asm_symbol.t;
      offset_upper : Targetint.t;
    }
  | Code_address_from_symbol_diff of {
      upper : Asm_symbol.t;
      lower : Asm_symbol.t;
    }
  | Code_address_from_symbol_plus_bytes of {
      sym : Asm_symbol.t;
      offset_in_bytes : Targetint.t;
    }
  | Offset_into_debug_info of {
      lbl : Asm_label.t;
      comment : string option;
    }
  | Offset_into_debug_info_from_symbol of {
      sym : Asm_symbol.t;
      comment : string option;
    }
  | Offset_into_debug_line of Asm_label.t
  | Offset_into_debug_line_from_symbol of Asm_symbol.t
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
  | Flag_true { comment = _; } -> Format.pp_print_string ppf "true"
  | Bool { b; comment = _; } -> Format.fprintf ppf "%b" b
  | Int8 { i; comment = _; } -> Int8.print ppf i
  | Int16 { i; comment = _; } -> Int16.print ppf i
  | Int32 { i; comment = _; } -> Format.fprintf ppf "%ld" i
  | Int64 { i; comment = _; } -> Format.fprintf ppf "%Ld" i
  | Uleb128 { i; comment = _; } -> Format.fprintf ppf "(uleb128 %Ld)" i
  | Sleb128 { i; comment = _; } -> Format.fprintf ppf "(sleb128 %Ld)" i
  | String { str; comment = _; } -> Format.fprintf ppf "\"%S\"" str
  | Indirect_string { str; comment = _; } ->
    Format.fprintf ppf "\"%S\" [indirect]" str
  | Absolute_address { addr; comment = _; } ->
    Format.fprintf ppf "0x%Lx" (Targetint.to_int64 addr)
  | Code_address_from_label { lbl; comment = _; } -> Asm_label.print ppf lbl
  | Code_address_from_symbol { sym; comment = _; } -> Asm_symbol.print ppf sym
  | Code_address_from_label_symbol_diff { upper; lower; offset_upper; } ->
    Format.fprintf ppf "(%a + %a) - %a"
      Asm_label.print upper
      Targetint.print offset_upper
      Asm_symbol.print lower
  | Code_address_from_symbol_diff { upper; lower; } ->
    Format.fprintf ppf "%a - %a"
      Asm_symbol.print upper
      Asm_symbol.print lower
  | Code_address_from_symbol_plus_bytes { sym; offset_in_bytes; } ->
    Format.fprintf ppf "%a + %a"
      Asm_symbol.print sym
      Targetint.print offset_in_bytes
  | Offset_into_debug_info { lbl; comment = _; } ->
    Format.fprintf ppf "%a - .debug_info" Asm_label.print lbl
  | Offset_into_debug_info_from_symbol { sym; comment = _; } ->
    Format.fprintf ppf "%a - .debug_info" Asm_symbol.print sym
  | Offset_into_debug_line lbl ->
    Format.fprintf ppf "%a - .debug_line" Asm_label.print lbl
  | Offset_into_debug_line_from_symbol sym ->
    Format.fprintf ppf "%a - .debug_line" Asm_symbol.print sym
  | Offset_into_debug_loc lbl ->
    Format.fprintf ppf "%a - .debug_loc" Asm_label.print lbl
  | Offset_into_debug_abbrev lbl ->
    Format.fprintf ppf "%a - .debug_abbrev" Asm_label.print lbl
  | Distance_between_labels_16bit { upper; lower; } ->
    Format.fprintf ppf "%a - %a (16)"
      Asm_label.print upper
      Asm_label.print lower
  | Distance_between_labels_32bit { upper; lower; } ->
    Format.fprintf ppf "%a - %a (32)"
      Asm_label.print upper
      Asm_label.print lower
  | Distance_between_labels_64bit { upper; lower; } ->
    Format.fprintf ppf "%a - %a (64)"
      Asm_label.print upper
      Asm_label.print lower

let flag_true ?comment () = Flag_true { comment; }

let bool ?comment b = Bool { b; comment; }

let int8 ?comment i = Int8 { i; comment; }

let int16 ?comment i = Int16 { i; comment; }

let int32 ?comment i = Int32 { i; comment; }

let int64 ?comment i = Int64 { i; comment; }

let uleb128 ?comment i = Uleb128 { i; comment; }

let sleb128 ?comment i = Sleb128 { i; comment; }

let string ?comment str = String { str; comment; }

let indirect_string ?comment str = Indirect_string { str; comment; }

let absolute_address ?comment addr = Absolute_address { addr; comment; }

let code_address_from_label ?comment lbl =
  Code_address_from_label { lbl; comment; }

let code_address_from_symbol ?comment sym =
  Code_address_from_symbol { sym; comment; }

let code_address_from_label_symbol_diff ~upper ~lower ~offset_upper =
  Code_address_from_label_symbol_diff { upper; lower; offset_upper; }

let code_address_from_symbol_diff ~upper ~lower =
  Code_address_from_symbol_diff { upper; lower; }

let code_address_from_symbol_plus_bytes sym offset_in_bytes =
  Code_address_from_symbol_plus_bytes { sym; offset_in_bytes; }

let offset_into_debug_info ?comment lbl =
  Offset_into_debug_info { lbl; comment; }

let offset_into_debug_info_from_symbol ?comment sym =
  Offset_into_debug_info_from_symbol { sym; comment; }

let offset_into_debug_line lbl = Offset_into_debug_line lbl

let offset_into_debug_line_from_symbol sym =
  Offset_into_debug_line_from_symbol sym

let offset_into_debug_loc lbl = Offset_into_debug_loc lbl

let offset_into_debug_abbrev lbl = Offset_into_debug_abbrev lbl

let distance_between_labels_16bit ~upper ~lower =
  Distance_between_labels_16bit { upper; lower; }

let distance_between_labels_32bit ~upper ~lower =
  Distance_between_labels_32bit { upper; lower; }

let distance_between_labels_64bit ~upper ~lower =
  Distance_between_labels_64bit { upper; lower; }

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
  | Flag_true _ -> Dwarf_int.zero ()  (* see comment below *)
  | Bool _ -> Dwarf_int.one ()
  | Int8 _ -> Dwarf_int.one ()
  | Int16 _ -> Dwarf_int.two ()
  | Int32 _ -> Dwarf_int.four ()
  | Int64 _ -> Dwarf_int.eight ()
  | Uleb128 { i; comment = _; } -> uleb128_size i
  | Sleb128 { i; comment = _; } -> sleb128_size i
  | Absolute_address _
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
  | String { str; comment = _; } ->
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
  | Flag_true { comment; } ->
    (* See DWARF-4 specification p.148 *)
    begin match comment with
    | None -> ()
    | Some comment -> A.comment (comment ^ " (Flag_true elided)")
    end
  | Bool { b; comment; } -> A.int8 ?comment (if b then Int8.one else Int8.zero)
  | Int8 { i; comment; } -> A.int8 ?comment i
  | Int16 { i; comment; } -> A.int16 ?comment i
  | Int32 { i; comment; } -> A.int32 ?comment i
  | Int64 { i; comment; } -> A.int64 ?comment i
  | Uleb128 { i; comment; } -> A.uleb128 ?comment i
  | Sleb128 { i; comment; } -> A.sleb128 ?comment i
  | String { str; comment; } -> A.string ?comment str
  | Indirect_string { str; comment; } ->
    (* "Indirect" strings are collected together into ".debug_str". *)
    let label = A.cache_string ?comment str in
    A.offset_into_section_label (DWARF Debug_str) label
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Absolute_address { addr; comment; } -> A.targetint ?comment addr
  | Code_address_from_label { lbl; comment; } -> A.label ?comment lbl
  | Code_address_from_symbol { sym; comment; } -> A.symbol ?comment sym
  | Code_address_from_label_symbol_diff { upper; lower; offset_upper; } ->
    A.between_symbol_and_label_offset ~upper ~lower ~offset_upper
  | Code_address_from_symbol_diff { upper; lower; } ->
    A.between_symbols ~upper ~lower
  | Code_address_from_symbol_plus_bytes { sym; offset_in_bytes; } ->
    A.symbol_plus_offset sym ~offset_in_bytes
  | Offset_into_debug_line label ->
    A.offset_into_section_label (DWARF Debug_line) label
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Offset_into_debug_line_from_symbol symbol ->
    A.offset_into_section_symbol (DWARF Debug_line) symbol
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Offset_into_debug_info { lbl; comment; } ->
    A.offset_into_section_label ?comment (DWARF Debug_info) lbl
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Offset_into_debug_info_from_symbol { sym; comment; } ->
    A.offset_into_section_symbol ?comment (DWARF Debug_info) sym
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Offset_into_debug_loc label ->
    A.offset_into_section_label (DWARF Debug_loc) label
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Offset_into_debug_abbrev label ->
    A.offset_into_section_label (DWARF Debug_abbrev) label
      ~width:(width_for_ref_addr_or_sec_offset ())
  | Distance_between_labels_16bit { upper; lower; } ->
    (* CR-someday mshinwell: This should really be checked for overflow, but
       seems hard... *)
    A.between_labels_16bit ~upper ~lower
  | Distance_between_labels_32bit { upper; lower; } ->
    (* CR-someday mshinwell: Same comment as for the 16 bit case. *)
    A.between_labels_32bit ~upper ~lower
  | Distance_between_labels_64bit { upper; lower; } ->
    A.between_labels_64bit ~upper ~lower
