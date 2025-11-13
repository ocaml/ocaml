(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  (* Compilation unit info *)
  producer : string;
  comp_dir : string;
  language : Dwarf_language.t;

  (* DIE tree *)
  mutable dies : Proto_die.t list;
  mutable cu_die : Proto_die.t option;

  (* Tables *)
  location_lists : Location_list_table.t;
  range_lists : Range_list_table.t;
  line_number_table : Line_number_table.t;
}

let create ~producer ~comp_dir ~language () =
  let line_table = Line_number_table.create () in
  Line_number_table.set_comp_dir line_table comp_dir;
  {
    producer;
    comp_dir;
    language;
    dies = [];
    cu_die = None;
    location_lists = Location_list_table.create ();
    range_lists = Range_list_table.create ();
    line_number_table = line_table;
  }

let create_cu_die t =
  let cu = Proto_die.create Dwarf_tag.DW_TAG_compile_unit in
  let cu = Proto_die.with_name cu (Filename.basename t.comp_dir) in
  let cu = Proto_die.add_attribute cu {
    attr = DW_AT_producer;
    value = String t.producer;
    form = DW_FORM_strp;
  } in
  let cu = Proto_die.add_attribute cu {
    attr = DW_AT_comp_dir;
    value = String t.comp_dir;
    form = DW_FORM_strp;
  } in
  let cu = Proto_die.add_attribute cu {
    attr = DW_AT_language;
    value = Constant (Int (Dwarf_language.to_code t.language));
    form = DW_FORM_data1;
  } in
  Proto_die.set_has_children cu true

let add_die t die =
  t.dies <- die :: t.dies

let add_location_list t entries =
  Location_list_table.add_location_list t.location_lists entries

let add_range_list t entries =
  Range_list_table.add_range_list t.range_lists entries

let add_line_number_entry t ~address ~file ~line ~column =
  let entry : Line_number_table.entry = {
    address;
    position = { file; line; column };
    is_stmt = true;
    basic_block = false;
    prologue_end = false;
    epilogue_begin = false;
  } in
  Line_number_table.add_entry t.line_number_table entry

let compilation_unit t =
  match t.cu_die with
  | Some cu -> cu
  | None ->
      let cu = create_cu_die t in
      t.cu_die <- Some cu;
      cu

let all_dies t = List.rev t.dies

let location_list_table t = t.location_lists

let range_list_table t = t.range_lists

(* Type DIE creation *)

(** Create a base type DIE (e.g., int, float, value) *)
let create_base_type ~name ~byte_size ~encoding =
  let die = Proto_die.create Dwarf_tag.DW_TAG_base_type in
  let die = Proto_die.with_name die name in
  let die = Proto_die.with_byte_size die byte_size in
  let die = Proto_die.with_encoding die encoding in
  die

(** Initialize standard OCaml types and add them to the world.
    Returns offsets for each type so they can be referenced. *)
type type_offsets = {
  ocaml_value : int;  (* Generic OCaml value type *)
  ocaml_int : int;    (* OCaml integer type *)
}

let add_standard_types t =
  (* We need to calculate DIE offsets. Types should be added first.
     For now, we'll use placeholder offsets. In a real implementation,
     we'd need to calculate actual offsets after emission. *)

  (* OCaml value: 8-byte pointer to any OCaml value *)
  let value_die = create_base_type
    ~name:"value"
    ~byte_size:8
    ~encoding:Dwarf_encoding.DW_ATE_address
  in
  add_die t value_die;

  (* OCaml int: immediate integer (tagged) *)
  let int_die = create_base_type
    ~name:"int"
    ~byte_size:8
    ~encoding:Dwarf_encoding.DW_ATE_signed
  in
  add_die t int_die;

  (* Return placeholder offsets - these will be resolved during emission *)
  { ocaml_value = 0; ocaml_int = 0 }

(* Section emission - simplified versions *)

type relocation = {
  offset : int;  (* Offset in section where relocation is needed *)
  label : string;  (* Label name to emit *)
}

type str_relocation = {
  offset : int;  (* Offset in .debug_info where relocation is needed *)
  str_offset : int;  (* Offset within .debug_str section *)
}

type section_data = {
  debug_info : bytes;
  debug_info_relocs : relocation list;  (* Address relocations *)
  debug_str_relocs : str_relocation list;  (* String table relocations *)
  debug_abbrev : bytes;
  debug_str : bytes;
  debug_line : bytes option;
  debug_loc : bytes option;
  debug_ranges : bytes option;
}

(* Collect all strings from DIEs that use DW_FORM_strp *)
let rec collect_strings die acc =
  (* Collect strings from this DIE's attributes *)
  let acc = List.fold_left (fun acc (attr : Proto_die.attribute) ->
    match attr.value, attr.form with
    | String s, DW_FORM_strp -> s :: acc
    | _ -> acc
  ) acc (Proto_die.attributes die) in
  (* Recursively collect from children *)
  List.fold_left (fun acc child ->
    collect_strings child acc
  ) acc (Proto_die.children die)

(* Build string table and return (bytes, offset_map) *)
let build_string_table strings =
  (* Remove duplicates and sort for determinism *)
  let unique_strings = List.sort_uniq String.compare strings in
  let buf = Buffer.create 1024 in
  let offsets = ref [] in
  List.iter (fun s ->
    let offset = Buffer.length buf in
    offsets := (s, offset) :: !offsets;
    Buffer.add_string buf s;
    Buffer.add_char buf '\000'  (* Null terminator *)
  ) unique_strings;
  (Bytes.of_string (Buffer.contents buf), List.rev !offsets)

(* emit_debug_str is no longer needed - string table is built in emit() *)

let emit_debug_abbrev _t =
  (* Emit the standard abbreviation table.
     This is identical for ALL compilation units, ensuring that
     when multiple .o files are linked, all CUs can reference
     offset 0 and find the same table structure. *)
  Standard_abbrevs.emit_standard_table ()

let write_attribute_value buf (value : Dwarf_value.t) (form : Dwarf_form.t) str_offsets relocs str_relocs =
  match form, value with
  | DW_FORM_addr, Address addr ->
      (* 8-byte address for 64-bit systems *)
      let bytes = Bytes.create 8 in
      Bytes.set_int64_le bytes 0 addr;
      Buffer.add_bytes buf bytes
  | DW_FORM_addr, Label_address label ->
      (* 8-byte address that needs relocation *)
      let offset = Buffer.length buf in
      relocs := { offset; label } :: !relocs;
      (* Write placeholder zeros - will be replaced by assembler *)
      let bytes = Bytes.create 8 in
      Buffer.add_bytes buf bytes
  | DW_FORM_data1, Constant (Int n) ->
      Buffer.add_char buf (Char.chr (n land 0xff))
  | DW_FORM_data2, Constant (Int n) ->
      Buffer.add_char buf (Char.chr (n land 0xff));
      Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff))
  | DW_FORM_data4, Constant (Int n) ->
      for i = 0 to 3 do
        Buffer.add_char buf (Char.chr ((n lsr (i * 8)) land 0xff))
      done
  | DW_FORM_data8, Constant (Int64 n) ->
      let bytes = Bytes.create 8 in
      Bytes.set_int64_le bytes 0 n;
      Buffer.add_bytes buf bytes
  | DW_FORM_udata, Constant (Int n) ->
      Leb128.write_uleb128 buf n
  | DW_FORM_sdata, Constant (Int n) ->
      Leb128.write_sleb128 buf n
  | DW_FORM_string, String s ->
      Buffer.add_string buf s;
      Buffer.add_char buf '\000'
  | DW_FORM_strp, String s ->
      (* Look up string offset in string table for relocation *)
      let str_offset = match List.assoc_opt s str_offsets with
        | Some off -> off
        | None -> 0
      in
      (* Record relocation for this string table reference *)
      let offset = Buffer.length buf in
      str_relocs := { offset; str_offset } :: !str_relocs;
      (* Write placeholder zeros - will be replaced by relocation *)
      for _ = 0 to 3 do
        Buffer.add_char buf '\000'
      done
  | DW_FORM_flag, Flag b ->
      Buffer.add_char buf (if b then '\001' else '\000')
  | DW_FORM_flag_present, Flag true ->
      () (* No value bytes for flag_present *)
  | DW_FORM_sec_offset, Sec_offset offset ->
      (* Write 4-byte section offset *)
      for i = 0 to 3 do
        Buffer.add_char buf (Char.chr ((offset lsr (i * 8)) land 0xff))
      done
  | DW_FORM_ref4, Reference (Offset offset) ->
      (* Write 4-byte reference *)
      for i = 0 to 3 do
        Buffer.add_char buf (Char.chr ((offset lsr (i * 8)) land 0xff))
      done
  | DW_FORM_exprloc, Expr_loc expr ->
      (* Write length as ULEB128, then expression bytes *)
      Leb128.write_uleb128 buf (Bytes.length expr);
      Buffer.add_bytes buf expr
  | DW_FORM_block, Block block ->
      (* Variable-length block with ULEB128 length *)
      Leb128.write_uleb128 buf (Bytes.length block);
      Buffer.add_bytes buf block
  | DW_FORM_block1, Block block ->
      (* 1-byte length - must be <= 255 *)
      let len = Bytes.length block in
      if len > 255 then
        failwith (Printf.sprintf "DW_FORM_block1: block too large (%d bytes, max 255)" len);
      Buffer.add_char buf (Char.chr len);
      Buffer.add_bytes buf block
  | DW_FORM_block2, Block block ->
      (* 2-byte length - must be <= 65535 *)
      let len = Bytes.length block in
      if len > 65535 then
        failwith (Printf.sprintf "DW_FORM_block2: block too large (%d bytes, max 65535)" len);
      Buffer.add_char buf (Char.chr (len land 0xff));
      Buffer.add_char buf (Char.chr ((len lsr 8) land 0xff));
      Buffer.add_bytes buf block
  | DW_FORM_block4, Block block ->
      (* 4-byte length *)
      let len = Bytes.length block in
      for i = 0 to 3 do
        Buffer.add_char buf (Char.chr ((len lsr (i * 8)) land 0xff))
      done;
      Buffer.add_bytes buf block
  | _ ->
      (* Unsupported form/value combination - emit empty *)
      ()

(* Build a map from proto_die to abbrev_code using standard codes *)
let build_abbrev_map cu_with_children =
  (* Build a hashtable mapping proto_die -> standard abbrev_code *)
  let die_map = Hashtbl.create 100 in

  (* Recursive function to assign standard codes *)
  let rec assign_codes proto_die =
    (* Get the standard code for this DIE *)
    let code = Standard_abbrevs.get_code_for_die proto_die in
    Hashtbl.add die_map proto_die code;

    (* Recursively process children *)
    List.iter assign_codes (Proto_die.children proto_die)
  in

  assign_codes cu_with_children;
  die_map

let rec write_die buf die die_map str_offsets relocs_ref str_relocs_ref =
  (* Look up abbreviation code for this DIE *)
  let abbrev_code = try Hashtbl.find die_map die with Not_found -> 1 in
  (* Write abbreviation code *)
  Leb128.write_uleb128 buf abbrev_code;
  (* Write attribute values in the order they appear in the abbreviation *)
  List.iter (fun (attr : Proto_die.attribute) ->
    write_attribute_value buf attr.value attr.form str_offsets relocs_ref str_relocs_ref
  ) (Proto_die.attributes die);
  (* Recursively write children *)
  List.iter (fun child ->
    write_die buf child die_map str_offsets relocs_ref str_relocs_ref
  ) (Proto_die.children die);
  (* Write null DIE to terminate children list if this DIE has children *)
  if Proto_die.has_children die && List.length (Proto_die.children die) > 0 then
    Buffer.add_char buf '\000'

let emit_debug_info_with_str_offsets t str_offsets =
  let buf = Buffer.create 2048 in

  (* Build DIE tree *)
  let cu = compilation_unit t in
  let cu_with_children = Proto_die.add_children cu (all_dies t) in

  (* Build abbreviation code map for all DIEs *)
  let die_map = build_abbrev_map cu_with_children in

  (* Track relocations for label addresses and string table references *)
  let relocs_ref = ref [] in
  let str_relocs_ref = ref [] in

  (* Start building DIEs in a separate buffer to calculate length *)
  let die_buf = Buffer.create 2048 in

  (* Write CU DIE and all its children with proper abbrev codes *)
  write_die die_buf cu_with_children die_map str_offsets relocs_ref str_relocs_ref;

  let die_bytes = Buffer.contents die_buf in
  let die_length = String.length die_bytes in

  (* Calculate unit length:
     version (2) + abbrev_offset (4) + address_size (1) + DIEs *)
  let unit_length = 2 + 4 + 1 + die_length in

  (* CU header offset - relocations in DIEs need to be offset by this *)
  let cu_header_size = 4 + 2 + 4 + 1 in  (* length + version + abbrev_offset + address_size *)

  (* Write compilation unit header *)
  (* Unit length (4 bytes, not including the length field itself) *)
  for i = 0 to 3 do
    Buffer.add_char buf (Char.chr ((unit_length lsr (i * 8)) land 0xff))
  done;
  (* DWARF version (2 bytes) *)
  Buffer.add_string buf "\x04\x00"; (* Version 4 *)
  (* Abbreviation table offset (4 bytes) - always 0 for first CU *)
  Buffer.add_string buf "\x00\x00\x00\x00";
  (* Address size (1 byte) *)
  Buffer.add_char buf '\x08'; (* 64-bit *)

  (* Append the DIE data *)
  Buffer.add_string buf die_bytes;

  (* Adjust relocation offsets for CU header *)
  let relocs = List.map (fun (r : relocation) -> { r with offset = r.offset + cu_header_size }) (List.rev !relocs_ref) in
  let str_relocs = List.map (fun (r : str_relocation) -> { r with offset = r.offset + cu_header_size }) (List.rev !str_relocs_ref) in

  (Bytes.of_string (Buffer.contents buf), relocs, str_relocs)

let emit t =
  let line_bytes =
    let files = Line_number_table.files t.line_number_table in
    if List.length files = 0 then None
    else Some (Line_number_table.emit t.line_number_table)
  in

  (* Build string table once to avoid inconsistency *)
  let cu = compilation_unit t in
  let cu_with_children = Proto_die.add_children cu (all_dies t) in
  let strings = collect_strings cu_with_children [] in
  let str_bytes, str_offsets = build_string_table strings in

  (* Emit debug_info using the same string offsets *)
  let debug_info_bytes, debug_info_relocs, debug_str_relocs =
    emit_debug_info_with_str_offsets t str_offsets in

  {
    debug_info = debug_info_bytes;
    debug_info_relocs = debug_info_relocs;
    debug_str_relocs = debug_str_relocs;
    debug_abbrev = emit_debug_abbrev t;
    debug_str = str_bytes;
    debug_line = line_bytes;
    debug_loc =
      if Location_list_table.is_empty t.location_lists then None
      else Some (Bytes.create 0); (* Placeholder *)
    debug_ranges =
      if Range_list_table.is_empty t.range_lists then None
      else Some (Bytes.create 0); (* Placeholder *)
  }

let print ppf t =
  Format.fprintf ppf "@[<v>DWARF World:";
  Format.fprintf ppf "@,Producer: %s" t.producer;
  Format.fprintf ppf "@,Compilation Directory: %s" t.comp_dir;
  Format.fprintf ppf "@,Language: %a" Dwarf_language.print t.language;
  Format.fprintf ppf "@,Top-level DIEs: %d" (List.length t.dies);
  Format.fprintf ppf "@,Location lists: %d" (Location_list_table.count t.location_lists);
  Format.fprintf ppf "@,Range lists: %d" (Range_list_table.count t.range_lists);
  Format.fprintf ppf "@]"
