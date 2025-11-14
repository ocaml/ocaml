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

(** Standard abbreviation table for OCaml DWARF.

    This module defines a fixed set of abbreviation codes that ALL
    compilation units use. This ensures that when multiple .o files
    are linked together, all CUs can reference offset 0 in the
    .debug_abbrev section and find the same table structure.

    This solves the multi-CU abbreviation offset issue without
    requiring link-time DWARF processing. *)

type standard_entry = {
  code : int;
  tag : Dwarf_tag.t;
  has_children : bool;
  attributes : (Dwarf_attributes.t * Dwarf_form.t) list;
}

(** Standard abbreviation codes used by all OCaml compilation units *)
let standard_table : standard_entry list = [
  (* Code 1: Compilation Unit *)
  {
    code = 1;
    tag = DW_TAG_compile_unit;
    has_children = true;
    attributes = [
      (DW_AT_name, DW_FORM_string);      (* DWARF 5: inline string *)
      (DW_AT_producer, DW_FORM_string);
      (DW_AT_comp_dir, DW_FORM_string);
      (DW_AT_language, DW_FORM_data1);
      (DW_AT_stmt_list, DW_FORM_sec_offset);  (* Pointer to line number program *)
    ];
  };

  (* Code 2: Subprogram without children (no parameters) *)
  {
    code = 2;
    tag = DW_TAG_subprogram;
    has_children = false;
    attributes = [
      (DW_AT_name, DW_FORM_string);      (* DWARF 5: inline string *)
      (DW_AT_low_pc, DW_FORM_addr);
      (DW_AT_high_pc, DW_FORM_addr);
      (DW_AT_external, DW_FORM_flag_present);
      (DW_AT_decl_file, DW_FORM_data1);  (* Link to source file in line table *)
    ];
  };

  (* Code 3: Subprogram with children (has parameters) *)
  {
    code = 3;
    tag = DW_TAG_subprogram;
    has_children = true;
    attributes = [
      (DW_AT_name, DW_FORM_string);      (* DWARF 5: inline string *)
      (DW_AT_low_pc, DW_FORM_addr);
      (DW_AT_high_pc, DW_FORM_addr);
      (DW_AT_external, DW_FORM_flag_present);
      (DW_AT_decl_file, DW_FORM_data1);  (* Link to source file in line table *)
    ];
  };

  (* Code 4: Formal parameter *)
  {
    code = 4;
    tag = DW_TAG_formal_parameter;
    has_children = false;
    attributes = [
      (DW_AT_name, DW_FORM_string);      (* DWARF 5: inline string *)
      (DW_AT_location, DW_FORM_exprloc);
    ];
  };

  (* Code 5: Formal parameter with type *)
  {
    code = 5;
    tag = DW_TAG_formal_parameter;
    has_children = false;
    attributes = [
      (DW_AT_name, DW_FORM_string);      (* DWARF 5: inline string *)
      (DW_AT_type, DW_FORM_ref4);
      (DW_AT_location, DW_FORM_exprloc);
    ];
  };

  (* Code 6: Base type *)
  {
    code = 6;
    tag = DW_TAG_base_type;
    has_children = false;
    attributes = [
      (DW_AT_name, DW_FORM_string);      (* DWARF 5: inline string *)
      (DW_AT_byte_size, DW_FORM_data1);
      (DW_AT_encoding, DW_FORM_data1);
    ];
  };

  (* Code 7: Pointer type *)
  {
    code = 7;
    tag = DW_TAG_pointer_type;
    has_children = false;
    attributes = [
      (DW_AT_byte_size, DW_FORM_data1);
      (DW_AT_type, DW_FORM_ref4);
    ];
  };

  (* Code 8: Subprogram with type (has return type) *)
  {
    code = 8;
    tag = DW_TAG_subprogram;
    has_children = true;
    attributes = [
      (DW_AT_name, DW_FORM_string);      (* DWARF 5: inline string *)
      (DW_AT_type, DW_FORM_ref4);
      (DW_AT_low_pc, DW_FORM_addr);
      (DW_AT_high_pc, DW_FORM_addr);
      (DW_AT_external, DW_FORM_flag_present);
      (DW_AT_decl_file, DW_FORM_data1);  (* Link to source file in line table *)
    ];
  };
]

(** Get the abbreviation code for a DIE based on its signature.
    Returns the standard code if it matches a known pattern,
    otherwise raises an error. *)
let get_code_for_die (die : Proto_die.t) : int =
  let tag = Proto_die.tag die in
  let has_children = Proto_die.has_children die in
  let attrs = Proto_die.attributes die in
  let attr_sig = List.map (fun (attr : Proto_die.attribute) ->
    (attr.attr, attr.form)
  ) attrs in

  (* Find matching entry in standard table *)
  let rec find_match = function
    | [] ->
        (* No match - this shouldn't happen with current OCaml DWARF usage *)
        failwith (Printf.sprintf
          "No standard abbreviation code for DIE: tag=%s has_children=%b attrs=%d"
          (Dwarf_tag.to_string tag)
          has_children
          (List.length attr_sig))
    | entry :: rest ->
        if entry.tag = tag &&
           entry.has_children = has_children &&
           entry.attributes = attr_sig then
          entry.code
        else
          find_match rest
  in
  find_match standard_table

(** Emit the standard abbreviation table.
    This is the same for ALL compilation units. *)
let emit_standard_table () : bytes =
  let buf = Buffer.create 256 in

  List.iter (fun entry ->
    (* Write abbreviation code (uleb128) *)
    Leb128.write_uleb128 buf entry.code;

    (* Write tag (uleb128) *)
    let tag_code = Dwarf_tag.to_code entry.tag in
    Leb128.write_uleb128 buf tag_code;

    (* Write has_children flag *)
    Buffer.add_char buf (if entry.has_children then '\001' else '\000');

    (* Write attributes as (name, form) pairs *)
    List.iter (fun (attr, form) ->
      Leb128.write_uleb128 buf (Dwarf_attributes.to_code attr);
      Leb128.write_uleb128 buf (Dwarf_form.to_code form);
    ) entry.attributes;

    (* Null terminator for attribute list *)
    Buffer.add_char buf '\000';
    Buffer.add_char buf '\000';
  ) standard_table;

  (* Null terminator for abbreviation table *)
  Buffer.add_char buf '\000';

  Bytes.of_string (Buffer.contents buf)

(** Check if a DIE matches one of the standard entries *)
let is_standard_die (die : Proto_die.t) : bool =
  try
    let _ = get_code_for_die die in
    true
  with Failure _ -> false
