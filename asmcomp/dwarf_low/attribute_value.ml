open Std_internal

type t = Attribute.t * Value.t

let create_low_pc ~address_label =
  Attribute.low_pc,
    Value.as_code_address_from_label address_label

let create_high_pc ~address_label =
  Attribute.high_pc,
    Value.as_code_address_from_label address_label

let create_producer ~producer_name =
  Attribute.producer, Value.as_string producer_name

let create_name ~source_file_path =  (* CR mshinwell: bad name for argument *)
  Attribute.name, Value.as_string source_file_path

let create_comp_dir ~directory =
  Attribute.comp_dir, Value.as_string directory

let create_stmt_list ~section_offset_label =
  Attribute.stmt_list,
    Value.as_four_byte_int_from_label section_offset_label

let create_external ~is_visible_externally =
  let flag = if is_visible_externally then 1 else 0 in
  Attribute.extern'l, Value.as_byte flag

(*
let create_location ~offset_from_start_of_debug_loc =
  Attribute.location,
    Value.as_four_byte_int offset_from_start_of_debug_loc
*)
let create_location ~location_list_label =
  Attribute.location,
    Value.as_code_address_from_label location_list_label

let create_type ~label_name =
  Attribute.typ',
    Value.as_code_address_from_label ("Ldie__" ^ label_name)

let create_encoding ~encoding =
  Attribute.encoding, Encoding_attribute.as_dwarf_value encoding

let create_byte_size ~byte_size =
  assert (byte_size >= 1 && byte_size <= 0xff); (* CR mshinwell: not assert *)
  Attribute.byte_size, Value.as_byte byte_size

let emit (_attr, value) ~emitter =
  Value.emit value ~emitter

let size (_attr, value) =
  Value.size value

let attribute (attr, _value) = attr
