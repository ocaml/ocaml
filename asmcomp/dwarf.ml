(* Emission of DWARF 3 debugging information.
   http://dwarfstd.org/doc/Dwarf3.pdf
*)

open Emitaux

module List = ListLabels
let sprintf = Printf.sprintf

(* CR mshinwell: fixme: copied from emit.mlp *)
let macosx = (Config.system = "macosx")
let emit_symbol s =
    if macosx then emit_string "_";
    Emitaux.emit_symbol '$' s

module Dwarf_value : sig
  type t

  val as_four_byte_int : int -> t
  val as_four_byte_int_from_label : string -> t
  val as_two_byte_int : int -> t
  val as_byte : int -> t
  val as_uleb128 : int -> t
  val as_string : string -> t
  val as_code_address_from_label : string -> t
  val as_code_address : int -> t

  val size : t -> int
  val emit : t -> unit
end = struct
  type t = [
  | `Four_byte_int of int
  | `Four_byte_int_from_label of string
  | `Two_byte_int of int
  | `Byte of int
  | `Uleb128 of int
  | `String of string
  | `Code_address_from_label of string
  | `Code_address of int
  ]

  exception Too_large_for_four_byte_int of int
  exception Too_large_for_two_byte_int of int
  exception Too_large_for_byte of int

  let as_four_byte_int i =
    if not (i >= 0 && i <= 0xffff_ffff) then
      raise (Too_large_for_four_byte_int i);
    `Four_byte_int i

  let as_four_byte_int_from_label l =
    `Four_byte_int_from_label l

  let as_two_byte_int i =
    if not (i >= 0 && i <= 0xffff) then
      raise (Too_large_for_two_byte_int i);
    `Two_byte_int i

  let as_byte i =
    if not (i >= 0 && i <= 0xff) then
      raise (Too_large_for_byte i);
    `Byte i

  let as_uleb128 i =
    assert (i >= 0);
    `Uleb128 i

  let as_string s =
    `String s

  let as_code_address_from_label s =
    `Code_address_from_label s

  let as_code_address i =
    `Code_address i

  let size = function
    | `Four_byte_int _ | `Four_byte_int_from_label _ -> 4
    | `Two_byte_int _ -> 2
    | `Byte _ -> 1
    | `Uleb128 i ->
      if i = 0 then 1
      else 1 + int_of_float (floor (log (float_of_int i) /. log 128.))
    | `String s -> 1 + String.length s
    | `Code_address_from_label _ | `Code_address _ -> 8

  let emit = function
    | `Four_byte_int i -> emit_string (sprintf "\t.long\t0x%x\n" i)
    | `Four_byte_int_from_label l -> emit_string (sprintf "\t.long\t%s\n" l)
    | `Two_byte_int i -> emit_string (sprintf "\t.value\t0x%x\n" i)
    | `Byte b -> emit_string (sprintf "\t.byte\t0x%x\n" b)
    | `Uleb128 i -> emit_string (sprintf "\t.uleb128\t0x%x\n" i)
    | `String s -> emit_string (sprintf "\t.string\t\"%s\"\n" s)
    | `Code_address_from_label s -> emit_string (sprintf "\t.quad\t%s\n" s)
    | `Code_address i -> emit_string (sprintf "\t.quad\t0x%x\n" i)
end

module Dwarf_child_determination : sig
  type t

  val yes : t
  val no : t

  val emit : t -> unit
end = struct
  type t = [
  | `DW_CHILDREN_no
  | `DW_CHILDREN_yes
  ]

  let encode = function
    | `DW_CHILDREN_no -> 0x00
    | `DW_CHILDREN_yes -> 0x01

  let no = `DW_CHILDREN_no
  let yes = `DW_CHILDREN_yes

  let emit t =
    Dwarf_value.emit (Dwarf_value.as_byte (encode t))
end

module Dwarf_tag : sig
  type t

  val compile_unit : t
  val subprogram : t

  val child_determination : t -> Dwarf_child_determination.t
  val emit : t -> unit
end = struct
  type t = [
  | `DW_TAG_compile_unit
  | `DW_TAG_subprogram
  ]

  let encode = function
    | `DW_TAG_compile_unit -> 0x11
    | `DW_TAG_subprogram -> 0x2e

  let child_determination = function
    | `DW_TAG_compile_unit -> Dwarf_child_determination.yes
    | `DW_TAG_subprogram -> Dwarf_child_determination.no

  let compile_unit = `DW_TAG_compile_unit
  let subprogram = `DW_TAG_subprogram

  let emit t =
    Dwarf_value.emit (Dwarf_value.as_uleb128 (encode t))
end

module Dwarf_form : sig
  type t

  val addr : t
  val data4 : t
  val string : t
  val flag : t

  val emit : t -> unit
end = struct
  type t = [
  | `DW_FORM_addr
  | `DW_FORM_string
  | `DW_FORM_data4
  | `DW_FORM_flag
  ]

  let encode = function
    | `DW_FORM_addr -> 0x1
    | `DW_FORM_data4 -> 0x6
    | `DW_FORM_string -> 0x8
    | `DW_FORM_flag -> 0x0c

  let addr = `DW_FORM_addr
  let data4 = `DW_FORM_data4
  let string = `DW_FORM_string
  let flag = `DW_FORM_flag

  let emit t =
    Dwarf_value.emit (Dwarf_value.as_uleb128 (encode t))
end

module Dwarf_attribute : sig
  type t

  val low_pc : t
  val high_pc : t
  val producer : t
  val name : t
  val comp_dir : t
  val stmt_list : t
  val extern'l : t

  val emit_followed_by_form : t -> unit
end = struct
  type t = [
  | `DW_AT_low_pc
  | `DW_AT_high_pc
  | `DW_AT_name
  | `DW_AT_comp_dir
  | `DW_AT_producer
  | `DW_AT_stmt_list
  | `DW_AT_external
  ]

  let encode = function
    | `DW_AT_low_pc -> 0x11
    | `DW_AT_high_pc -> 0x12
    | `DW_AT_name -> 0x03
    | `DW_AT_comp_dir -> 0x1b
    | `DW_AT_producer -> 0x25
    | `DW_AT_stmt_list -> 0x10
    | `DW_AT_external -> 0x3f

  let form = function
    | `DW_AT_low_pc -> Dwarf_form.addr
    | `DW_AT_high_pc -> Dwarf_form.addr
    | `DW_AT_name -> Dwarf_form.string
    | `DW_AT_comp_dir -> Dwarf_form.string
    | `DW_AT_producer -> Dwarf_form.string
    | `DW_AT_stmt_list -> Dwarf_form.data4
    | `DW_AT_external -> Dwarf_form.flag

  let low_pc = `DW_AT_low_pc
  let high_pc = `DW_AT_high_pc
  let producer = `DW_AT_producer
  let name = `DW_AT_name
  let comp_dir = `DW_AT_comp_dir
  let stmt_list = `DW_AT_stmt_list
  let extern'l = `DW_AT_external

  let emit_followed_by_form t =
    Dwarf_value.emit (Dwarf_value.as_uleb128 (encode t));
    Dwarf_form.emit (form t)
end

module Dwarf_attribute_value = struct
  type t = Dwarf_attribute.t * Dwarf_value.t

  let create_low_pc ~address_label =
    Dwarf_attribute.low_pc,
      Dwarf_value.as_code_address_from_label address_label

  let create_high_pc ~address_label =
    Dwarf_attribute.high_pc,
      Dwarf_value.as_code_address_from_label address_label

  let create_producer ~producer_name =
    Dwarf_attribute.producer, Dwarf_value.as_string producer_name

  let create_name ~source_file_path =  (* CR mshinwell: bad name for argument *)
    Dwarf_attribute.name, Dwarf_value.as_string source_file_path

  let create_comp_dir ~directory =
    Dwarf_attribute.comp_dir, Dwarf_value.as_string directory

  let create_stmt_list ~section_offset_label =
    Dwarf_attribute.stmt_list,
      Dwarf_value.as_four_byte_int_from_label section_offset_label

  let create_external ~is_visible_externally =
    let flag = if is_visible_externally then 1 else 0 in
    Dwarf_attribute.extern'l, Dwarf_value.as_byte flag

  let emit (_attr, value) =
    Dwarf_value.emit value

  let size (_attr, value) =
    Dwarf_value.size value

  let attribute (attr, _value) = attr
end

module Dwarf_version : sig
  type t

  val two : t
  val three : t
  val four : t

  val size : t -> int
  val emit : t -> unit
end = struct
  type t = [
  | `Dwarf_2
  | `Dwarf_3
  | `Dwarf_4
  ]

  let encode = function
    | `Dwarf_2 -> 2
    | `Dwarf_3 -> 3
    | `Dwarf_4 -> 4

  let two = `Dwarf_2
  let three = `Dwarf_3
  let four = `Dwarf_4

  let emit t =
    Dwarf_value.emit (Dwarf_value.as_two_byte_int (encode t))

  let size _t = 2
end

module Dwarf_abbreviation_code : sig
  type t

  val of_int : int -> t
  val null : unit -> t

  val size : t -> int
  val emit : t -> unit
end = struct
  type t = Dwarf_value.t

  exception Bad_abbreviation_code of int

  let of_int i =
    if i < 1 then raise (Bad_abbreviation_code i);
    Dwarf_value.as_uleb128 i

  let null () =
    Dwarf_value.as_uleb128 0

  let emit t =
    Dwarf_value.emit t

  let size t =
    Dwarf_value.size t
end

module Dwarf_abbreviations_table_entry = struct
  type t = {
    abbreviation_code : Dwarf_abbreviation_code.t;
    tag : Dwarf_tag.t;
    attributes : Dwarf_attribute.t list;
  }

  let create ~abbreviation_code ~tag ~attributes =
    { abbreviation_code;
      tag;
      attributes;
    }

  let emit t =
    Dwarf_abbreviation_code.emit t.abbreviation_code;
    Dwarf_tag.emit t.tag;
    Dwarf_child_determination.emit (Dwarf_tag.child_determination t.tag);
    List.iter t.attributes ~f:Dwarf_attribute.emit_followed_by_form;
    Dwarf_value.emit (Dwarf_value.as_uleb128 0);
    Dwarf_value.emit (Dwarf_value.as_uleb128 0)
end

module Dwarf_abbreviations_table = struct
  type t = Dwarf_abbreviations_table_entry.t list

  let create abbrev_table_entries =
    abbrev_table_entries

  let emit t =
    List.iter t ~f:Dwarf_abbreviations_table_entry.emit;
    Dwarf_value.emit (Dwarf_value.as_uleb128 0)
end

module Dwarf_aranges_table = struct
  let emit ~start_of_code_label ~end_of_code_label =
    let distance_label =
      sprintf "%s-%s" end_of_code_label start_of_code_label
    in
    let address_width_in_bytes_on_target = Dwarf_value.as_byte 8 in
    let values = [
      Dwarf_value.as_two_byte_int 2;  (* section version number *)
      Dwarf_value.as_four_byte_int 0;
      address_width_in_bytes_on_target;
      Dwarf_value.as_byte 0;
      Dwarf_value.as_two_byte_int 0;
      Dwarf_value.as_two_byte_int 0;
      Dwarf_value.as_code_address_from_label start_of_code_label;
      Dwarf_value.as_code_address_from_label distance_label;
      Dwarf_value.as_code_address 0;
      Dwarf_value.as_code_address 0;
    ]
    in
    let size =
      List.fold_left values
        ~init:0
        ~f:(fun size value -> size + Dwarf_value.size value)
    in
    Dwarf_value.emit (Dwarf_value.as_four_byte_int size);
    List.iter values ~f:Dwarf_value.emit
end

module Dwarf_debugging_information_entry : sig
  type t

  val create : label_name:string
    -> abbreviation_code:Dwarf_abbreviation_code.t
    -> tag:Dwarf_tag.t
    -> attribute_values:Dwarf_attribute_value.t list
    -> t

  val null : unit -> t

  val size : t -> int
  val emit : t -> unit

  val to_abbreviations_table_entry : t
    -> Dwarf_abbreviations_table_entry.t option
end = struct
  type t = {
    label_name : string;
    abbreviation_code : Dwarf_abbreviation_code.t;
    tag : Dwarf_tag.t option;
    attribute_values : Dwarf_attribute_value.t list;
  }

  let create ~label_name ~abbreviation_code ~tag ~attribute_values =
    { label_name;
      abbreviation_code;
      tag = Some tag;
      attribute_values;
    }

  let emit t =
    (* CR mshinwell: share code with below *)
    emit_string ".Ldie__";
    emit_symbol t.label_name;
    emit_string ":\n";
    Dwarf_abbreviation_code.emit t.abbreviation_code;
    List.iter t.attribute_values ~f:Dwarf_attribute_value.emit

  let size t =
    List.fold_left t.attribute_values
      ~init:(Dwarf_abbreviation_code.size t.abbreviation_code)
      ~f:(fun size attribute_value ->
            size + Dwarf_attribute_value.size attribute_value)

  let null () =
    { label_name = "null";
      abbreviation_code = Dwarf_abbreviation_code.null ();
      tag = None;
      attribute_values = [];
    }

  let to_abbreviations_table_entry t =
    match t.tag with
    | Some tag ->
      let attributes =
        List.map t.attribute_values ~f:Dwarf_attribute_value.attribute
      in
      let entry =
        Dwarf_abbreviations_table_entry.create
          ~abbreviation_code:t.abbreviation_code
          ~tag
          ~attributes
      in
      Some entry
    | None -> None
end

module Dwarf_debug_info_section : sig
  type t

  val create :
       tags_with_attribute_values:
         (string * Dwarf_tag.t * (Dwarf_attribute_value.t list)) list
    -> t

  val size : t -> int
  val emit : t -> unit

  val to_abbreviations_table : t -> Dwarf_abbreviations_table.t
end = struct
  type t = {
    dies : Dwarf_debugging_information_entry.t list;
  }

  let create ~tags_with_attribute_values =
    let next_abbreviation_code = ref 1 in
    let dies =
      List.map tags_with_attribute_values
        ~f:(fun (label_name, tag, attribute_values) ->
              let abbreviation_code =
                Dwarf_abbreviation_code.of_int !next_abbreviation_code
              in
              next_abbreviation_code := !next_abbreviation_code + 1;
              Dwarf_debugging_information_entry.create ~label_name
                ~abbreviation_code
                ~tag
                ~attribute_values)
    in
    { dies = dies @ [Dwarf_debugging_information_entry.null ()];
    }

  let dwarf_version = Dwarf_version.three
  let debug_abbrev_offset =
    Dwarf_value.as_four_byte_int_from_label ".Ldebug_abbrev0"
  let address_width_in_bytes_on_target = Dwarf_value.as_byte 8

  let size_without_first_word t =
    let total_die_size =
      List.fold_left t.dies
        ~init:0
        ~f:(fun size die -> size + Dwarf_debugging_information_entry.size die)
    in
    Dwarf_version.size dwarf_version
      + Dwarf_value.size debug_abbrev_offset
      + Dwarf_value.size address_width_in_bytes_on_target
      + total_die_size

  let size t = 4 + size_without_first_word t

  let emit t =
    let size = size_without_first_word t in
    Dwarf_value.emit (Dwarf_value.as_four_byte_int size);
    Dwarf_version.emit dwarf_version;
    Dwarf_value.emit debug_abbrev_offset;
    Dwarf_value.emit address_width_in_bytes_on_target;
    List.iter t.dies ~f:Dwarf_debugging_information_entry.emit

  let to_abbreviations_table t =
    let entries =
      List.fold_right t.dies
        ~init:[]
        ~f:(fun die entries ->
              match
                Dwarf_debugging_information_entry.
                  to_abbreviations_table_entry die
              with
              | None -> entries
              | Some entry -> entry::entries)
    in
    Dwarf_abbreviations_table.create entries
end

module Dwarf_pubnames_table = struct
  let emit ~externally_visible_functions ~debug_info =
    let size_of_entry function_name =
      4 + (String.length function_name) + 1
    in
    let size_of_all_entries =
      List.fold_left externally_visible_functions
        ~init:0
        ~f:(fun size entry -> size + size_of_entry entry)
    in
    let total_size = 2 + 4 + 4 + size_of_all_entries + 4 in
    let write_offset_name_pair function_name =
      (* CR mshinwell: should use [Dwarf_value.emit], no? *)
      emit_string "\t.long\t.Ldie__";
      emit_symbol function_name;
      emit_string "-.Ldie__compile_unit\n";
      Dwarf_value.emit (Dwarf_value.as_string function_name)
    in
    Dwarf_value.emit (Dwarf_value.as_four_byte_int total_size);
    Dwarf_value.emit (Dwarf_value.as_two_byte_int 2);  (* version number *)
    Dwarf_value.emit (Dwarf_value.as_four_byte_int_from_label ".Ldebug_info0");
    Dwarf_value.emit
      (Dwarf_value.as_four_byte_int (Dwarf_debug_info_section.size debug_info));
    List.iter externally_visible_functions ~f:write_offset_name_pair;
    Dwarf_value.emit (Dwarf_value.as_four_byte_int 0)
end

let size_of_dwarf_attribute_value_list attr_vals =
  List.fold_left attr_vals
    ~init:0
    ~f:(fun size attr_val -> size + Dwarf_attribute_value.size attr_val)

let emit_section_declaration ~section_name =
  emit_string (sprintf "\t.section\t%s,\"\",@progbits\n" section_name)

let emit_switch_to_section ~section_name =
  emit_string (sprintf "\t.section\t%s\n" section_name)

let emit_label_declaration ~label_name =
  emit_string (sprintf "%s:\n" label_name)

let emit_debugging_info_prologue () =
  emit_section_declaration ~section_name:".debug_abbrev";
  emit_label_declaration ~label_name:".Ldebug_abbrev0";
  emit_section_declaration ~section_name:".debug_line";
  emit_label_declaration ~label_name:".Ldebug_line0"

let emit_debugging_info_epilogue ~source_file_path
                                 ~start_of_code_label ~end_of_code_label
                                 ~externally_visible_functions =
  let producer_name = sprintf "ocamlopt %s" Sys.ocaml_version in
  let compile_unit_attribute_values =
    let common = [
      Dwarf_attribute_value.create_producer ~producer_name;
      Dwarf_attribute_value.create_low_pc ~address_label:start_of_code_label;
      Dwarf_attribute_value.create_high_pc ~address_label:end_of_code_label;
      Dwarf_attribute_value.create_stmt_list
        ~section_offset_label:".Ldebug_line0";
    ]
    in
    match source_file_path with
    | None -> common
    | Some source_file_path ->
      (Dwarf_attribute_value.create_name ~source_file_path)::common
  in
  let function_symbol_attribute_values symbol =
    symbol, Dwarf_tag.subprogram, [
      Dwarf_attribute_value.create_name ~source_file_path:symbol;
      Dwarf_attribute_value.create_comp_dir ~directory:(Sys.getcwd ());
      Dwarf_attribute_value.create_external ~is_visible_externally:true;
    ]
  in
  let tags_with_attribute_values = [
    "compile_unit", Dwarf_tag.compile_unit, compile_unit_attribute_values;
  ] @ List.map externally_visible_functions ~f:function_symbol_attribute_values
  in
  let debug_info =
    Dwarf_debug_info_section.create ~tags_with_attribute_values
  in
  let debug_abbrev =
    Dwarf_debug_info_section.to_abbreviations_table debug_info
  in
  emit_section_declaration ~section_name:".debug_info";
  emit_label_declaration ".Ldebug_info0";
  Dwarf_debug_info_section.emit debug_info;
  emit_switch_to_section ~section_name:".debug_abbrev";
  Dwarf_abbreviations_table.emit debug_abbrev;
  emit_section_declaration ~section_name:".debug_pubnames";
  Dwarf_pubnames_table.emit ~externally_visible_functions
    ~debug_info;
  emit_section_declaration ~section_name:".debug_aranges";
  Dwarf_aranges_table.emit ~start_of_code_label ~end_of_code_label
