(* Emission of DWARF 3 debugging information.
   http://dwarfstd.org/doc/Dwarf3.pdf
*)

open Emitaux

module Mini_core = struct
  module List = struct
    include ListLabels
    let fold = fold_left
  end

  let sprintf = Printf.sprintf
end

include Mini_core  (* a glimpse of another world *)

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
  val as_leb128 : int -> t
  val as_string : string -> t
  val as_code_address_from_label : string -> t
  val as_code_address_from_label_diff : string -> string -> t
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
  | `Leb128 of int
  | `String of string
  | `Code_address_from_label of string
  | `Code_address_from_label_diff of string * string
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

  exception Negative_leb128_not_yet_thought_about
  let as_leb128 i =
    if i < 0 then raise Negative_leb128_not_yet_thought_about;
    `Leb128 i

  let as_string s =
    `String s

  let as_code_address_from_label s =
    `Code_address_from_label s

  (* CR mshinwell: this mangling stuff is crap, and needs to be fixed *)
  let as_code_address_from_label_diff s2 s1 =
    `Code_address_from_label_diff (s2, s1)

  let as_code_address i =
    `Code_address i

  let size = function
    | `Four_byte_int _ | `Four_byte_int_from_label _ -> 4
    | `Two_byte_int _ -> 2
    | `Byte _ -> 1
    | `Uleb128 i | `Leb128 i ->
      if i = 0 then 1
      else 1 + int_of_float (floor (log (float_of_int i) /. log 128.))
    | `String s -> 1 + String.length s
    | `Code_address_from_label _ | `Code_address _
    | `Code_address_from_label_diff _ -> 8

  let emit = function
    | `Four_byte_int i -> emit_string (sprintf "\t.long\t0x%x\n" i)
    | `Four_byte_int_from_label l ->
      emit_string "\t.long\t";
      emit_symbol l;
      emit_string "\n"
    | `Two_byte_int i -> emit_string (sprintf "\t.value\t0x%x\n" i)
    | `Byte b -> emit_string (sprintf "\t.byte\t0x%x\n" b)
    | `Uleb128 i -> emit_string (sprintf "\t.uleb128\t0x%x\n" i)
    | `Leb128 i -> emit_string (sprintf "\t.sleb128\t0x%x\n" i)
    | `String s -> emit_string (sprintf "\t.string\t\"%s\"\n" s)
    | `Code_address_from_label s ->
      emit_string "\t.quad\t";
      emit_symbol s;
      emit_string "\n"
    | `Code_address_from_label_diff (s2, s1) ->
      emit_string "\t.quad\t";
      emit_symbol s2;
      emit_string " - ";
      emit_symbol s1;
      emit_string "\n"
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
  val subprogram_with_no_children : t
  val formal_parameter : t
  val variable : t
  val base_type : t

  val child_determination : t -> Dwarf_child_determination.t
  val emit : t -> unit
end = struct
  type t = [
  | `DW_TAG_compile_unit
  | `DW_TAG_subprogram
  | `DW_TAG_subprogram__no_children
  | `DW_TAG_formal_parameter
  | `DW_TAG_variable
  | `DW_TAG_base_type
  ]

  let encode = function
    | `DW_TAG_compile_unit -> 0x11
    | `DW_TAG_subprogram -> 0x2e
    | `DW_TAG_subprogram__no_children -> 0x2e
    | `DW_TAG_formal_parameter -> 0x05
    | `DW_TAG_variable -> 0x34
    | `DW_TAG_base_type -> 0x35

  (* CR mshinwell: "__no_children" is a hack *)

  let child_determination = function
    | `DW_TAG_compile_unit -> Dwarf_child_determination.yes
    | `DW_TAG_subprogram -> Dwarf_child_determination.yes
    | `DW_TAG_subprogram__no_children -> Dwarf_child_determination.no
    | `DW_TAG_formal_parameter -> Dwarf_child_determination.no
    | `DW_TAG_variable -> Dwarf_child_determination.no
    | `DW_TAG_base_type -> Dwarf_child_determination.no

  let compile_unit = `DW_TAG_compile_unit
  let subprogram = `DW_TAG_subprogram
  let subprogram_with_no_children = `DW_TAG_subprogram__no_children
  let formal_parameter = `DW_TAG_formal_parameter
  let variable = `DW_TAG_variable
  let base_type = `DW_TAG_base_type

  let emit t =
    Dwarf_value.emit (Dwarf_value.as_uleb128 (encode t))
end

module Dwarf_form : sig
  type t

  val addr : t
  val data1 : t
  val data4 : t
  val string : t
  val flag : t
  val block : t
  val ref_addr : t

  val emit : t -> unit
end = struct
  type t = [
  | `DW_FORM_addr
  | `DW_FORM_string
  | `DW_FORM_data1
  | `DW_FORM_data4
  | `DW_FORM_flag
  | `DW_FORM_block
  | `DW_FORM_ref_addr
  ]

  let encode = function
    | `DW_FORM_addr -> 0x01
    | `DW_FORM_data1 -> 0x0b
    | `DW_FORM_data4 -> 0x06
    | `DW_FORM_string -> 0x08
    | `DW_FORM_flag -> 0x0c
    | `DW_FORM_block -> 0x09
    | `DW_FORM_ref_addr -> 0x10

  let addr = `DW_FORM_addr
  let data1 = `DW_FORM_data1
  let data4 = `DW_FORM_data4
  let string = `DW_FORM_string
  let flag = `DW_FORM_flag
  let block = `DW_FORM_block
  let ref_addr = `DW_FORM_ref_addr

  let emit t =
    Dwarf_value.emit (Dwarf_value.as_uleb128 (encode t))
end

module Dwarf_operator : sig
  type t

  val register : reg_number:int -> offset:int -> t

  val size : t -> int
  val emit : t -> unit
end = struct
  type t = [
  | `DW_op_regx of Dwarf_value.t
  ]

  let register ~reg_number ~offset:_ =
    let reg_number = Dwarf_value.as_uleb128 reg_number in
    `DW_op_regx reg_number

  let opcode = function
    | `DW_op_regx _ -> 0x90

  let size t =
    let opcode_size = 1 in
    let args_size =
      match t with
      | `DW_op_regx reg_number -> Dwarf_value.size reg_number
    in
    opcode_size + args_size

  let emit t =
    Dwarf_value.emit (Dwarf_value.as_byte (opcode t));
    match t with
    | `DW_op_regx reg_number -> Dwarf_value.emit reg_number
end

(*
module Dwarf_block = struct
  type 'a t = 'a

  let emit t ~contents_size ~contents_emit =
    let contents_size = contents_size t in
    Dwarf_value.emit (Dwarf_value.as_uleb128 contents_size);
    contents_emit t
end
*)

module Dwarf_encoding_attribute : sig
  type t

  val signed : t

  val size : t -> int
  val as_dwarf_value : t -> Dwarf_value.t
end = struct
  type t = [
  | `DW_ATE_signed
  ]

  let signed = `DW_ATE_signed

  let encode = function
    | `DW_ATE_signed -> 0x05

  let size _t = 1

  let as_dwarf_value t =
    Dwarf_value.as_byte (encode t)
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
  val location : t
  val typ' : t
  val encoding : t
  val byte_size : t

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
  | `DW_AT_location
  | `DW_AT_type
  | `DW_AT_encoding
  | `DW_AT_byte_size
  ]

  let encode = function
    | `DW_AT_low_pc -> 0x11
    | `DW_AT_high_pc -> 0x12
    | `DW_AT_name -> 0x03
    | `DW_AT_comp_dir -> 0x1b
    | `DW_AT_producer -> 0x25
    | `DW_AT_stmt_list -> 0x10
    | `DW_AT_external -> 0x3f
    | `DW_AT_location -> 0x02
    | `DW_AT_type -> 0x49
    | `DW_AT_encoding -> 0x3e
    | `DW_AT_byte_size -> 0x0b

  let form = function
    | `DW_AT_low_pc -> Dwarf_form.addr
    | `DW_AT_high_pc -> Dwarf_form.addr
    | `DW_AT_name -> Dwarf_form.string
    | `DW_AT_comp_dir -> Dwarf_form.string
    | `DW_AT_producer -> Dwarf_form.string
    | `DW_AT_stmt_list -> Dwarf_form.data4
    | `DW_AT_external -> Dwarf_form.flag
    | `DW_AT_location -> Dwarf_form.data4
    | `DW_AT_type -> Dwarf_form.ref_addr
    | `DW_AT_encoding -> Dwarf_form.data1
    | `DW_AT_byte_size -> Dwarf_form.data1

  let low_pc = `DW_AT_low_pc
  let high_pc = `DW_AT_high_pc
  let producer = `DW_AT_producer
  let name = `DW_AT_name
  let comp_dir = `DW_AT_comp_dir
  let stmt_list = `DW_AT_stmt_list
  let extern'l = `DW_AT_external
  let location = `DW_AT_location
  let typ' = `DW_AT_type
  let encoding = `DW_AT_encoding
  let byte_size = `DW_AT_byte_size

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

  let create_location ~offset_from_start_of_debug_loc =
    Dwarf_attribute.location,
      Dwarf_value.as_four_byte_int offset_from_start_of_debug_loc

  let create_type ~label_name =
    Dwarf_attribute.typ',
      Dwarf_value.as_code_address_from_label ("Ldie__" ^ label_name)

  let create_encoding ~encoding =
    Dwarf_attribute.encoding, Dwarf_encoding_attribute.as_dwarf_value encoding

  let create_byte_size ~byte_size =
    assert (byte_size >= 1 && byte_size <= 0xff);
    Dwarf_attribute.byte_size, Dwarf_value.as_byte byte_size

  let emit (_attr, value) =
    Dwarf_value.emit value

  let size (_attr, value) =
    Dwarf_value.size value

  let attribute (attr, _value) = attr
end

module Dwarf_simple_location_expression = struct
  (* Register name operators only for the moment.  There are other
     cases (p23-24) *)
  type t = Dwarf_operator.t

  let in_register ~reg_number =
    Dwarf_operator.register ~reg_number ~offset:0

  let size t =
    Dwarf_operator.size t

  let emit t =
    Dwarf_operator.emit t
end

module Dwarf_location_expression = struct
  type t = Dwarf_simple_location_expression.t  (* will do for the moment *)

  let in_register ~reg_number =
    Dwarf_simple_location_expression.in_register ~reg_number

  let size t =
    Dwarf_simple_location_expression.size t

  let emit t =
    Dwarf_simple_location_expression.emit t
end

module Dwarf_location_list_entry = struct
  type t = {
    start_of_code_label : string;
    beginning_address_label : string;
    ending_address_label : string;
    expr : Dwarf_location_expression.t;
  }

  let create ~start_of_code_label
             ~first_address_when_in_scope
             ~first_address_when_not_in_scope
             ~location_expression =
    { start_of_code_label;
      beginning_address_label = first_address_when_in_scope;
      ending_address_label = first_address_when_not_in_scope;
      expr = location_expression;
    }

  let expr_size t = Dwarf_location_expression.size t.expr

  let size t = 8 + 8 + 2 + (expr_size t)

  let emit t =
    Dwarf_value.emit
      (Dwarf_value.as_code_address_from_label_diff
        t.beginning_address_label t.start_of_code_label);
    Dwarf_value.emit
      (Dwarf_value.as_code_address_from_label_diff
        t.ending_address_label t.start_of_code_label);
    Dwarf_value.emit (Dwarf_value.as_two_byte_int (expr_size t));
    Dwarf_location_expression.emit t.expr
end

module Dwarf_location_list : sig
  type t

  val create : Dwarf_location_list_entry.t list -> t
  val size : t -> int
  val emit : t -> unit
end = struct
  type t = Dwarf_location_list_entry.t list

  let create entries = entries

  let size t =
    let body_size =
      List.fold t
        ~init:0
        ~f:(fun size entry -> size + Dwarf_location_list_entry.size entry)
    in
    body_size + 8 + 8

  let emit t =
    List.iter t ~f:Dwarf_location_list_entry.emit;
    Dwarf_value.emit (Dwarf_value.as_code_address 0);
    Dwarf_value.emit (Dwarf_value.as_code_address 0)
end

module Dwarf_debug_loc_table : sig
  type t

  val create : unit -> t

  val insert : t
    -> location_list:Dwarf_location_list.t
    -> t * Dwarf_attribute_value.t

  val emit : t -> unit
end = struct
  type t = Dwarf_location_list.t list

  let create () = []

  let insert t ~location_list =
    let size_so_far =
      List.fold t
        ~init:0
        ~f:(fun size loc_list -> size + Dwarf_location_list.size loc_list)
    in
    let attribute_referencing_the_new_list =
      Dwarf_attribute_value.create_location
        ~offset_from_start_of_debug_loc:size_so_far
    in
    (location_list::t), attribute_referencing_the_new_list

  let emit t =
    List.iter (List.rev t) ~f:Dwarf_location_list.emit
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
    let address_width_in_bytes_on_target = Dwarf_value.as_byte 8 in
    let values = [
      Dwarf_value.as_two_byte_int 2;  (* section version number *)
      Dwarf_value.as_four_byte_int 0;
      address_width_in_bytes_on_target;
      Dwarf_value.as_byte 0;
      Dwarf_value.as_two_byte_int 0;
      Dwarf_value.as_two_byte_int 0;
      Dwarf_value.as_code_address_from_label start_of_code_label;
      Dwarf_value.as_code_address_from_label_diff
        end_of_code_label start_of_code_label;
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
    emit_string "Ldie__";
    emit_symbol t.label_name;
    emit_string ":\n";
    Dwarf_abbreviation_code.emit t.abbreviation_code;
    List.iter t.attribute_values ~f:Dwarf_attribute_value.emit

  let size t =
    List.fold_left t.attribute_values
      ~init:(Dwarf_abbreviation_code.size t.abbreviation_code)
      ~f:(fun size attribute_value ->
            size + Dwarf_attribute_value.size attribute_value)

  let null =
    let counter = ref 0 in
    fun () ->
    let count = !counter in
    counter := count + 1;
    { label_name = sprintf "null%d" count;
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
         (int * string * Dwarf_tag.t * (Dwarf_attribute_value.t list)) list
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
    (* CR mshinwell: the depth thing is nasty -- use a proper tree? *)
    let _depth, dies =
      List.fold_left tags_with_attribute_values
        ~init:(0, [])
        ~f:(fun (current_depth, dies) 
                (depth, label_name, tag, attribute_values) ->
              let need_null_entry = depth < current_depth in
              let dies =
                if need_null_entry then
                  (Dwarf_debugging_information_entry.null ())::dies
                else
                  dies
              in
              let abbreviation_code =
                Dwarf_abbreviation_code.of_int !next_abbreviation_code
              in
              next_abbreviation_code := !next_abbreviation_code + 1;
              let die =
                Dwarf_debugging_information_entry.create ~label_name
                  ~abbreviation_code
                  ~tag
                  ~attribute_values
              in
              depth, (die::dies))
    in
    { dies = (List.rev dies) @ [Dwarf_debugging_information_entry.null ()];
    }

  let dwarf_version = Dwarf_version.two
  let debug_abbrev_offset =
    Dwarf_value.as_four_byte_int_from_label "Ldebug_abbrev0"
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
  (* http://llvm.org/docs/SourceLevelDebugging.html claims this table
     is a waste of space.  Maybe we don't need to emit it. *)

  let emit ~externally_visible_functions ~debug_info =
    let size_of_entry function_name =
      4 + (String.length function_name) + 1
    in
    let size_of_all_entries =
      List.fold externally_visible_functions
        ~init:0
        ~f:(fun size entry -> size + size_of_entry entry)
    in
    let total_size = 2 + 4 + 4 + size_of_all_entries + 4 in
    let write_offset_name_pair function_name =
      (* CR mshinwell: should use [Dwarf_value.emit], no? *)
      emit_string "\t.long\tLdie__";
      emit_symbol function_name;
      emit_string "-Ldie__compile_unit\n";
      Dwarf_value.emit (Dwarf_value.as_string function_name)
    in
    Dwarf_value.emit (Dwarf_value.as_four_byte_int total_size);
    Dwarf_value.emit (Dwarf_value.as_two_byte_int 2);  (* version number *)
    Dwarf_value.emit (Dwarf_value.as_four_byte_int_from_label "Ldebug_info0");
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
  emit_symbol label_name;
  emit_string ":\n"

module Compilation_unit_state : sig
  type t

  val create : source_file_path:string option
    -> start_of_code_label:string
    -> end_of_code_label:string
    -> t

  val start_function : t
    -> function_name:string
    -> arguments_and_locations:(Ident.t * Reg.location) list
    -> unit

  val end_function : t
    -> function_name:string
    -> unit

  val emit_debugging_info_prologue : t -> unit
  val emit_debugging_info_epilogue : t -> unit
end = struct
  type t = {
    source_file_path : string option;
    start_of_code_label : string;
    end_of_code_label : string;
    mutable externally_visible_functions : string list;
    mutable function_tags :
      (int * string * Dwarf_tag.t * Dwarf_attribute_value.t list) list;
    mutable debug_loc_table : Dwarf_debug_loc_table.t;
  }

  let create ~source_file_path ~start_of_code_label ~end_of_code_label =
    { source_file_path;
      start_of_code_label;
      end_of_code_label;
      externally_visible_functions = [];
      function_tags = [];
      debug_loc_table = Dwarf_debug_loc_table.create ();
    }

  let builtin_ocaml_type_label_value = "type_value"

  let build_ocaml_type_tags () = [
    1, builtin_ocaml_type_label_value, Dwarf_tag.base_type, [
      Dwarf_attribute_value.create_name ~source_file_path:"value";
      Dwarf_attribute_value.create_encoding
        ~encoding:Dwarf_encoding_attribute.signed;
      Dwarf_attribute_value.create_byte_size
        ~byte_size:8;
    ];
  ]

  let start_function t ~function_name ~arguments_and_locations =
    let starting_label = sprintf "Llr_begin_%s" function_name in
    let ending_label = sprintf "Llr_end_%s" function_name in
    emit_label_declaration starting_label;
    let debug_loc_table, argument_tags =
      List.fold arguments_and_locations
        ~init:(t.debug_loc_table, [])
        ~f:(fun (debug_loc_table, tags) (ident, pseudoreg_location) ->
              let location_expression =
                match pseudoreg_location with
                | Reg.Unknown -> None
                (* CR mshinwell: fix the stack case *)
                | Reg.Stack _ -> None
                | Reg.Reg reg_number ->
                  Some (Dwarf_location_expression.in_register reg_number)
              in
              match location_expression with
              | None -> debug_loc_table, tags
              | Some location_expression ->
                let location_list_entry =
                  Dwarf_location_list_entry.create
                    ~start_of_code_label:t.start_of_code_label
                    ~first_address_when_in_scope:starting_label
                    ~first_address_when_not_in_scope:ending_label  (* fixme *)
                    ~location_expression
                in
                let location_list =
                  Dwarf_location_list.create [location_list_entry]
                in
                let debug_loc_table, loclistptr_attribute_value =
                  Dwarf_debug_loc_table.insert debug_loc_table
                    ~location_list
                in
                let arg_name = Ident.unique_name ident in
                let tag =
                  2, function_name ^ "__arg__" ^ arg_name,
                    Dwarf_tag.variable,
                    [Dwarf_attribute_value.create_name
                       ~source_file_path:arg_name;
                     loclistptr_attribute_value;
                     Dwarf_attribute_value.create_type
                       ~label_name:builtin_ocaml_type_label_value;
                    ]
                in
                debug_loc_table, tag::tags)
    in
    let subprogram_tag =
      let tag =
        if List.length argument_tags > 0 then
          Dwarf_tag.subprogram
        else
          Dwarf_tag.subprogram_with_no_children
      in
      1, function_name, tag, [
        Dwarf_attribute_value.create_name ~source_file_path:function_name;
        Dwarf_attribute_value.create_external ~is_visible_externally:true;
        Dwarf_attribute_value.create_low_pc ~address_label:starting_label;
        Dwarf_attribute_value.create_high_pc ~address_label:ending_label;
      ]
    in
    let this_function's_tags =
      subprogram_tag::(List.rev argument_tags)
    in
    t.externally_visible_functions <-
      function_name::t.externally_visible_functions;
    t.debug_loc_table <- debug_loc_table;
    t.function_tags <- t.function_tags @ this_function's_tags

  let end_function _t ~function_name =
    emit_label_declaration (sprintf "Llr_end_%s" function_name)

  let emit_debugging_info_prologue _t =
    emit_section_declaration ~section_name:".debug_abbrev";
    emit_label_declaration ~label_name:"Ldebug_abbrev0";
    emit_section_declaration ~section_name:".debug_line";
    emit_label_declaration ~label_name:"Ldebug_line0";
    emit_section_declaration ~section_name:".debug_loc";
    emit_label_declaration ~label_name:"Ldebug_loc0"

  let emit_debugging_info_epilogue t =
    let producer_name = sprintf "ocamlopt %s" Sys.ocaml_version in
    let compile_unit_attribute_values =
      let common = [
        Dwarf_attribute_value.create_producer ~producer_name;
        Dwarf_attribute_value.create_low_pc
          ~address_label:t.start_of_code_label;
        Dwarf_attribute_value.create_high_pc
          ~address_label:t.end_of_code_label;
        Dwarf_attribute_value.create_stmt_list
          ~section_offset_label:"Ldebug_line0";
        Dwarf_attribute_value.create_comp_dir ~directory:(Sys.getcwd ());
      ]
      in
      match t.source_file_path with
      | None -> common
      | Some source_file_path ->
        (Dwarf_attribute_value.create_name ~source_file_path)::common
    in
    let tags_with_attribute_values = [
      0, "compile_unit", Dwarf_tag.compile_unit, compile_unit_attribute_values;
    ] @ (build_ocaml_type_tags ()) @ t.function_tags
    in
    let debug_info =
      Dwarf_debug_info_section.create ~tags_with_attribute_values
    in
    let debug_abbrev =
      Dwarf_debug_info_section.to_abbreviations_table debug_info
    in
    emit_section_declaration ~section_name:".debug_info";
    emit_label_declaration "Ldebug_info0";
    Dwarf_debug_info_section.emit debug_info;
    emit_switch_to_section ~section_name:".debug_abbrev";
    Dwarf_abbreviations_table.emit debug_abbrev;
    emit_section_declaration ~section_name:".debug_pubnames";
    Dwarf_pubnames_table.emit
      ~externally_visible_functions:t.externally_visible_functions
      ~debug_info;
    emit_section_declaration ~section_name:".debug_aranges";
    Dwarf_aranges_table.emit ~start_of_code_label:t.start_of_code_label
      ~end_of_code_label:t.end_of_code_label;
    emit_switch_to_section ~section_name:".debug_loc";
    Dwarf_debug_loc_table.emit t.debug_loc_table
end
