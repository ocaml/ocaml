(* Emission of DWARF 3 debugging information.
   http://dwarfstd.org/doc/Dwarf3.pdf
*)

module Mini_core = struct
  module List = struct
    include ListLabels
    let fold = fold_left
  end

  let sprintf = Printf.sprintf
end

include Mini_core  (* a glimpse of another world *)

open Emitaux

(* CR mshinwell: fixme: copied from emit.mlp *)
let macosx = (Config.system = "macosx")
let emit_symbol s =
    if macosx then emit_string "_";
    Emitaux.emit_symbol '$' s

let emit_label_declaration ~label_name =
  emit_symbol label_name;
  emit_string ":\n"

let size_of_dwarf_attribute_value_list attr_vals =
  List.fold_left attr_vals
    ~init:0
    ~f:(fun size attr_val -> size + Dwarf.Attribute_value.size attr_val)

let emit_section_declaration ~section_name =
  emit_string (sprintf "\t.section\t%s,\"\",@progbits\n" section_name)

let emit_switch_to_section ~section_name =
  emit_string (sprintf "\t.section\t%s\n" section_name)

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
      (int * string * Dwarf.Tag.t * Dwarf.Attribute_value.t list) list;
    mutable debug_loc_table : Dwarf.Debug_loc_table.t;
  }

  let create ~source_file_path ~start_of_code_label ~end_of_code_label =
    { source_file_path;
      start_of_code_label;
      end_of_code_label;
      externally_visible_functions = [];
      function_tags = [];
      debug_loc_table = Dwarf.Debug_loc_table.create ();
    }

  let builtin_ocaml_type_label_value = "type_value"

  let build_ocaml_type_tags () = [
    1, builtin_ocaml_type_label_value, Dwarf.Tag.base_type, [
      Dwarf.Attribute_value.create_name ~source_file_path:"value";
      Dwarf.Attribute_value.create_encoding
        ~encoding:Dwarf.Encoding_attribute.signed;
      Dwarf.Attribute_value.create_byte_size
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
                  Some (Dwarf.Location_expression.in_register reg_number)
              in
              match location_expression with
              | None -> debug_loc_table, tags
              | Some location_expression ->
                let base_address_selection_entry =
                  Dwarf.Location_list_entry.create_base_address_selection_entry
                    ~base_address_label:starting_label
                in
                let location_list_entry =
                  Dwarf.Location_list_entry.create_location_list_entry
                    ~start_of_code_label:starting_label
                    ~first_address_when_in_scope:starting_label
                    ~first_address_when_not_in_scope:ending_label  (* fixme *)
                    ~location_expression
                in
                let location_list =
                  Dwarf.Location_list.create
                    [base_address_selection_entry; location_list_entry]
                in
                let debug_loc_table, loclistptr_attribute_value =
                  Dwarf.Debug_loc_table.insert debug_loc_table
                    ~location_list
                in
                let arg_name = Ident.name ident in
                let tag =
                  2, function_name ^ "__arg__" ^ (Ident.unique_name ident),
                    Dwarf.Tag.formal_parameter,
                    [Dwarf.Attribute_value.create_name
                       ~source_file_path:arg_name;
                     loclistptr_attribute_value;
                     Dwarf.Attribute_value.create_type
                       ~label_name:builtin_ocaml_type_label_value;
                    ]
                in
                debug_loc_table, tag::tags)
    in
    let subprogram_tag =
      let tag =
        if List.length argument_tags > 0 then
          Dwarf.Tag.subprogram
        else
          Dwarf.Tag.subprogram_with_no_children
      in
      1, function_name, tag, [
        Dwarf.Attribute_value.create_name ~source_file_path:function_name;
        Dwarf.Attribute_value.create_external ~is_visible_externally:true;
        Dwarf.Attribute_value.create_low_pc ~address_label:starting_label;
        Dwarf.Attribute_value.create_high_pc ~address_label:ending_label;
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
        Dwarf.Attribute_value.create_producer ~producer_name;
        Dwarf.Attribute_value.create_low_pc
          ~address_label:t.start_of_code_label;
        Dwarf.Attribute_value.create_high_pc
          ~address_label:t.end_of_code_label;
        Dwarf.Attribute_value.create_stmt_list
          ~section_offset_label:"Ldebug_line0";
        Dwarf.Attribute_value.create_comp_dir ~directory:(Sys.getcwd ());
      ]
      in
      match t.source_file_path with
      | None -> common
      | Some source_file_path ->
        (Dwarf.Attribute_value.create_name ~source_file_path)::common
    in
    let tags_with_attribute_values = [
      0, "compile_unit", Dwarf.Tag.compile_unit, compile_unit_attribute_values;
    ] @ (build_ocaml_type_tags ()) @ t.function_tags
    in
    let debug_info =
      Dwarf.Debug_info_section.create ~tags_with_attribute_values
    in
    let debug_abbrev =
      Dwarf.Debug_info_section.to_abbreviations_table debug_info
    in
    emit_section_declaration ~section_name:".debug_info";
    emit_label_declaration "Ldebug_info0";
    Dwarf.Debug_info_section.emit debug_info;
    emit_switch_to_section ~section_name:".debug_abbrev";
    Dwarf.Abbreviations_table.emit debug_abbrev;
    emit_section_declaration ~section_name:".debug_pubnames";
    Dwarf.Pubnames_table.emit
      ~externally_visible_functions:t.externally_visible_functions
      ~debug_info;
    emit_section_declaration ~section_name:".debug_aranges";
    Dwarf.Aranges_table.emit ~start_of_code_label:t.start_of_code_label
      ~end_of_code_label:t.end_of_code_label;
    emit_switch_to_section ~section_name:".debug_loc";
    Dwarf.Debug_loc_table.emit t.debug_loc_table
end
