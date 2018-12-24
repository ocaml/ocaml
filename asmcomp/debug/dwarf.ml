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

module ARV = Available_ranges_all_vars
module DAH = Dwarf_attribute_helpers
module L = Linearize
module LB = Lexical_block_ranges
module SLDL = Simple_location_description_lang
module V = Backend_var

type t = {
  compilation_unit_header_label : Asm_label.t;
  compilation_unit_proto_die : Proto_die.t;
  value_type_proto_die : Proto_die.t;
  address_table : Address_table.t;
  debug_loc_table : Debug_loc_table.t;
  debug_ranges_table : Debug_ranges_table.t;
  location_list_table : Location_list_table.t;
  range_list_table : Range_list_table.t;
  start_of_code_symbol : Asm_symbol.t;
  end_of_code_symbol : Asm_symbol.t;
  mutable rvalue_dies_required_for : V.Set.t;
  function_abstract_instances :
    (Proto_die.t * Asm_symbol.t) Debuginfo.Function.Id.Tbl.t;
  die_symbols_for_external_declarations : Asm_symbol.t Asm_symbol.Tbl.t;
  mutable emitted : bool;
}

type is_variable_phantom = Non_phantom | Phantom

type proto_dies_for_var = {
  is_variable_phantom : is_variable_phantom;
  value_die_lvalue : Proto_die.reference;
  value_die_rvalue : Proto_die.reference;
  type_die : Proto_die.reference;
}

(* CR mshinwell: On OS X 10.11 (El Capitan), dwarfdump doesn't seem to be able
   to read our 64-bit DWARF output. *)

let dwarf_version = ref Dwarf_version.five

let supports_call_sites () = true

let mangle_symbol symbol =
  let unit_name =
    Linkage_name.to_string (Compilation_unit.get_linkage_name (
      Symbol.compilation_unit symbol))
  in
  let symbol' =
    Compilenv.concat_symbol unit_name
      (Linkage_name.to_string (Symbol.label symbol))
  in
  Asm_symbol.of_external_name (Symbol.compilation_unit symbol) symbol'

let create ~sourcefile ~prefix_name ~unit_name =
  begin match !Clflags.dwarf_format with
  | Thirty_two -> Dwarf_format.set Thirty_two
  | Sixty_four -> Dwarf_format.set Sixty_four
  end;
  begin match !Clflags.dwarf_version with
  | Four -> dwarf_version := Dwarf_version.four
  | Five -> dwarf_version := Dwarf_version.five
  end;
  let cwd = Sys.getcwd () in
  let source_directory_path, source_filename =
    if Filename.is_relative sourcefile then cwd, sourcefile
    else Filename.dirname sourcefile, Filename.basename sourcefile
  in
  let object_directory_path =
    if Filename.is_relative prefix_name then source_directory_path
    else Filename.dirname prefix_name
  in
  let object_filename =
    (Filename.basename prefix_name) ^ Config.ext_obj
  in
  let start_of_code_symbol =
    mangle_symbol (
      Symbol.of_global_linkage (Compilation_unit.get_current_exn ())
        (Linkage_name.create "code_begin"))
  in
  let end_of_code_symbol =
    mangle_symbol (
      Symbol.of_global_linkage (Compilation_unit.get_current_exn ())
        (Linkage_name.create "code_end"))
  in
  let debug_line_label = Asm_section.label (DWARF Debug_line) in
  let address_table = Address_table.create () in
  let addr_base = Address_table.base_addr address_table in
  let debug_loc_table = Debug_loc_table.create () in
  let debug_ranges_table = Debug_ranges_table.create () in
  let location_list_table = Location_list_table.create () in
  let loclists_base = Location_list_table.base_addr location_list_table in
  let range_list_table = Range_list_table.create () in
  let rnglists_base = Range_list_table.base_addr range_list_table in
  let compilation_unit_proto_die =
    let dwarf_5_only =
      match !Clflags.dwarf_version with
      | Four -> []
      | Five -> [
        DAH.create_addr_base addr_base;
        DAH.create_loclists_base loclists_base;
        DAH.create_rnglists_base rnglists_base;
      ]
    in
    let attribute_values =
      [ DAH.create_name (Ident.name unit_name);
        (* The [OCaml] attribute value here is only defined in DWARF-5, but
           it doesn't mean anything else in DWARF-4, so we always emit it.
           This saves special-case logic in gdb based on the producer name. *)
        DAH.create_language OCaml;
        DAH.create_producer "ocamlopt";
        DAH.create_ocaml_compiler_version Sys.ocaml_version;
        DAH.create_ocaml_cmi_magic_number Config.cmi_magic_number;
        DAH.create_ocaml_cmt_magic_number Config.cmt_magic_number;
        DAH.create_ocaml_load_path !Config.load_path;
        DAH.create_comp_dir cwd;
        DAH.create_ocaml_source_directory_path source_directory_path;
        DAH.create_ocaml_source_filename source_filename;
        DAH.create_ocaml_object_directory_path object_directory_path;
        DAH.create_ocaml_object_filename object_filename;
        DAH.create_low_pc_from_symbol start_of_code_symbol;
        DAH.create_high_pc_from_symbol end_of_code_symbol;
        DAH.create_stmt_list ~debug_line_label;
      ] @ dwarf_5_only
    in
    Proto_die.create ~parent:None
      ~tag:Compile_unit
      ~attribute_values
      ()
  in
  let value_type_proto_die =
    Proto_die.create ~parent:(Some compilation_unit_proto_die)
      ~tag:Base_type
      ~attribute_values:[
        DAH.create_name "<value>";
        DAH.create_encoding ~encoding:Encoding_attribute.signed;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
      ]
      ()
  in
  { compilation_unit_proto_die;
    compilation_unit_header_label = Asm_label.create ();
    value_type_proto_die;
    debug_loc_table;
    debug_ranges_table;
    address_table;
    location_list_table;
    range_list_table;
    start_of_code_symbol;
    end_of_code_symbol;
    rvalue_dies_required_for = V.Set.empty;
    function_abstract_instances = Debuginfo.Function.Id.Tbl.create 42;
    die_symbols_for_external_declarations = Asm_symbol.Tbl.create 42;
    emitted = false;
  }

type var_uniqueness = {
  name_is_unique : bool;
  position_is_unique : bool;
}

let arch_size_addr = Targetint.of_int_exn Arch.size_addr

let calculate_var_uniqueness ~available_ranges_vars =
  let module String = Misc.Stdlib.String in
  let by_name = String.Tbl.create 42 in
  let by_position = Debuginfo.Code_range.Option.Tbl.create 42 in
  let update_uniqueness var pos =
    let name = Backend_var.name var in
    begin match String.Tbl.find by_name name with
    | exception Not_found ->
      String.Tbl.add by_name name (Backend_var.Set.singleton var)
    | vars ->
      String.Tbl.replace by_name name (Backend_var.Set.add var vars)
    end;
    begin match Debuginfo.Code_range.Option.Tbl.find by_position pos with
    | exception Not_found ->
      Debuginfo.Code_range.Option.Tbl.add by_position pos
        (Backend_var.Set.singleton var)
    | vars ->
      Debuginfo.Code_range.Option.Tbl.replace by_position pos
        (Backend_var.Set.add var vars)
    end
  in
  let result = Backend_var.Tbl.create 42 in
  ARV.iter available_ranges_vars
    ~f:(fun var range ->
      let range_info = ARV.Range.info range in
      let dbg = ARV.Range_info.debuginfo range_info in
      let pos = Debuginfo.position dbg in
      update_uniqueness var pos;
      Backend_var.Tbl.replace result var
        { name_is_unique = false;
          position_is_unique = false;
        });
  String.Tbl.iter (fun _name vars ->
      match Backend_var.Set.get_singleton vars with
      | None -> ()
      | Some var ->
        let var_uniqueness = Backend_var.Tbl.find result var in
        Backend_var.Tbl.replace result var
          { var_uniqueness with
            name_is_unique = true;
          })
    by_name;
  Debuginfo.Code_range.Option.Tbl.iter (fun _pos vars ->
      match Backend_var.Set.get_singleton vars with
      | None -> ()
      | Some var ->
        let var_uniqueness = Backend_var.Tbl.find result var in
        Backend_var.Tbl.replace result var
          { var_uniqueness with
            position_is_unique = true;
          })
    by_position;
  result

let proto_dies_for_variable var ~proto_dies_for_vars =
  match Backend_var.Tbl.find proto_dies_for_vars var with
  | exception Not_found -> None
  | result -> Some result

let normal_type_for_var ?reference _t ~parent ident_for_type =
  let name_attribute =
    match ident_for_type with
    | None -> []
    | Some (compilation_unit, var) ->
      let name =
        Name_laundry.base_type_die_name_for_var compilation_unit var
      in
      [DAH.create_name name]
  in
  Proto_die.create ?reference
    ~parent
    ~tag:Base_type
    ~attribute_values:(name_attribute @ [
      DAH.create_encoding ~encoding:Encoding_attribute.signed;
      DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
    ])
    ()

let type_die_reference_for_var var ~proto_dies_for_vars =
  match proto_dies_for_variable var ~proto_dies_for_vars with
  | None -> None
  | Some dies -> Some dies.type_die

(* Build a new DWARF type for [var].  Each variable has its
   own type, which is basically its stamped name, and is nothing to do with
   its inferred OCaml type.  The inferred type may be recovered by the
   debugger by extracting the stamped name and then using that as a key
   for lookup into the .cmt file for the appropriate module.

   We emit the parameter index into the type if the variable in question
   is a function parameter.  This is used in the debugger support library.
   It would be nice not to have to have this hack, but it avoids changes
   in the main gdb code to pass parameter indexes to the printing function.
   It is arguably more robust, too.
*)
(* CR mshinwell: we're not emitting param indexes *)
(* CR mshinwell: Add proper type for [ident_for_type] *)
let construct_type_of_value_description t ~parent ident_for_type
      ~(phantom_defining_expr : ARV.Range_info.phantom_defining_expr)
      ~proto_dies_for_vars ~reference =
  let normal_case () =
    let (_ : Proto_die.t) =
      normal_type_for_var ~reference t ~parent ident_for_type
    in
    ()
  in
  let name_attribute =
    match ident_for_type with
    | None -> []
    | Some (compilation_unit, var) ->
      let name = Name_laundry.base_type_die_name_for_var compilation_unit var in
      [DAH.create_name name]
  in
  match phantom_defining_expr with
  | Non_phantom -> normal_case ()
  | Phantom defining_expr ->
    match defining_expr with
    | Iphantom_const_int _
    | Iphantom_const_symbol _
    | Iphantom_read_symbol_field _ -> normal_case ()
    | Iphantom_var var ->
      let type_attribute_value =
        match type_die_reference_for_var var ~proto_dies_for_vars with
        | None ->
          (* This (and other similar cases below) guard against the case where
             a variable referenced in the defining expression of a phantom
             variable doesn't have any available range. *)
          DAH.create_type ~proto_die:t.value_type_proto_die
        | Some target_type ->
          DAH.create_type_from_reference ~proto_die_reference:target_type
      in
      let _proto_die : Proto_die.t =
        Proto_die.create ~reference
          ~parent
          ~tag:Typedef
          ~attribute_values:(name_attribute @ [
            type_attribute_value;
          ])
          ()
      in
      ()
    | Iphantom_read_field { var = _; field = _; } ->
      (* We cannot dereference an implicit pointer when evaluating a DWARF
         location expression, which means we must restrict ourselves to 
         projections from non-phantom identifiers. *)
      normal_case ()
    | Iphantom_offset_var { var = _; offset_in_words = _; } ->
      (* The same applies here as for [Iphantom_read_var_field] above, but we
         never generate this for phantom identifiers at present (it is only
         used for offsetting a closure environment variable). *)
      normal_case ()
    | Iphantom_block { tag = _; fields; } ->
      (* The block will be described as a composite location expression
         pointed at by an implicit pointer.  The corresponding type is
         a pointer type whose target type is a structure type; the members of
         the structure correspond to the fields of the block (plus the
         header).  We don't use DWARF array types as we need to describe a
         different type for each field. *)
      let struct_type_die =
        Proto_die.create ~parent
          ~tag:Structure_type
          ~attribute_values:[
            DAH.create_name "<block>";
            DAH.create_byte_size_exn
              ~byte_size:((1 + List.length fields) * Arch.size_addr);
          ]
          ()
      in
      List.iteri (fun index field ->
          let name = string_of_int (index - 1) in
          let type_attribute =
            match field with
            | None -> DAH.create_type ~proto_die:t.value_type_proto_die
            | Some var ->
              (* It's ok if [var] is a phantom identifier, since we will
                 be building a composite location expression to describe the
                 structure, and implicit pointers are permitted there.  (That
                 is to say, the problem for [Iphantom_read_var_field] above
                 does not exist here). *)
              match type_die_reference_for_var var ~proto_dies_for_vars with
              | None -> DAH.create_type ~proto_die:t.value_type_proto_die
              | Some proto_die_reference ->
                DAH.create_type_from_reference ~proto_die_reference
          in
          Proto_die.create_ignore ~sort_priority:index
            ~parent:(Some struct_type_die)
            ~tag:Member
            ~attribute_values:(type_attribute :: name_attribute @ [
              DAH.create_name name;
              DAH.create_bit_size (Int64.of_int (Arch.size_addr * 8));
              DAH.create_data_member_location
                ~byte_offset:(Int64.of_int (index * Arch.size_addr));
            ])
            ())
        (None :: fields);  (* "None" is for the GC header. *)
      let _pointer_to_struct_type_die : Proto_die.t =
        Proto_die.create ~reference ~parent
          ~tag:Pointer_type
          ~attribute_values:(name_attribute @ [
            DAH.create_type ~proto_die:struct_type_die;
          ])
          ()
      in
      ()

let is_variable_phantom var ~proto_dies_for_vars =
  match proto_dies_for_variable var ~proto_dies_for_vars with
  | None -> None
  | Some { is_variable_phantom; _ } -> Some is_variable_phantom

let die_location_of_variable_lvalue t var ~proto_dies_for_vars =
  (* We may need to reference the locations of other values in order to
     describe the location of some particular value.  This is done by using
     the "call" functionality of DWARF location descriptions.
     (DWARF-4 specification section 2.5.1.5, page 24.)  This avoids any need
     to transitively resolve phantom lets (to constants, symbols or
     non-phantom variables) in the compiler. *)
  match proto_dies_for_variable var ~proto_dies_for_vars with
  | None -> None
  | Some { value_die_lvalue; _; } ->
    let location =
      SLDL.Lvalue.location_from_another_die
        ~die_label:value_die_lvalue
        ~compilation_unit_header_label:t.compilation_unit_header_label
    in
    Some location

let die_location_of_variable_rvalue t var ~proto_dies_for_vars =
  match proto_dies_for_variable var ~proto_dies_for_vars with
  | None -> None
  | Some { value_die_rvalue; _; } ->
    t.rvalue_dies_required_for <- V.Set.add var t.rvalue_dies_required_for;
    let location =
      SLDL.Rvalue.location_from_another_die
        ~die_label:value_die_rvalue
        ~compilation_unit_header_label:t.compilation_unit_header_label
    in
    Some location

type location_description =
  | Simple of Simple_location_description.t
  | Composite of Composite_location_description.t

let reg_location_description0 (reg : Reg.t) ~offset_from_cfa_in_bytes
      ~need_rvalue =
  let module SLD = Simple_location_description in
  match reg.loc with
  | Unknown ->
    Misc.fatal_errorf "Register without location: %a" Printmach.reg reg
  | Reg n ->
    let dwarf_reg_number =
      let reg_class = Proc.register_class reg in
      let first_available_reg = Proc.first_available_register.(reg_class) in
      let num_hard_regs = Proc.num_available_registers.(reg_class) in
      let n = n - first_available_reg in
      (* This [None] case isn't an error to cover situations such as found
         in the i386 backend where [num_available_registers] does not extend
         to the end of the register arrays (in that case for the x87 top of
         stack register). *)
      if n < 0 || n >= num_hard_regs then None
      else Some (Proc.dwarf_register_numbers ~reg_class).(n)
    in
    begin match dwarf_reg_number with
    | None -> None
    | Some dwarf_reg_number ->
      let location_description =
        if not need_rvalue then
          SLDL.compile (SLDL.of_lvalue (
            SLDL.Lvalue.in_register ~dwarf_reg_number))
        else
          SLDL.compile (SLDL.of_rvalue (
            SLDL.Rvalue.in_register ~dwarf_reg_number))
      in
      Some location_description
    end
  | Stack _ ->
    match offset_from_cfa_in_bytes with
    | None ->
      Misc.fatal_errorf "Register %a assigned to stack but no offset \
          from CFA provided"
        Printmach.reg reg
    | Some offset_from_cfa_in_bytes ->
      if offset_from_cfa_in_bytes mod Arch.size_addr <> 0 then begin
        Misc.fatal_errorf "Dwarf.location_list_entry: misaligned stack \
            slot at offset %d (reg %a)"
          offset_from_cfa_in_bytes
          Printmach.reg reg
      end;
      (* CR-soon mshinwell: use [offset_in_bytes] instead *)
      let offset_in_words =
        Targetint.of_int_exn (offset_from_cfa_in_bytes / Arch.size_addr)
      in
      if not need_rvalue then
        Some (SLDL.compile (SLDL.of_lvalue (
          SLDL.Lvalue.in_stack_slot ~offset_in_words)))
      else
        Some (SLDL.compile (SLDL.of_rvalue (
          SLDL.Rvalue.in_stack_slot ~offset_in_words)))

let reg_location_description reg ~offset_from_cfa_in_bytes
      ~need_rvalue : location_description option =
  match
    reg_location_description0 reg ~offset_from_cfa_in_bytes ~need_rvalue
  with
  | None -> None
  | Some simple_loc_desc -> Some (Simple simple_loc_desc)

let phantom_var_location_description t
      ~(defining_expr : Mach.phantom_defining_expr) ~need_rvalue:_
      ~proto_dies_for_vars ~parent
      : location_description option =
  let module SLD = Simple_location_description in
  let lvalue lvalue = Some (Simple (SLDL.compile (SLDL.of_lvalue lvalue))) in
  let rvalue rvalue = Some (Simple (SLDL.compile (SLDL.of_rvalue rvalue))) in
  match defining_expr with
  | Iphantom_const_int i -> rvalue (SLDL.Rvalue.signed_int_const i)
  | Iphantom_const_symbol symbol ->
    let symbol = Asm_symbol.create symbol in
    lvalue (SLDL.Lvalue.const_symbol ~symbol)
  | Iphantom_read_symbol_field { sym; field; } ->
    let symbol = Asm_symbol.create sym in
    (* CR-soon mshinwell: Fix [field] to be of type [Targetint.t] *)
    let field = Targetint.of_int field in
    rvalue (SLDL.Rvalue.read_symbol_field ~symbol ~field)
  | Iphantom_var var ->
    (* mshinwell: What happens if [var] isn't available at some point
       just due to the location list?  Should we push zero on the stack
       first?  Or can we detect the stack is empty?  Or does gdb just abort
       evaluation of the whole thing if the location list doesn't match?
       mshinwell: The answer seems to be that you get an error saying
       that you tried to pop something from an empty stack.  What we do
       now is to wrap the location description in a composite location
       description (with only one piece), which then causes gdb to
       correctly detect unavailability. *)
    (* CR-someday mshinwell: consider DWARF extension to avoid this?
       The problem is worse for the read-field/read-var cases below,
       since I think the (e.g.) dereferencing needs to be inside the piece
       delimiters, exposing us to an error.  At the moment this should just
       get caught by the exception handler in libmonda, but even still, a
       better solution would be desirable.  Follow-up: actually it doesn't
       get that far---the error is thrown even before the value printer
       is called. *)
    begin match die_location_of_variable_rvalue t var ~proto_dies_for_vars with
    | None -> None
    | Some rvalue ->
      let location = SLDL.compile (SLDL.of_rvalue rvalue) in
      let composite =
        Composite_location_description.
          pieces_of_simple_location_descriptions
            [location, arch_size_addr]
      in
      Some (Composite composite)
    end
  | Iphantom_read_field { var; field; } ->
    (* CR-soon mshinwell: Clarify the following (maybe in SLDL directly):
       "lvalue" == "no DW_op_stack_value" *)
    (* Reminder: see CR in flambda_utils.ml regarding these constructions *)
    (* We need to use a location expression that when evaluated will
       yield the runtime value of [var] *actually on the DWARF stack*.
       This is necessary so that we can apply other stack operations,
       such as arithmetic and dereferencing, to the value.  The upshot is
       that we cannot use the "standalone" location-describing operators
       such as DW_op_regx; instead we would use DW_op_bregx.  (Another
       way of looking at this is that we need to get the DWARF operator
       sequence without [DW_op_stack_value], either implicitly or
       explicitly, at the end.) *)
    begin match is_variable_phantom var ~proto_dies_for_vars with
    | None | Some Phantom ->
      (* For the moment, show this field access as unavailable, since we
         cannot "DW_OP_deref" a value built up with implicit pointers. *)
      None
    | Some Non_phantom ->
      match die_location_of_variable_rvalue t var ~proto_dies_for_vars with
      | None -> None
      | Some block ->
        let field = Targetint.of_int_exn field in
        let read_field =
          SLDL.compile (SLDL.of_rvalue (SLDL.Rvalue.read_field ~block ~field))
        in
        let composite =
          Composite_location_description.
            pieces_of_simple_location_descriptions
              [read_field, arch_size_addr]
        in
        Some (Composite composite)
    end
  | Iphantom_offset_var { var; offset_in_words; } ->
    begin match die_location_of_variable_lvalue t var ~proto_dies_for_vars with
    | None -> None
    | Some location ->
      let offset_in_words = Targetint.of_int_exn offset_in_words in
      let offset_var =
        SLDL.compile (SLDL.of_rvalue (
          SLDL.Rvalue.offset_pointer location ~offset_in_words))
      in
      let composite =
        Composite_location_description.
          pieces_of_simple_location_descriptions
            [offset_var, arch_size_addr]
      in
      Some (Composite composite)
    end
  | Iphantom_block { tag; fields; } ->
    (* A phantom block construction: instead of the block existing in the
       target program's address space, it is going to be conjured up in the
       *debugger's* address space using instructions described in DWARF.
       References between such blocks do not use normal pointers in the
       target's address space---instead they use "implicit pointers"
       (requires GNU DWARF extensions prior to DWARF-5). *)
    (* CR-someday mshinwell: use a cache to dedup the CLDs *)
    let header =
      SLDL.compile (SLDL.of_rvalue (SLDL.Rvalue.signed_int_const (
        Targetint.of_int64 (Int64.of_nativeint (
          Cmmgen.black_block_header tag (List.length fields))))))
    in
    let header_size = arch_size_addr in
    let field_size = arch_size_addr in
    let fields =
      List.map (fun var ->
          let simple_location_description =
            match var with
            | None ->
              (* This element of the block isn't accessible. *)
              []
            | Some ident ->
              match
                die_location_of_variable_rvalue t ident ~proto_dies_for_vars
              with
              | None -> []
              | Some rvalue -> SLDL.compile (SLDL.of_rvalue rvalue)
          in
          simple_location_description, field_size)
        fields
    in
    let composite_location_description =
      Composite_location_description.pieces_of_simple_location_descriptions
        ((header, header_size) :: fields)
    in
    let proto_die =
      Proto_die.create ~parent
        ~tag:Variable
        ~attribute_values:[
          DAH.create_composite_location_description
            composite_location_description;
        ]
        ()
    in
    rvalue (SLDL.Rvalue.implicit_pointer ~offset_in_bytes:Targetint.zero
      ~die_label:(Proto_die.reference proto_die)
      (!dwarf_version))

type location_list_entry =
  | Dwarf_4 of Dwarf_4_location_list_entry.t
  | Dwarf_5 of Location_list_entry.t

let location_list_entry t (fundecl : L.fundecl) ~parent ~subrange
      ~proto_dies_for_vars ~need_rvalue : location_list_entry option =
  let location_description =
    match ARV.Subrange.info subrange with
    | Non_phantom { reg; offset_from_cfa_in_bytes; } ->
      reg_location_description reg ~offset_from_cfa_in_bytes ~need_rvalue
    | Phantom defining_expr ->
      phantom_var_location_description t ~defining_expr
        ~need_rvalue ~proto_dies_for_vars ~parent
  in
  let single_location_description =
    match location_description with
    | None -> None
    | Some (Simple simple) ->
      Some (Single_location_description.of_simple_location_description simple)
    | Some (Composite composite) ->
      Some (Single_location_description.of_composite_location_description
        composite)
  in
  match single_location_description with
  | None -> None
  | Some single_location_description ->
    let start_pos = Asm_label.create_int (ARV.Subrange.start_pos subrange) in
    let end_pos = Asm_label.create_int (ARV.Subrange.end_pos subrange) in
    let end_pos_offset = ARV.Subrange.end_pos_offset subrange in
    match !Clflags.dwarf_version with
    | Four ->
      let location_list_entry =
        Dwarf_4_location_list_entry.create_location_list_entry
          ~start_of_code_symbol:(Asm_symbol.create fundecl.fun_name)
          ~first_address_when_in_scope:start_pos
          ~first_address_when_not_in_scope:end_pos
          ~first_address_when_not_in_scope_offset:(Some end_pos_offset)
          ~single_location_description
      in
      Some (Dwarf_4 location_list_entry)
    | Five ->
      let start_inclusive =
        Address_table.add t.address_table start_pos
          ~start_of_code_symbol:t.start_of_code_symbol
      in
      let end_exclusive =
        Address_table.add t.address_table end_pos
          ~adjustment:end_pos_offset
          ~start_of_code_symbol:t.start_of_code_symbol
      in
      let loc_desc =
        Counted_location_description.create single_location_description
      in
      let location_list_entry : Location_list_entry.entry =
        (* DWARF-5 spec page 45 line 1. *)
        Startx_endx {
          start_inclusive;
          end_exclusive;
          payload = loc_desc;
        }
      in
      Some (Dwarf_5 (Location_list_entry.create location_list_entry
        ~start_of_code_symbol:t.start_of_code_symbol))

let find_scope_die_from_debuginfo ~whole_function_lexical_block dbg
      ~scope_proto_dies =
  let block = Debuginfo.innermost_block dbg in
  match Debuginfo.Current_block.to_block block with
  | Toplevel -> Some whole_function_lexical_block
  | Block block ->
    let module B = Debuginfo.Block in
    match B.Map.find block scope_proto_dies with
    | exception Not_found -> None
    | proto_die -> Some proto_die

let dwarf_for_variable t (fundecl : L.fundecl)
      ~function_proto_die ~whole_function_lexical_block
      ~scope_proto_dies ~uniqueness_by_var ~proto_dies_for_vars
      ~need_rvalue (var : Backend_var.t) ~phantom:_ ~hidden ~ident_for_type
      ~range =
  let range_info = ARV.Range.info range in
  let provenance = ARV.Range_info.provenance range_info in
  let var_is_a_parameter_of_fundecl_itself =
    match ARV.Range_info.is_parameter range_info with
    | Local -> false
    | Parameter _ -> true
  in
  let phantom_defining_expr = ARV.Range_info.phantom_defining_expr range_info in
  let (parent_proto_die : Proto_die.t), hidden =
    if var_is_a_parameter_of_fundecl_itself then
      function_proto_die, hidden
    else
      (* Local variables need to be children of "lexical blocks", which in turn
         are children of the function.  It is important to generate accurate
         lexical block information to avoid large numbers of variables, many
         of which may be out of scope, being visible in the debugger at the
         same time. *)
      match provenance with
      | None ->
        (* Any variable without provenance gets hidden. *)
        whole_function_lexical_block, true
      | Some provenance ->
        let dbg = Backend_var.Provenance.debuginfo provenance in
        let block_die =
          find_scope_die_from_debuginfo ~whole_function_lexical_block
            dbg ~scope_proto_dies
        in
        match block_die with
        | Some block_die -> block_die, hidden
        | None ->
          (* There are be no instructions marked with the block in which
             [var] was defined.  For the moment, just hide [var], and put
             it in the toplevel scope for the function. *)
          whole_function_lexical_block, true
  in
  (* Build a location list that identifies where the value of [ident] may be
     found at runtime, indexed by program counter range.
     The representations of location lists (and range lists, used below to
     describe lexical blocks) changed completely between DWARF-4 and
     DWARF-5. *)
  let location_list_attribute_value =
    let dwarf_4_location_list_entries, location_list =
      ARV.Range.fold range
        ~init:([], Location_list.create ())
        ~f:(fun (dwarf_4_location_list_entries, location_list) subrange ->
          let location_list_entry =
            location_list_entry t fundecl ~parent:(Some function_proto_die)
              ~subrange ~proto_dies_for_vars ~need_rvalue
          in
          match location_list_entry with
          | None -> dwarf_4_location_list_entries, location_list
          | Some (Dwarf_4 location_list_entry) ->
            let dwarf_4_location_list_entries =
              location_list_entry :: dwarf_4_location_list_entries
            in
            dwarf_4_location_list_entries, location_list
          | Some (Dwarf_5 location_list_entry) ->
            let location_list =
              Location_list.add location_list location_list_entry
            in
            dwarf_4_location_list_entries, location_list)
    in
    match !Clflags.dwarf_version with
    | Four ->
      let base_address_selection_entry =
        let fun_symbol = Asm_symbol.create fundecl.fun_name in
        Dwarf_4_location_list_entry.create_base_address_selection_entry
          ~base_address_symbol:fun_symbol
      in
      let location_list_entries =
        base_address_selection_entry :: dwarf_4_location_list_entries
      in
      let location_list = Dwarf_4_location_list.create ~location_list_entries in
      Debug_loc_table.insert t.debug_loc_table ~location_list
    | Five ->
      let location_list_index =
        Location_list_table.add t.location_list_table location_list
      in
      DAH.create_location location_list_index
  in
  let type_and_name_attributes =
    match type_die_reference_for_var var ~proto_dies_for_vars with
    | None -> []
    | Some reference ->
      let name_is_unique, position_is_unique =
        match Backend_var.Tbl.find uniqueness_by_var var with
        | exception Not_found ->
          Misc.fatal_errorf "No uniqueness information for %a"
            Backend_var.print var
        | { name_is_unique; position_is_unique; } ->
          name_is_unique, position_is_unique
      in
      let name_for_var =
        (* If the unstamped name of [ident] is unambiguous within the
           function, then use it; otherwise, equip the name with the location
           of its definition together with any inlined-out frames.
           It might seem that parameters always have unique names, but they
           don't, in the case of ones that weren't named in the source code. *)
        (* CR-soon mshinwell: There are certain cases (e.g. let-bindings in
           [Lambda]) which currently lack location information or (e.g.
           inlined functions' parameters in [Closure]) where locations may
           alias.  For these ones we disambiguate using the stamp. *)
        if name_is_unique then Backend_var.name var
        else if not position_is_unique then Backend_var.unique_toplevel_name var
        else
          match provenance with
          | None -> Backend_var.unique_toplevel_name var
          | Some provenance ->
            let dbg = Backend_var.Provenance.debuginfo provenance in
            match Debuginfo.position dbg with
            | None -> Backend_var.unique_toplevel_name var
            | Some position ->
              Format.asprintf "%s[%a]"
                (Backend_var.name var)
                Debuginfo.Code_range.print_compact position
      in
      (* CR-someday mshinwell: This should be tidied up.  It's only correct by
         virtue of the fact we do the closure-env ones second below. *)
      (* CR mshinwell: re-check this CR-someday *)
      let type_attribute =
        if not need_rvalue then begin
          construct_type_of_value_description t
            ~parent:(Some t.compilation_unit_proto_die)
            ident_for_type ~phantom_defining_expr ~proto_dies_for_vars
            ~reference;
          [DAH.create_type_from_reference ~proto_die_reference:reference;
          ]
        end else begin
          []
        end
      in
      let name_attribute =
        if hidden || need_rvalue then []
        else [DAH.create_name name_for_var]
      in
      name_attribute @ type_attribute
  in
  let is_parameter =
    let is_parameter_from_provenance =
      match provenance with
      | None -> Is_parameter.local
      | Some provenance -> Backend_var.Provenance.is_parameter provenance
    in
    (* The two inputs here correspond to:
       1. The normal case of parameters of function declarations, which are
          identified in [Selectgen].
       2. Parameters of inlined functions, which have to be tagged much
          earlier, on [let]-bindings when inlining is performed. *)
    Is_parameter.join (ARV.Range_info.is_parameter range_info)
      is_parameter_from_provenance
  in
  let tag : Dwarf_tag.t =
    match is_parameter with
    | Parameter _index -> Formal_parameter
    | Local -> Variable
  in
  let reference =
    match proto_dies_for_variable var ~proto_dies_for_vars with
    | None -> None
    | Some proto_dies ->
      if need_rvalue then Some proto_dies.value_die_rvalue
      else Some proto_dies.value_die_lvalue
  in
  let sort_priority =
    match is_parameter with
    | Local -> None
    | Parameter { index; } ->
      (* Ensure that parameters appear in the correct order in the debugger. *)
      Some index
  in
  Proto_die.create_ignore ?reference
    ?sort_priority
    ~parent:(Some parent_proto_die)
    ~tag
    ~attribute_values:(type_and_name_attributes @ [
      location_list_attribute_value;
    ])
    ()

(* This function covers local variables, parameters, variables in closures
   and other "fun_var"s in the current mutually-recursive set.  (The last
   two cases are handled by the explicit addition of phantom lets way back
   in [Flambda_to_clambda].)  Phantom variables are also covered. *)
let iterate_over_variable_like_things t ~available_ranges_vars ~rvalues_only
      ~f =
  ARV.iter available_ranges_vars ~f:(fun var range ->
    let should_process =
      (not rvalues_only)
        || V.Set.mem var t.rvalue_dies_required_for
    in
    if should_process then begin
      let range_info = ARV.Range.info range in
      let provenance = ARV.Range_info.provenance range_info in
      let phantom : is_variable_phantom =
        match ARV.Range_info.phantom_defining_expr range_info with
        | Non_phantom -> Non_phantom
        | Phantom _ -> Phantom
      in
      (* There are two variables in play here:
         1. [var] is the "real" variable that is used for obtaining a value
            at runtime in the debugger.
         2. [ident_for_type] is the corresponding identifier with the stamp
            as in the typed tree together with its original compilation unit.
            This is the one used for lookup in .cmt files to retrieve a type.
         We cannot conflate these since the multiple [vars] that might
         be associated with a given [ident_for_type] (due to inlining) may
         not all have the same value. *)
      (* CR-soon mshinwell: Default arguments currently appear as local
         variables, not parameters. *)
      (* CR-someday mshinwell: Introduce some flag on Backend_var.t to mark
         identifiers that were generated internally (or vice-versa)? *)
      let hidden =
        let name = Backend_var.name var in
        String.length name >= 1
          && String.get name 0 = '*'
          && String.get name (String.length name - 1) = '*'
      in
      let ident_for_type =
        match provenance with
        | None ->
          (* In this case the variable won't be given a name in the DWARF,
             so as not to appear in the debugger; but we still need to emit
             a DIE for it, as it may be referenced as part of some chain of
             phantom lets. *)
          None
        | Some provenance ->
          Some (Backend_var.Provenance.ident_for_type provenance)
      in
      f var ~phantom ~hidden ~ident_for_type ~range
    end)

let dwarf_for_variables_and_parameters t fundecl ~function_proto_die
      ~whole_function_lexical_block ~scope_proto_dies
      ~available_ranges_vars =
  let proto_dies_for_vars = Backend_var.Tbl.create 42 in
  let uniqueness_by_var =
    calculate_var_uniqueness ~available_ranges_vars
  in
  iterate_over_variable_like_things t ~available_ranges_vars
    ~rvalues_only:false
    ~f:(fun var ~phantom ~hidden:_ ~ident_for_type:_ ~range:_ ->
      let value_die_lvalue = Proto_die.create_reference () in
      let value_die_rvalue = Proto_die.create_reference () in
      let type_die = Proto_die.create_reference () in
      assert (not (Backend_var.Tbl.mem proto_dies_for_vars var));
      Backend_var.Tbl.add proto_dies_for_vars var
        { is_variable_phantom = phantom;
          value_die_lvalue;
          value_die_rvalue;
          type_die;
        });
  t.rvalue_dies_required_for <- V.Set.empty;
  (* CR-someday mshinwell: Consider changing [need_rvalue] to use a variant
     type "lvalue or rvalue". *)
  iterate_over_variable_like_things t ~available_ranges_vars
    ~rvalues_only:false
    ~f:(dwarf_for_variable t fundecl ~function_proto_die
      ~whole_function_lexical_block ~scope_proto_dies
      ~uniqueness_by_var ~proto_dies_for_vars ~need_rvalue:false);
  iterate_over_variable_like_things t ~available_ranges_vars
    ~rvalues_only:true
    ~f:(dwarf_for_variable t fundecl ~function_proto_die
      ~whole_function_lexical_block ~scope_proto_dies
      ~uniqueness_by_var ~proto_dies_for_vars ~need_rvalue:true)

let add_abstract_instance t fun_dbg =
  let function_name = Debuginfo.Function.name fun_dbg in
  let is_visible_externally =
    Debuginfo.Function.is_visible_externally fun_dbg
  in
  let abstract_instance_proto_die =
    (* DWARF-5 specification section 3.3.8.1, page 82. *)
    Proto_die.create ~parent:(Some t.compilation_unit_proto_die)
      ~tag:Subprogram
      ~attribute_values:[
        DAH.create_name function_name;
        DAH.create_external ~is_visible_externally;
        (* We assume every function might potentially be inlined (and possibly
           in the future), so we choose [DW_INL_inlined] as the most appropriate
           setting for [DW_AT_inline], even if it doesn't seem exactly
           correct.  We must set something here to ensure that the subprogram
           is marked as an abstract instance root. *)
        DAH.create_inline Inlined;
      ]
      ()
  in
  let id = Debuginfo.Function.id fun_dbg in
  let abstract_instance_proto_die_symbol =
    Name_laundry.abstract_instance_root_die_name id
  in
  Proto_die.set_name abstract_instance_proto_die
    abstract_instance_proto_die_symbol;
  Debuginfo.Function.Id.Tbl.add t.function_abstract_instances id
    (abstract_instance_proto_die, abstract_instance_proto_die_symbol);
  abstract_instance_proto_die, abstract_instance_proto_die_symbol

let find_or_add_abstract_instance t fun_dbg =
  let id = Debuginfo.Function.id fun_dbg in
  match
    Debuginfo.Function.Id.Tbl.find t.function_abstract_instances id
  with
  | exception Not_found -> add_abstract_instance t fun_dbg
  | existing_instance -> existing_instance

let find_maybe_in_another_unit_or_add_abstract_instance t fun_dbg =
  if not (Debuginfo.Function.dwarf_die_present fun_dbg) then
    None
  else
    let id = Debuginfo.Function.id fun_dbg in
    let dbg_comp_unit = Debuginfo.Function.Id.compilation_unit id in
    let this_comp_unit = Compilation_unit.get_current_exn () in
    let abstract_instance_proto_die_symbol =
      if Compilation_unit.equal dbg_comp_unit this_comp_unit then
        let _abstract_instance_proto_die, abstract_instance_proto_die_symbol =
          find_or_add_abstract_instance t fun_dbg
        in
        abstract_instance_proto_die_symbol
      else
        Name_laundry.abstract_instance_root_die_name id
    in
    Some abstract_instance_proto_die_symbol

let create_range_list_and_summarise t (fundecl : L.fundecl) range =
  LB.Range.fold range
    ~init:([], Range_list.create (), Address_index.Pair.Set.empty)
    ~f:(fun (dwarf_4_range_list_entries, range_list, summary) subrange ->
      let start_pos = LB.Subrange.start_pos subrange in
      let end_pos = LB.Subrange.end_pos subrange in
      let end_pos_offset = LB.Subrange.end_pos_offset subrange in
      let start_inclusive =
        Address_table.add t.address_table (Asm_label.create_int start_pos)
          ~start_of_code_symbol:t.start_of_code_symbol
      in
      let end_exclusive =
        Address_table.add t.address_table (Asm_label.create_int end_pos)
          ~adjustment:(LB.Subrange.end_pos_offset subrange)
          ~start_of_code_symbol:t.start_of_code_symbol
      in
      let range_list_entry : Range_list_entry.entry =
        (* DWARF-5 spec page 54 line 1. *)
        Startx_endx {
          start_inclusive;
          end_exclusive;
          payload = ();
        }
      in
      let range_list_entry =
        Range_list_entry.create range_list_entry
          ~start_of_code_symbol:t.start_of_code_symbol
      in
      (* We still use the [Range_list] when emitting DWARF-4 (even though
         it is a DWARF-5 structure) for the purposes of de-duplicating ranges. *)
      let range_list = Range_list.add range_list range_list_entry in
      let summary =
        Address_index.Pair.Set.add (start_inclusive, end_exclusive) summary
      in
      let dwarf_4_range_list_entries =
        match !Clflags.dwarf_version with
        | Four ->
          let range_list_entry =
            Dwarf_4_range_list_entry.create_range_list_entry
              ~start_of_code_symbol:(Asm_symbol.create fundecl.fun_name)
              ~first_address_when_in_scope:(Asm_label.create_int start_pos)
              ~first_address_when_not_in_scope:(Asm_label.create_int end_pos)
              ~first_address_when_not_in_scope_offset:(Some end_pos_offset)
          in
          range_list_entry :: dwarf_4_range_list_entries
        | Five -> dwarf_4_range_list_entries
      in
      dwarf_4_range_list_entries, range_list, summary)

(* "Summaries", sets of pairs of the starting and ending points of ranges,
   are used to dedup entries in the range list table.  We do this for range
   lists but not yet for location lists since deduping entries in the latter
   would involve comparing DWARF location descriptions. *)
module All_summaries = Identifiable.Make (struct
  include Address_index.Pair.Set
  let hash t = Hashtbl.hash (elements t)
end)

let create_lexical_block_and_inlined_frame_proto_dies t (fundecl : L.fundecl)
      lexical_block_ranges ~function_proto_die
      ~start_of_function ~end_of_function
      : Proto_die.t * (Proto_die.t Debuginfo.Block.Map.t) =
  let module B = Debuginfo.Block in
  let whole_function_lexical_block =
    Proto_die.create ~parent:(Some function_proto_die)
      ~tag:Lexical_block
      ~attribute_values:[
        start_of_function;
        end_of_function;
      ]
      ()
  in
  let all_blocks = LB.all_indexes lexical_block_ranges in
  let scope_proto_dies, _all_summaries =
    B.Set.fold (fun block (scope_proto_dies, all_summaries) ->
        let range = LB.find lexical_block_ranges block in
        let rec create_up_to_root block scope_proto_dies all_summaries =
          match B.Map.find block scope_proto_dies with
          | proto_die ->
            proto_die, scope_proto_dies, all_summaries
          | exception Not_found ->
            let parent, scope_proto_dies, all_summaries =
              match B.parent block with
              | None ->
                function_proto_die, scope_proto_dies, all_summaries
              | Some parent ->
                create_up_to_root parent scope_proto_dies all_summaries
            in
            let range_list_attribute, all_summaries =
              let dwarf_4_range_list_entries, range_list, summary =
                create_range_list_and_summarise t fundecl range
              in
              match All_summaries.Map.find summary all_summaries with
              | exception Not_found ->
                let range_list_index =
                  Range_list_table.add t.range_list_table range_list
                in
                let range_list_attribute =
                  match !Clflags.dwarf_version with
                  | Four ->
                    let base_address_selection_entry =
                      Dwarf_4_range_list_entry.
                        create_base_address_selection_entry
                          ~base_address_symbol:
                            (Asm_symbol.create fundecl.fun_name)
                    in
                    let range_list_entries =
                      base_address_selection_entry
                        :: dwarf_4_range_list_entries
                    in
                    let range_list =
                      Dwarf_4_range_list.create ~range_list_entries
                    in
                    Debug_ranges_table.insert t.debug_ranges_table ~range_list
                  | Five ->
                    DAH.create_ranges range_list_index
                in
                let all_summaries =
                  All_summaries.Map.add summary range_list_attribute
                    all_summaries
                in
                range_list_attribute, all_summaries
              | range_list_attribute ->
                range_list_attribute, all_summaries
            in
            let proto_die =
              match B.frame_classification block with
              | Lexical_scope_only ->
                Proto_die.create ~parent:(Some parent)
                  ~tag:Lexical_block
                  ~attribute_values:[
                    range_list_attribute;
                  ]
                  ()
              | Inlined_frame call_site ->
                let fun_dbg = Debuginfo.Call_site.fun_dbg call_site in
                let abstract_instance_symbol =
                  find_maybe_in_another_unit_or_add_abstract_instance t fun_dbg
                in
                let range = LB.find lexical_block_ranges block in
                let entry_pc =
                  (* CR-someday mshinwell: The "entry PC" is supposed to be the
                     address of the "temporally first" instruction of the
                     inlined function.  We assume here that we don't do
                     transformations which might cause the first instruction
                     of the inlined function to not be the one at the lowest
                     address amongst all instructions of the inlined function.
                     If this assumption is wrong the most likely outcome seems
                     to be breakpoints being slightly in the wrong place,
                     although still in the correct function.  Making this
                     completely accurate will necessitate more tracking of
                     instruction ordering from earlier in the compiler. *)
                  match LB.Range.lowest_address range with
                  | None -> []
                  | Some lowest_address -> [
                      DAH.create_entry_pc (Asm_label.create_int lowest_address);
                    ]
                in
                (* Note that with Flambda, this DIE may not be in the scope
                   of the referenced abstract instance DIE, as inline
                   expansions may be made out of the scope of the function
                   declaration. *)
                let abstract_instance =
                  match abstract_instance_symbol with
                  | None -> []
                  | Some abstract_instance_symbol ->
                    [DAH.create_abstract_origin
                       ~die_symbol:abstract_instance_symbol]
                in
                let code_range = Debuginfo.Call_site.position call_site in
                Proto_die.create ~parent:(Some parent)
                  ~tag:Inlined_subroutine
                  ~attribute_values:(entry_pc @ abstract_instance @ [
                    range_list_attribute;
                    (* See comment below about the use of the number 1 here. *)
                    DAH.create_call_file 1;
                    DAH.create_call_line
                      (Debuginfo.Code_range.line code_range);
                    DAH.create_call_column
                      (Debuginfo.Code_range.char_start code_range);
                  ])
                  ()
            in
            let scope_proto_dies =
              B.Map.add block proto_die scope_proto_dies
            in
            proto_die, scope_proto_dies, all_summaries
        in
        let _proto_die, scope_proto_dies, all_summaries =
          create_up_to_root block scope_proto_dies all_summaries
        in
        scope_proto_dies, all_summaries)
      all_blocks
      (B.Map.empty, All_summaries.Map.empty)
  in
  whole_function_lexical_block, scope_proto_dies

(* CR mshinwell: Share with [Available_ranges_vars]. *)
let offset_from_cfa_in_bytes reg stack_loc ~stack_offset =
  let frame_size = Proc.frame_size ~stack_offset in
  let slot_offset =
    Proc.slot_offset stack_loc ~reg_class:(Proc.register_class reg)
      ~stack_offset
  in
  Some (frame_size - slot_offset)

let add_call_site_argument t ~call_site_die ~is_tail ~arg_index ~(arg : Reg.t)
      ~stack_offset (insn : L.instruction) =
  let param_location =
    let offset_from_cfa_in_bytes =
      match arg.loc with
      | Stack stack_loc ->
        offset_from_cfa_in_bytes arg stack_loc ~stack_offset
      | Reg _ -> None
      | Unknown ->
        Misc.fatal_errorf "Register without location: %a"
          Printmach.reg arg
    in
    let param_location =
      reg_location_description0 arg ~offset_from_cfa_in_bytes
        ~need_rvalue:false
    in
    match param_location with
    | None -> []
    | Some param_location ->
      let param_location =
        Single_location_description.of_simple_location_description
          param_location
      in
      [DAH.create_single_location_description param_location]
  in
  let arg_location, type_attribute =
    (* The reason we call [Reg_availability_set.canonicalise] from
       [Available_ranges] vars, rather than traversing each function's
       code and rewriting all of the availability sets first, is so that
       we don't lose information (in particular that a particular hard
       register before a call contains the value of a certain variable)
       that we may need here.  (Specifically for the call to
       [Reg_availability_set.find_all_holding_value_of].) *)
    match Reg_availability_set.find_reg_opt insn.available_before arg with
    | None -> [], []
    | Some rd ->
      match Reg_with_debug_info.debug_info rd with
      | None -> [], []
      | Some debug_info ->
        let holds_value_of =
          Reg_with_debug_info.Debug_info.holds_value_of debug_info
        in
        let type_attribute =
          match Reg_with_debug_info.Debug_info.provenance debug_info with
          | None -> []
          | Some provenance ->
            let type_die =
              (* CR mshinwell: This should reuse DIEs which were created
                 previously to describe these vars.  Also, shouldn't these
                 DIEs be parented higher up? *)
              normal_type_for_var t ~parent:(Some call_site_die)
                (Some (Backend_var.Provenance.ident_for_type provenance))
            in
            [DAH.create_type_from_reference
              ~proto_die_reference:(Proto_die.reference type_die)
            ]
        in
        let arg_location =
          if is_tail then begin
            (* If this is a tail call site, no arguments of the call will be
               available with certainty in the callee, since any argument we
               describe in DWARF (see comment below) will be on the stack---and
               our stack frame may have been overwritten by the time such
               argument is queried in the debugger. *)
            []
          end else begin
            let everywhere_holding_var =
              Reg_availability_set.find_all_holding_value_of
                insn.available_before holds_value_of
            in
            (* Only registers spilled at the time of the call will be available
               with certainty (in the caller's frame) during the execution of
               the callee. *)
            let on_stack =
              Misc.Stdlib.List.filter_map (fun rd ->
                  let reg = Reg_with_debug_info.reg rd in
                  match reg.loc with
                  | Stack stack_loc ->
                    Some (reg,
                      offset_from_cfa_in_bytes reg stack_loc ~stack_offset)
                  | Reg _ -> None
                  | Unknown ->
                    Misc.fatal_errorf "Register without location: %a"
                      Printmach.reg reg)
                everywhere_holding_var
            in
            match on_stack with
            | [] -> []
            | (reg, offset_from_cfa_in_bytes) :: _ ->
              let arg_location_rvalue =
                reg_location_description0 reg ~offset_from_cfa_in_bytes
                  ~need_rvalue:true
              in
              match arg_location_rvalue with
              | None -> []
              | Some arg_location_rvalue ->
                let _arg_location =
                  Single_location_description.of_simple_location_description
                    arg_location_rvalue
                in
                (* gdb does not seem to accept a simple location description
                   here -- it complains about the use of [DW_op_stack_value]. *)
                let composite =
                  Composite_location_description.
                    pieces_of_simple_location_descriptions
                      [arg_location_rvalue, arch_size_addr]
                in
                [DAH.create_composite_call_value_location_description composite;
                ]
          end
        in
        arg_location, type_attribute
  in
  let tag : Dwarf_tag.t =
    match !Clflags.dwarf_version with
    | Four -> Dwarf_4 GNU_call_site_parameter
    | Five -> Call_site_parameter
  in
  (* We don't give the name of the parameter since it is
     complicated to calculate (and there is currently insufficient
     information to perform the calculation if the function is in
     a different compilation unit). *)
  Proto_die.create_ignore ~sort_priority:arg_index
    ~parent:(Some call_site_die)
    ~tag
    ~attribute_values:(arg_location @ param_location @ type_attribute)
    ()

let add_call_site t ~whole_function_lexical_block ~scope_proto_dies
      ~stack_offset ~is_tail ~args ~(call_labels : Mach.call_labels)
      (insn : L.instruction) attrs =
  let dbg = insn.dbg in
  let block_die =
    find_scope_die_from_debuginfo ~whole_function_lexical_block dbg
      ~scope_proto_dies
  in
  match block_die with
  | None ->
    Misc.fatal_errorf "No lexical block DIE found for debuginfo (the block \
        should always exist since this debuginfo came from a [Linearize] \
        instruction, not a [Backend_var]):@ %a"
      Debuginfo.print dbg
  | Some block_die ->
    let position_attrs =
      match Debuginfo.position dbg with
      | None -> []
      | Some code_range ->
        match !Clflags.dwarf_version with
        | Four -> []
        | Five -> [
            (* We assume that the current source file will always be
               numbered 1 by the assembler (which generates .debug_line at the
               moment). *)
            DAH.create_call_file 1;
            DAH.create_call_line (Debuginfo.Code_range.line code_range);
            DAH.create_call_column (Debuginfo.Code_range.char_start code_range);
          ]
    in
    let call_site_die =
      let dwarf_5_only =
        match !Clflags.dwarf_version with
        | Four -> [
            DAH.create_low_pc (Asm_label.create_int call_labels.after);
          ]
        | Five -> [
            DAH.create_call_pc (Asm_label.create_int call_labels.before);
            DAH.create_call_return_pc (Asm_label.create_int call_labels.after);
          ]
      in
      let tag : Dwarf_tag.t =
        match !Clflags.dwarf_version with
        | Four -> Dwarf_4 GNU_call_site
        | Five -> Call_site
      in
      Proto_die.create ~parent:(Some block_die)
        ~tag
        ~attribute_values:(attrs @ position_attrs @ dwarf_5_only @ [
          DAH.create_call_tail_call ~is_tail;
        ])
        ()
    in
    (* CR-someday mshinwell: For the moment, don't generate argument
       information if one or more of the arguments is split across registers.
       This could be improved in the future. *)
    let no_split_args =
      Array.for_all (fun (arg : Reg.t) ->
          match arg.part with
          | None -> true
          | Some _ -> false)
        args
    in
    if no_split_args then begin
      Array.iteri (fun arg_index arg ->
          add_call_site_argument t ~call_site_die ~is_tail ~arg_index ~arg
            ~stack_offset insn)
        args
    end

type direct_callee =
  | Ocaml of Debuginfo.Function.t
  | External of Asm_symbol.t

let call_target_for_direct_callee t (callee : direct_callee) =
  let die_symbol =
    match callee with
    | Ocaml callee_dbg ->
      if not (Debuginfo.Function.dwarf_die_present callee_dbg) then None
      else
        let id = Debuginfo.Function.id callee_dbg in
        Some (Name_laundry.concrete_instance_die_name id)
    | External callee ->
      match
        Asm_symbol.Tbl.find t.die_symbols_for_external_declarations callee
      with
      | exception Not_found ->
        (* CR-someday mshinwell: dedup DIEs for runtime functions across
           compilation units (maybe only generate the DIEs when compiling the
           startup file)? *)
        let callee_die =
          Proto_die.create ~parent:(Some t.compilation_unit_proto_die)
            ~tag:Subprogram
            ~attribute_values:[
            ]
            ()
        in
        let die_symbol =
          Name_laundry.external_declaration_die_name callee
            (Compilation_unit.get_current_exn ())
        in
        Proto_die.set_name callee_die die_symbol;
        Asm_symbol.Tbl.add t.die_symbols_for_external_declarations
          callee die_symbol;
        Some die_symbol
      | die_symbol -> Some die_symbol
  in
  match die_symbol with
  | None -> []
  | Some die_symbol -> [DAH.create_call_origin ~die_symbol]

let call_target_for_indirect_callee ~(callee : Reg.t) ~stack_offset =
  let offset_from_cfa_in_bytes, clobbered_by_call =
    match callee.loc with
    | Stack stack_loc ->
      offset_from_cfa_in_bytes callee stack_loc ~stack_offset, false
    | Reg _ -> None, true
    | Unknown ->
      Misc.fatal_errorf "Register without location: %a" Printmach.reg callee
  in
  let simple_location_desc =
    reg_location_description0 callee ~offset_from_cfa_in_bytes
      ~need_rvalue:false
  in
  match simple_location_desc with
  | None -> []
  | Some simple_location_desc ->
    let location_desc =
      Single_location_description.of_simple_location_description
        simple_location_desc
    in
    (* It seems unlikely that we won't be calling through a [Reg], but we
       support the stack case (yielding [DW_AT_call_target] rather than the
       "clobbered" variant) for completeness. *)
    if clobbered_by_call then
      [DAH.create_call_target_clobbered location_desc] 
    else
      [DAH.create_call_target location_desc] 

let dwarf_for_call_sites t ~whole_function_lexical_block
      ~scope_proto_dies ~(fundecl : L.fundecl)
      ~external_calls_generated_during_emit ~function_symbol =
  let found_self_tail_calls = ref false in
  let add_call_site =
    add_call_site t ~whole_function_lexical_block ~scope_proto_dies
  in
  let add_indirect_ocaml_call ~stack_offset ~callee ~args ~call_labels insn =
    add_call_site ~stack_offset ~is_tail:false ~args ~call_labels insn
      (call_target_for_indirect_callee ~callee ~stack_offset)
  in
  let add_direct_ocaml_call ~stack_offset ~callee_dbg ~args ~call_labels insn =
    match callee_dbg with
    | None -> ()
    | Some callee_dbg ->
      add_call_site ~stack_offset ~is_tail:false ~args ~call_labels insn
        (call_target_for_direct_callee t (Ocaml callee_dbg))
  in
  let add_indirect_ocaml_tail_call ~stack_offset ~callee ~args ~call_labels
        insn =
    add_call_site ~stack_offset ~is_tail:true ~args ~call_labels insn
      (call_target_for_indirect_callee ~callee ~stack_offset)
  in
  let add_direct_ocaml_tail_call ~stack_offset ~callee ~callee_dbg
        ~args ~call_labels insn =
    match callee_dbg with
    | None -> ()
    | Some callee_dbg ->
      (* DWARF-5 spec section 3.4, page 89, lines 19--22: [DW_TAG_call_site]s
         are not to be generated for self tail calls (called "tail recursion
         calls" in the spec). *)
      if Asm_symbol.equal callee function_symbol
        && not !Clflags.dwarf_emit_self_tail_calls
      then begin
        found_self_tail_calls := true
      end else begin
        add_call_site ~stack_offset ~is_tail:true ~args ~call_labels insn
          (call_target_for_direct_callee t (Ocaml callee_dbg))
      end
  in
  let add_external_call ~stack_offset ~callee ~args ~call_labels insn =
    add_call_site ~stack_offset ~is_tail:false ~args ~call_labels insn
      (call_target_for_direct_callee t (External callee))
  in
  let rec traverse_insns (insn : L.instruction) ~stack_offset =
    match insn.desc with
    | Lend -> stack_offset
    | Lop op ->
      let stack_offset =
        match op with
        | Icall_ind { call_labels; } ->
          let args = Array.sub insn.arg 1 (Array.length insn.arg - 1) in
          add_indirect_ocaml_call ~stack_offset
            ~callee:insn.arg.(0) ~args ~call_labels insn;
          stack_offset
        | Icall_imm { callee_dbg; call_labels; _ } ->
          add_direct_ocaml_call ~stack_offset
            ~callee_dbg ~args:insn.arg ~call_labels insn;
          stack_offset
        | Itailcall_ind { call_labels; } ->
          let args = Array.sub insn.arg 1 (Array.length insn.arg - 1) in
          add_indirect_ocaml_tail_call ~stack_offset
            ~callee:insn.arg.(0) ~args ~call_labels insn;
          stack_offset
        | Itailcall_imm { func = callee; callee_dbg; call_labels; _ } ->
          let callee = Asm_symbol.create callee in
          add_direct_ocaml_tail_call ~stack_offset
            ~callee ~callee_dbg ~args:insn.arg ~call_labels insn;
          stack_offset
        | Iextcall { func = callee; alloc = _; call_labels; } ->
          let callee = Asm_symbol.create callee in
          add_external_call ~stack_offset
            ~callee ~args:insn.arg ~call_labels insn;
          stack_offset
        | Imove
        | Ispill
        | Ireload
        | Iconst_int _
        | Iconst_float _
        | Iconst_symbol _
        | Iload _
        | Istore _
        | Ialloc _
        | Iintop _
        | Iintop_imm _
        | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
        | Ifloatofint | Iintoffloat
        | Ispecific _
        | Iname_for_debugger _ -> stack_offset
        | Istackoffset delta -> stack_offset + delta
      in
      traverse_insns insn.next ~stack_offset
    | Lprologue
    | Lreloadretaddr
    | Lreturn
    | Llabel _
    | Lbranch _
    | Lcondbranch _
    | Lcondbranch3 _
    | Lswitch _
    | Lsetuptrap _
    | Lraise _ -> traverse_insns insn.next ~stack_offset
    | Lpushtrap ->
      traverse_insns insn.next
        ~stack_offset:(stack_offset + Proc.trap_frame_size_in_bytes)
    | Lpoptrap ->
      traverse_insns insn.next
        ~stack_offset:(stack_offset - Proc.trap_frame_size_in_bytes)
  in
  let (_stack_offset : int) =
    traverse_insns fundecl.fun_body ~stack_offset:Proc.initial_stack_offset
  in
  List.iter (fun ({ callee; call_labels; call_dbg = dbg; }
        : Emitaux.external_call_generated_during_emit) ->
      (* We omit [DW_tag_call_site_parameter] for these calls.  As such the
         [available_before] information and the stack offset is irrelevant
         here. *)
      let fake_insn : L.instruction =
        { L.end_instr with
          dbg;
        }
      in
      add_external_call ~stack_offset:0 ~callee ~args:[| |] ~call_labels
        fake_insn)
    external_calls_generated_during_emit;
  !found_self_tail_calls

let passes_for_fundecl (fundecl : L.fundecl) =
  let available_ranges_vars, fundecl =
    Profile.record "dwarf_available_ranges_vars" (fun fundecl ->
        Available_ranges_vars.create fundecl)
      ~accumulate:true
      fundecl
  in
  let available_ranges_phantom_vars, fundecl =
    Profile.record "dwarf_available_ranges_phantom_vars" (fun fundecl ->
        Available_ranges_phantom_vars.create fundecl)
      ~accumulate:true
      fundecl
  in
  let lexical_block_ranges, fundecl =
    Profile.record "dwarf_lexical_block_ranges" (fun fundecl ->
        Lexical_block_ranges.create fundecl)
      ~accumulate:true
      fundecl
  in
  let available_ranges_vars, available_ranges_phantom_vars,
        lexical_block_ranges, fundecl =
    let label_env, fundecl =
      Profile.record "dwarf_coalesce_labels" (fun () ->
          Coalesce_labels.fundecl fundecl)
        ~accumulate:true
        ()
    in
    let available_ranges_vars =
      Profile.record "dwarf_rewrite_labels_vars" (fun () ->
          Available_ranges_vars.rewrite_labels available_ranges_vars
            ~env:label_env)
        ~accumulate:true
        ()
    in
    let available_ranges_phantom_vars =
      Profile.record "dwarf_rewrite_labels_phantom_vars" (fun () ->
          Available_ranges_phantom_vars.rewrite_labels
            available_ranges_phantom_vars
            ~env:label_env)
        ~accumulate:true
        ()
    in
    let lexical_block_ranges =
      Profile.record "dwarf_rewrite_labels_lexical_blocks" (fun () ->
          Lexical_block_ranges.rewrite_labels lexical_block_ranges
            ~env:label_env)
        ~accumulate:true
        ()
    in
    available_ranges_vars, available_ranges_phantom_vars,
      lexical_block_ranges, fundecl
  in
  let available_ranges_vars =
    Available_ranges_all_vars.create ~available_ranges_vars
      ~available_ranges_phantom_vars
  in
  available_ranges_vars, lexical_block_ranges, fundecl

let dwarf_for_fundecl_and_emit t ~emit ~end_of_function_label
      (fundecl : L.fundecl) =
  let available_ranges_vars, lexical_block_ranges, fundecl =
    passes_for_fundecl fundecl
  in
  let external_calls_generated_during_emit =
    emit fundecl ~end_of_function_label
  in
  let symbol = Asm_symbol.create fundecl.fun_name in
  let start_of_function = DAH.create_low_pc_from_symbol symbol in
  let end_of_function =
    DAH.create_high_pc (Asm_label.create_int end_of_function_label)
  in
  let _abstract_instance_proto_die, abstract_instance_die_symbol =
    find_or_add_abstract_instance t fundecl.fun_dbg
  in
  let concrete_instance_proto_die =
    Proto_die.create ~parent:(Some t.compilation_unit_proto_die)
      ~tag:Subprogram
      ~attribute_values:[
        start_of_function;
        end_of_function;
        DAH.create_entry_pc_from_symbol symbol;
        DAH.create_abstract_origin
          ~die_symbol:abstract_instance_die_symbol;
      ]
      ()
  in
  Proto_die.set_name concrete_instance_proto_die
    (Name_laundry.concrete_instance_die_name
      (Debuginfo.Function.id fundecl.fun_dbg));
  let whole_function_lexical_block, scope_proto_dies =
    Profile.record "dwarf_create_lexical_block_and_inlined_frame_proto_dies"
      (fun () ->
        create_lexical_block_and_inlined_frame_proto_dies t
          fundecl lexical_block_ranges
          ~function_proto_die:concrete_instance_proto_die
          ~start_of_function ~end_of_function)
      ~accumulate:true
      ()
  in
  Profile.record "dwarf_for_variables_and_parameters" (fun () ->
      dwarf_for_variables_and_parameters t fundecl
        ~function_proto_die:concrete_instance_proto_die
        ~whole_function_lexical_block ~scope_proto_dies
        ~available_ranges_vars)
    ~accumulate:true
    ();
  if supports_call_sites () then begin
    let found_self_tail_calls =
      Profile.record "dwarf_for_call_sites" (fun () ->
          dwarf_for_call_sites t ~whole_function_lexical_block
            ~scope_proto_dies ~fundecl ~external_calls_generated_during_emit
            ~function_symbol:symbol)
        ~accumulate:true
        ()
    in
    if not found_self_tail_calls then begin
      Proto_die.add_or_replace_attribute_value concrete_instance_proto_die
        (DAH.create_call_all_calls ())
    end
  end

let dwarf_for_toplevel_constant t ~vars ~module_path ~symbol =
  (* Give each variable the same definition for the moment. *)
  List.iter (fun var ->
      let name =
        let path = Printtyp.string_of_path module_path in
        let name = Backend_var.name var in
        path ^ "." ^ name
      in
      let type_proto_die =
        normal_type_for_var t
          ~parent:(Some t.compilation_unit_proto_die)
          (Some (Compilation_unit.get_current_exn (), var))
      in
      let symbol = mangle_symbol symbol in
      Proto_die.create_ignore ~parent:(Some t.compilation_unit_proto_die)
        ~tag:Constant
        ~attribute_values:[
          DAH.create_name name;
          DAH.create_type ~proto_die:type_proto_die;
          DAH.create_const_value_from_symbol ~symbol;
          (* Mark everything as "external" so gdb puts the constants in its
             list of "global symbols". *)
          DAH.create_external ~is_visible_externally:true;
        ]
        ())
    vars

let dwarf_for_toplevel_constants t constants =
  List.iter (fun (constant : Clambda.preallocated_constant) ->
      match constant.provenance with
      | None -> ()
      | Some provenance ->
        (* Function declarations are emitted separately.  There's no more
           information that we require in a toplevel constant closure. *)
        match constant.definition with
        | Uconst_closure _ -> ()
        | _ ->
          let symbol =
            Symbol.of_global_linkage (Compilation_unit.get_current_exn ())
              (Linkage_name.create constant.symbol)
          in
          dwarf_for_toplevel_constant t ~vars:provenance.idents_for_types
            ~module_path:provenance.module_path
            ~symbol)
    constants

let dwarf_for_toplevel_inconstant t var ~module_path ~symbol =
  let name =
    let path = Printtyp.string_of_path module_path in
    let name = Backend_var.name var in
    path ^ "." ^ name
  in
  let type_proto_die =
    normal_type_for_var t
      ~parent:(Some t.compilation_unit_proto_die)
      (Some (Compilation_unit.get_current_exn (), var))
  in
  (* Toplevel inconstant "preallocated blocks" contain the thing of interest
     in field 0 (once it has been initialised).  We describe them using a
     single location description rather than a location list, since they
     should be accessible at all times independent of the current value of
     the PC. *)
  let single_location_description =
    Single_location_description.of_simple_location_description (
      (* We emit DWARF to describe an rvalue, rather than an lvalue, since
         we manually read these values ourselves in libmonda (whereas for
         e.g. a local variable bound to a read-symbol-field, the debugger
         will do a final dereference after determining the lvalue from the
         DWARF).  We cannot currently detect in libmonda whether or not a
         reference to a toplevel module component "M.foo" is a constant
         (represented as an rvalue in the DWARF, just the symbol's address)
         or an inconstant---so we must be consistent as far as l/rvalue-ness
         goes between the two. *)
      (* CR-soon mshinwell: Actually this isn't the case.  We could use
         SYMBOL_CLASS to distinguish them.  However maybe we'd better not
         in case this doesn't work well with non-gdb. *)
      let lang =
        SLDL.Rvalue.read_symbol_field ~symbol ~field:Targetint.zero
      in
      SLDL.compile (SLDL.of_rvalue lang))
  in
  Proto_die.create_ignore ~parent:(Some t.compilation_unit_proto_die)
    ~tag:Variable
    ~attribute_values:[
      DAH.create_name name;
      DAH.create_type ~proto_die:type_proto_die;
      DAH.create_single_location_description single_location_description;
      DAH.create_external ~is_visible_externally:true;  (* see above *)
    ]
    ()

let dwarf_for_toplevel_inconstants t inconstants =
  List.iter (fun (inconstant : Clambda.preallocated_block) ->
      (* CR-soon mshinwell: Should we be discarding toplevel things that don't
         have provenance?  Maybe not -- think. *)
      match inconstant.provenance with
      | None -> ()
      | Some provenance ->
        let symbol =
          Symbol.of_global_linkage (Compilation_unit.get_current_exn ())
            (Linkage_name.create inconstant.symbol)
        in
        let symbol = mangle_symbol symbol in
        (* CR-someday mshinwell: Support multi-field preallocated blocks
           (ignored for the moment as the only one is the module block, which
           isn't made visible in the debugger). *)
        match provenance.idents_for_types with
        | [] | _::_::_ -> ()
        | [ident] ->
          dwarf_for_toplevel_inconstant t ident
            ~module_path:provenance.module_path
            ~symbol)
    inconstants

let emit t =
  assert (not t.emitted);
  t.emitted <- true;
  Dwarf_world.emit ~compilation_unit_proto_die:t.compilation_unit_proto_die
    ~start_of_code_symbol:t.start_of_code_symbol
    ~end_of_code_symbol:t.end_of_code_symbol
    ~compilation_unit_header_label:t.compilation_unit_header_label
    ~address_table:t.address_table
    ~debug_loc_table:t.debug_loc_table
    ~debug_ranges_table:t.debug_ranges_table
    ~location_list_table:t.location_list_table
    ~range_list_table:t.range_list_table

let emit t = Profile.record "emit_dwarf" emit t
