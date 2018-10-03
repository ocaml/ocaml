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

module Available_subrange = Available_ranges.Available_subrange
module Available_range = Available_ranges.Available_range
module DAH = Dwarf_attribute_helpers
module SLDL = Simple_location_description_lang

(* DWARF-related state for a single compilation unit. *)
type t = {
  compilation_unit_header_label : Linearize.label;
  compilation_unit_proto_die : Proto_die.t;
  value_type_proto_die : Proto_die.t;
  debug_loc_table : Debug_loc_table.t;
  start_of_code_symbol : string;
  end_of_code_symbol : string;
  output_path : string;
  mutable emitted : bool;
}

type proto_dies_for_ident = {
  value_die_lvalue : Proto_die.reference;
  value_die_rvalue : Proto_die.reference;
  type_die : Proto_die.reference;
}

(* Note that on OS X 10.11 (El Capitan), dwarfdump doesn't seem to be able
   to read our 64-bit DWARF output. *)

let dwarf_version = ref Dwarf_version.four

let mangle_symbol symbol = Linkage_name.to_string (Symbol.label symbol)

let create ~prefix_name =
  begin match !Clflags.dwarf_format with
  | Thirty_two -> Dwarf_format.set Thirty_two
  | Sixty_four -> Dwarf_format.set Sixty_four
  end;
  begin match !Clflags.dwarf_version with
  | Four -> dwarf_version := Dwarf_version.four
  | Five -> dwarf_version := Dwarf_version.five
  end;
  let output_path, directory =
    let path = prefix_name ^ Config.ext_obj in
    if Filename.is_relative path then
      (* N.B. Relative---but may still contain directories,
         e.g. "foo/bar.ml". *)
      let dir = Sys.getcwd () in
      Filename.concat dir path,
        Filename.concat dir (Filename.dirname path)
    else
      path, Filename.dirname path
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
  let debug_line_label = Asm_directives.label_for_section (DWARF Debug_line) in
  let compilation_unit_proto_die =
    let attribute_values =
      let producer_name = Printf.sprintf "ocamlopt %s" Sys.ocaml_version in
      [ DAH.create_producer ~producer_name;
        DAH.create_name output_path;
        DAH.create_comp_dir ~directory;
        DAH.create_low_pc_from_symbol ~symbol:start_of_code_symbol;
        DAH.create_high_pc_from_symbol ~symbol:end_of_code_symbol;
        DAH.create_stmt_list ~debug_line_label;
      ]
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
  let debug_loc_table = Debug_loc_table.create () in
  { compilation_unit_proto_die;
    compilation_unit_header_label = Cmm.new_label ();
    value_type_proto_die;
    debug_loc_table;
    start_of_code_symbol;
    end_of_code_symbol;
    output_path;
    emitted = false;
  }

let proto_dies_for_variable var ~proto_dies_for_vars =
  match Backend_var.Tbl.find proto_dies_for_vars var with
  | exception Not_found ->
    Misc.fatal_errorf "Proto-DIE reference for %a not assigned"
      Backend_var.print var
  | result -> result

let normal_type_for_var _t ~parent var ~output_path =
  let var =
    match var with
    | `Var var -> var
    | `Unique_name name -> Backend_var.create_persistent name
  in
  let name =
    Name_laundry.base_type_die_name_for_var var ~output_path
  in
  Proto_die.create ~parent
    ~tag:Base_type
    ~attribute_values:[
      DAH.create_name name;
      DAH.create_encoding ~encoding:Encoding_attribute.signed;
      DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
    ]
    ()

let type_die_reference_for_var var ~proto_dies_for_vars =
  (proto_dies_for_variable var ~proto_dies_for_vars).type_die

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
let construct_type_of_value_description t ~parent var ~output_path
      ~(type_info : Available_ranges.type_info)
      ~proto_dies_for_vars ~reference =
  (* CR-soon mshinwell: share code with [normal_type_for_var], above *)
  let var =
    match var with
    | `Var var -> var
    | `Unique_name name -> Backend_var.create_persistent name
  in
  let name =
    Name_laundry.base_type_die_name_for_var var ~output_path
  in
  let normal_case () =
    (* CR-soon mshinwell: add [reference] to [Proto_die.create_ignore] *)
    let _proto_die : Proto_die.t =
      Proto_die.create ~reference
        ~parent
        ~tag:Base_type
        ~attribute_values:[
          DAH.create_name name;
          DAH.create_encoding ~encoding:Encoding_attribute.signed;
          DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        ]
        ()
    in
    ()
  in
  match type_info with
  | From_cmt_file _ -> normal_case ()
  | Phantom (_provenance, defining_expr) ->
    match defining_expr with
    | None -> normal_case ()
    | Some (Iphantom_const_int _)
    | Some (Iphantom_const_symbol _)
    | Some (Iphantom_read_symbol_field _) -> normal_case ()
    | Some (Iphantom_var var) ->
      let target_type =
        type_die_reference_for_var var ~proto_dies_for_vars
      in
      let _proto_die : Proto_die.t =
        Proto_die.create ~reference
          ~parent
          ~tag:Typedef
          ~attribute_values:[
            DAH.create_name name;
            DAH.create_type_from_reference ~proto_die_reference:target_type;
          ]
          ()
      in
      ()
    | Some (Iphantom_read_field { var = _; field = _; }) ->
      (* We cannot dereference an implicit pointer when evaluating a DWARF
         location expression, which means we must restrict ourselves to 
         projections from non-phantom identifiers.  This is ensured at the
         moment by the commented-out code in [Flambda_utils] and the fact that
         [Flambda_to_clambda] only generates [Uphantom_read_var_field] on
         the (non-phantom) environment parameter. *)
      normal_case ()
    | Some (Iphantom_offset_var { var = _; offset_in_words = _; }) ->
      (* The same applies here as for [Iphantom_read_var_field] above, but we
         never generate this for phantom identifiers at present (it is only
         used for offsetting a closure environment variable). *)
      normal_case ()
    | Some (Iphantom_block { tag = _; fields; }) ->
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
              let type_die_reference =
                type_die_reference_for_var var ~proto_dies_for_vars
              in
              DAH.create_type_from_reference
                ~proto_die_reference:type_die_reference;
          in
          let field_proto_die =
            Proto_die.create ~parent:(Some struct_type_die)
              ~tag:Member
              ~attribute_values:(type_attribute :: [
                DAH.create_name name;
                DAH.create_bit_size (Int64.of_int (Arch.size_addr * 8));
                DAH.create_data_member_location
                  ~byte_offset:(Int64.of_int (index * Arch.size_addr));
              ])
              ()
          in
          Proto_die.set_sort_priority field_proto_die index)
        (None :: fields);  (* "None" is for the GC header. *)
      let _pointer_to_struct_type_die : Proto_die.t =
        Proto_die.create ~reference ~parent
          ~tag:Pointer_type
          ~attribute_values:[
            DAH.create_name name;
            DAH.create_type ~proto_die:struct_type_die;
          ]
          ()
      in
      ()

let location_of_variable_lvalue t var ~proto_dies_for_vars =
  (* We may need to reference the locations of other values in order to
     describe the location of some particular value.  This is done by using
     the "call" functionality of DWARF location descriptions.
     (DWARF-4 specification section 2.5.1.5, page 24.)  This avoids any need
     to transitively resolve phantom lets (to constants, symbols or
     non-phantom variables) in the compiler. *)
  match proto_dies_for_variable var ~proto_dies_for_vars with
  | { value_die_lvalue; _; } ->
    SLDL.Lvalue.location_from_another_die
      ~die_label:value_die_lvalue
      ~compilation_unit_header_label:t.compilation_unit_header_label

let location_of_variable_rvalue t var ~proto_dies_for_vars =
  match proto_dies_for_variable var ~proto_dies_for_vars with
  | { value_die_rvalue; _; } ->
    SLDL.Rvalue.location_from_another_die
      ~die_label:value_die_rvalue
      ~compilation_unit_header_label:t.compilation_unit_header_label

type description =
  | Simple of Simple_location_description.t
  | Composite of Composite_location_description.t

let construct_value_description t ~parent ~fundecl
      ~(type_info : Available_ranges.type_info) ~available_subrange
      ~proto_dies_for_vars ~need_rvalue
      : Location_list_entry.t option =
  let location_description
        ~(location : unit Available_subrange.location) : description option =
    let module SLD = Simple_location_description in
    match location with
    | Reg (reg, ()) ->
      begin match reg.Reg.loc with
      | Reg.Unknown -> assert false  (* probably a bug in available_regs.ml *)
      | Reg.Reg n ->
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
              Simple (SLDL.compile (SLDL.of_lvalue (
                SLDL.Lvalue.in_register ~dwarf_reg_number)))
            else
              Simple (SLDL.compile (SLDL.of_rvalue (
                SLDL.Rvalue.in_register ~dwarf_reg_number)))
          in
          Some location_description
        end
      | Reg.Stack _ ->
        match
          Available_subrange.offset_from_stack_ptr_in_bytes available_subrange
        with
        | None ->  (* emit.mlp should have set the offset *)
          Misc.fatal_errorf "Register %a assigned to stack but without \
              stack offset annotation"
            Printmach.reg reg
        | Some offset_in_bytes_from_cfa ->
          if offset_in_bytes_from_cfa mod Arch.size_addr <> 0 then begin
            Misc.fatal_errorf "Dwarf.location_list_entry: misaligned stack \
                slot at offset %d (reg %a)"
              offset_in_bytes_from_cfa
              Printmach.reg reg
          end;
          (* CR-soon mshinwell: use [offset_in_bytes] instead *)
          let offset_in_words =
            Targetint.of_int_exn (offset_in_bytes_from_cfa / Arch.size_addr)
          in
          if not need_rvalue then
            Some (Simple (SLDL.compile (SLDL.of_lvalue (
              SLDL.Lvalue.in_stack_slot ~offset_in_words))))
          else
            Some (Simple (SLDL.compile (SLDL.of_rvalue (
              SLDL.Rvalue.in_stack_slot ~offset_in_words))))
      end
    | Phantom ->
      assert (not need_rvalue);  (* See comments below. *)
      let defining_expr =
        match type_info with
        | From_cmt_file _ -> assert false
        | Phantom (_provenance, defining_expr) ->
          (* CR mshinwell: don't ignore provenance
             Follow-up: Is it really needed?  For example, Inlining_transforms
             is using dummy module paths for function parameters, etc.
          *)
          match defining_expr with
          | Some defining_expr -> defining_expr
          | None ->
            (* There should never be any available subranges for a phantom
               identifier without a definition (see the bottom of
               available_ranges.ml for an explanation as to why these occur). *)
            assert false
      in
      let lvalue lvalue =
        Some (Simple (SLDL.compile (SLDL.of_lvalue lvalue)))
      in
      let rvalue rvalue =
        Some (Simple (SLDL.compile (SLDL.of_rvalue rvalue)))
      in
      let arch_size_addr = Targetint.of_int_exn Arch.size_addr in
      match defining_expr with
      | Iphantom_const_int i -> rvalue (SLDL.Rvalue.signed_int_const i)
      | Iphantom_const_symbol symbol ->
        lvalue (SLDL.Lvalue.const_symbol ~symbol)
      | Iphantom_read_symbol_field { sym; field; } ->
        (* CR mshinwell: Fix [field] to be of type [Targetint.t] *)
        let field = Targetint.of_int field in
        rvalue (SLDL.Rvalue.read_symbol_field ~symbol:sym ~field)
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
        let location =
          SLDL.compile (SLDL.of_rvalue (
            location_of_variable_rvalue t var ~proto_dies_for_vars))
        in
        let composite =
          Composite_location_description.pieces_of_simple_location_descriptions
            [location, arch_size_addr]
        in
        Some (Composite composite)
      | Iphantom_read_field { var; field; } ->
        (* CR mshinwell: Clarify the following (maybe in SLDL directly):
           "lvalue" == "no DW_op_stack_value" *)
        (* Reminder: see CR in flambda_utils.ml regarding these constructions *)
        let block =
          (* We need to use a location expression that when evaluated will
             yield the runtime value of [var] *actually on the DWARF stack*.
             This is necessary so that we can apply other stack operations,
             such as arithmetic and dereferencing, to the value.  The upshot is
             that we cannot use the "standalone" location-describing operators
             such as DW_op_regx; instead we would use DW_op_bregx.  (Another
             way of looking at this is that we need to get the DWARF operator
             sequence without [DW_op_stack_value], either implicitly or
             explicitly, at the end.) *)
          (* CR-someday mshinwell: When we fix the CR in flambda_utils.ml and
             generate [Iphantom_read_var_field] and [Iphantom_offset_var] for
             more than just closure parameters, we should think carefully as
             to whether the existing approach is reasonable (having separate
             DIEs for the rvalues and lvalues).  Maybe one of those DIEs should
             call the other. *)
          location_of_variable_rvalue t var ~proto_dies_for_vars
        in
        let field = Targetint.of_int_exn field in
        (* CR mshinwell: Ditch [SLDL.compile]. *)
        let read_field =
          SLDL.compile (SLDL.of_rvalue (SLDL.Rvalue.read_field ~block ~field))
        in
        let composite =
          Composite_location_description.pieces_of_simple_location_descriptions
            [read_field, arch_size_addr]
        in
        Some (Composite composite)
      | Iphantom_offset_var { var; offset_in_words; } ->
        let location =
          location_of_variable_lvalue t var ~proto_dies_for_vars
        in
        let offset_in_words = Targetint.of_int_exn offset_in_words in
        let offset_var =
          SLDL.compile (SLDL.of_rvalue (
            SLDL.Rvalue.offset_pointer location ~offset_in_words))
        in
        let composite =
          Composite_location_description.pieces_of_simple_location_descriptions
            [offset_var, arch_size_addr]
        in
        Some (Composite composite)
      | Iphantom_block { tag; fields; } ->
        (* A phantom block construction: instead of the block existing in the
           target program's address space, it is going to be conjured up in the
           *debugger's* address space using instructions described in DWARF.
           References between such blocks do not use normal pointers in the
           target's address space---instead they use "implicit pointers"
           (requires GNU DWARF extensions prior to DWARF-5). *)
        (* CR mshinwell: use a cache to dedup the CLDs *)
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
                  SLDL.compile (SLDL.of_rvalue (
                    location_of_variable_rvalue t ident ~proto_dies_for_vars))
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
  in
  let single_location_description =
    match
      location_description
        ~location:(Available_subrange.location available_subrange)
    with
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
    let start_of_code_symbol = fundecl.Linearize.fun_name in
    let first_address_when_in_scope =
      Available_subrange.start_pos available_subrange
    in
    let first_address_when_not_in_scope =
      Available_subrange.end_pos available_subrange
    in
    let first_address_when_not_in_scope_offset =
      Available_subrange.end_pos_offset available_subrange
    in
    let location_list_entry =
      Location_list_entry.create_location_list_entry
        ~start_of_code_symbol
        ~first_address_when_in_scope
        ~first_address_when_not_in_scope
        ~first_address_when_not_in_scope_offset
        ~single_location_description
    in
    Some location_list_entry

let dwarf_for_variable t ~fundecl ~function_proto_die
      ~lexical_block_proto_die ~proto_dies_for_vars
      ~need_rvalue
      (var : Backend_var.t) ~(ident_for_type : Ident.t option) ~is_unique
      ~range =
  let type_info = Available_range.type_info range in
  let is_parameter = Available_range.is_parameter range in
  let parent_proto_die : Proto_die.t =
    match is_parameter with
    | Parameter _index ->
      (* Parameters need to be children of the function in question. *)
      function_proto_die
    | Local ->
      (* Local variables need to be children of "lexical blocks", which in turn
         are children of the function.  We create a single lexical block per
         function to avoid the debugger getting confused. *)
      lexical_block_proto_die
  in
  (* Build a location list that identifies where the value of [ident] may be
     found at runtime, indexed by program counter range, and insert the list
     into the .debug_loc table. *)
  let location_list_attribute_value =
    (* DWARF-4 spec 2.6.2: "In the case of a compilation unit where all of the
       machine code is contained in a single contiguous section, no base
       address selection entry is needed."
       However, we tried this (and emitted plain label addresses rather than
       deltas in [Location_list_entry]), and the addresses were wrong in the
       final executable.  Oh well. *)
    let base_address_selection_entry =
      let fun_symbol = fundecl.Linearize.fun_name in
      Location_list_entry.create_base_address_selection_entry
        ~base_address_symbol:fun_symbol
    in
    let location_list_entries =
      Available_range.fold range
        ~init:[]
        ~f:(fun location_list_entries ~available_subrange ->
          let location_list_entry =
            construct_value_description t ~parent:(Some function_proto_die)
              ~fundecl ~available_subrange ~proto_dies_for_vars
              ~type_info ~need_rvalue
          in
          match location_list_entry with
          | None -> location_list_entries
          | Some location_list_entry ->
            location_list_entry::location_list_entries)
    in
    let location_list_entries =
      base_address_selection_entry :: location_list_entries
    in
    let location_list = Location_list.create ~location_list_entries in
    Debug_loc_table.insert t.debug_loc_table ~location_list
  in
  let type_and_name_attributes =
    let reference = type_die_reference_for_var var ~proto_dies_for_vars in
    let var, name_for_var =
      match ident_for_type with
      | None ->
        let name = "Foo.bar" in  (* CR mshinwell: fix Name_laundry *)
        `Var (Backend_var.create_persistent name), None
      | Some ident_for_type ->
        let var = `Var ident_for_type in
        match is_parameter with
        | Local ->
          (* If the unstamped name of [ident] is unambiguous within the
             function, then use it; otherwise, equip the name with the location
             of its definition together with any inlined-out frames. *)
          if is_unique then
            var, Some (Ident.name ident_for_type)
          else
            let provenance =
              match type_info with
              | From_cmt_file provenance
              | Phantom (provenance, _) -> provenance
            in
            (* CR-soon mshinwell: Try to remove this option *)
            begin match provenance with
            | None -> var, Some (Ident.name ident_for_type)
            | Some provenance ->
              let location =
                Format.asprintf "%a"
                  Debuginfo.print_compact
                  (Backend_var.Provenance.location provenance)
              in
              let name =
                Format.sprintf "%s[%s]"
                  (Ident.name ident_for_type)
                  location
              in
              var, Some name
            end
        | Parameter _ ->
          (* CR-someday mshinwell: This is actually only true for parameters
             with names in the source code... *)
          (* Parameters for a given function have unique names, so are never
             equipped with locations.  (A parameter may have the same name as
             a local, but in that case, the local will be equipped with its
             location.) *)
          var, Some (Ident.name ident_for_type)
    in
    (* CR-someday mshinwell: This should be tidied up.  It's only correct by
       virtue of the fact we do the closure-env ones second below. *)
    if not need_rvalue then begin
      construct_type_of_value_description t
        ~parent:(Some t.compilation_unit_proto_die)
        var ~output_path:t.output_path
        ~type_info ~proto_dies_for_vars
        ~reference
    end;
    let name_for_var =
      match name_for_var with
      | None -> []
      | Some name -> [DAH.create_name name]
    in
    name_for_var @ [
      DAH.create_type_from_reference ~proto_die_reference:reference;
    ]
  in
  let tag : Dwarf_tag.t =
    match is_parameter with
    | Parameter _index -> Formal_parameter
    | Local -> Variable
  in
  let reference =
    let proto_dies = proto_dies_for_variable var ~proto_dies_for_vars in
    if need_rvalue then proto_dies.value_die_rvalue
    else proto_dies.value_die_lvalue
  in
  let proto_die =
    Proto_die.create ~reference
      ~parent:(Some parent_proto_die)
      ~tag
      ~attribute_values:(type_and_name_attributes @ [
        location_list_attribute_value;
      ])
      ()
  in
  begin match is_parameter with
  | Local -> ()
  | Parameter { index; } ->
    (* Ensure that parameters appear in the correct order in the debugger. *)
    Proto_die.set_sort_priority proto_die index
  end

(* This function covers local variables, parameters, variables in closures
   and other "fun_var"s in the current mutually-recursive set.  (The last
   two cases are handled by the explicit addition of phantom lets way back
   in [Flambda_to_clambda].)  Phantom identifiers are also covered. *)
let iterate_over_variable_like_things _t ~available_ranges ~f =
  Available_ranges.fold available_ranges
    ~init:()
    ~f:(fun () ~var ~is_unique ~range ->
      (* There are two variables in play here:
         1. [var] is the "real" variable that is used to cross-reference
             between DIEs;
         2. [ident_for_type], if it is [Some], is the corresponding
            identifier with the stamp as in the typed tree.  This is the
            one used for lookup in .cmt files.
         We cannot conflate these since the multiple [vars] that might
         be associated with a given [ident_for_type] (due to inlining) may
         not all have the same value. *)
      (* CR-soon mshinwell: Default arguments currently appear as local
         variables, not parameters. *)
      (* CR mshinwell: Introduce some flag on Backend_var.t to mark identifiers
         that were generated internally (or vice-versa)? *)
      let ident_for_type =
        if Backend_var.name var = "*closure_env*"
          || Backend_var.name var = "*opt*"
          || Backend_var.name var = "*match*"
        then begin
          None
        end else begin
          let provenance =
            match Available_range.type_info range with
            | From_cmt_file provenance
            | Phantom (provenance, _) -> provenance
          in
          match provenance with
          | None ->
            (* In this case the variable won't be given a name in the DWARF,
               so as not to appear in the debugger; but we still need to emit
               a DIE for it, as it may be referenced as part of some chain of
               phantom lets. *)
            None
          | Some provenance ->
            let location = Backend_var.Provenance.location provenance in
            if location = Debuginfo.none
              && match Available_range.is_parameter range with
                 | Local -> true
                 | _ -> false
            then None
            else
              let original_ident =
                Backend_var.Provenance.original_ident provenance
              in
              Some original_ident
        end
      in
      f var ~ident_for_type ~is_unique ~range)

let dwarf_for_variables_and_parameters t ~function_proto_die
      ~lexical_block_proto_die ~available_ranges
      ~(fundecl : Linearize.fundecl) =
  let proto_dies_for_vars = Backend_var.Tbl.create 42 in
  iterate_over_variable_like_things t ~available_ranges
    ~f:(fun var ~ident_for_type:_ ~is_unique:_ ~range:_ ->
      let value_die_lvalue = Proto_die.create_reference () in
      let value_die_rvalue = Proto_die.create_reference () in
      let type_die = Proto_die.create_reference () in
      assert (not (Backend_var.Tbl.mem proto_dies_for_vars var));
      Backend_var.Tbl.add proto_dies_for_vars var
        { value_die_lvalue; value_die_rvalue; type_die; });
  (* CR-someday mshinwell: Consider changing [need_rvalue] to use a variant
     type "lvalue or rvalue". *)
  iterate_over_variable_like_things t ~available_ranges
    ~f:(dwarf_for_variable t ~fundecl ~function_proto_die
      ~lexical_block_proto_die ~proto_dies_for_vars
      ~need_rvalue:false);
  iterate_over_variable_like_things t ~available_ranges
    ~f:(fun var ~ident_for_type ~is_unique ~range ->
      (* We only need DIEs that yield actually on the DWARF stack the locations
         of entities for closure arguments, since those are the only ones
         that may be involved with [Iphantom_read_var_field] and
         [Iphantom_var] constructions.  (See comments above.) *)
      (* CR-someday mshinwell: Make this test less messy *)
      if Backend_var.name var = "*closure_env*" then begin
        dwarf_for_variable t ~fundecl ~function_proto_die
          ~lexical_block_proto_die ~proto_dies_for_vars
          ~need_rvalue:true
          var ~ident_for_type ~is_unique ~range
      end)

let dwarf_for_function_definition t ~(fundecl : Linearize.fundecl)
      ~available_ranges ~end_of_function_label =
  let symbol = fundecl.fun_name in
  let start_of_function =
    DAH.create_low_pc_from_symbol ~symbol
  in
  let end_of_function =
    DAH.create_high_pc ~address_label:end_of_function_label
  in
  let function_name =
    match fundecl.fun_module_path with
    | None ->
      begin match fundecl.fun_human_name with
      | "" -> "anon"
      | name -> name
      end
    | Some path ->
      let path = Printtyp.string_of_path path in
      (* CR-soon mshinwell: remove hack *)
      match path with
      | "_Ocaml_startup" ->
        begin match fundecl.fun_human_name with
        | "" -> "anon"
        | name -> name
        end
      | _ ->
        match fundecl.fun_human_name with
        | "" -> path
        | name -> path ^ "." ^ name
  in
  let is_visible_externally =
    (* Not strictly accurate---should probably depend on the .mli, but
       this should suffice for now. *)
    fundecl.fun_module_path <> None
  in
  let type_proto_die =
    normal_type_for_var t
      ~parent:(Some t.compilation_unit_proto_die)
      (`Unique_name fundecl.fun_name)
      ~output_path:t.output_path
  in
  let function_proto_die =
    Proto_die.create ~parent:(Some t.compilation_unit_proto_die)
      ~tag:Subprogram
      ~attribute_values:[
        DAH.create_name function_name;
        DAH.create_external ~is_visible_externally;
        start_of_function;
        end_of_function;
        DAH.create_type ~proto_die:type_proto_die
      ]
      ()
  in
  let lexical_block_proto_die =
    (* CR-someday mshinwell: Consider trying to improve this so that we don't
       have all locals visible at once. *)
    Proto_die.create ~parent:(Some function_proto_die)
      ~tag:Lexical_block
      ~attribute_values:[
        start_of_function;
        end_of_function;
      ]
      ()
  in
  dwarf_for_variables_and_parameters t ~function_proto_die
    ~lexical_block_proto_die ~available_ranges ~fundecl

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
          (`Var var)
          ~output_path:t.output_path
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
        ])
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
          dwarf_for_toplevel_constant t ~vars:provenance.original_idents
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
      (`Var var)
      ~output_path:t.output_path
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

let dwarf_for_toplevel_inconstants t inconstants =
  List.iter (fun (inconstant : Clambda.preallocated_block) ->
      (* CR mshinwell: Should we be discarding toplevel things that don't have
         provenance?  Maybe not -- think. *)
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
        match provenance.original_idents with
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
    ~debug_loc_table:t.debug_loc_table
