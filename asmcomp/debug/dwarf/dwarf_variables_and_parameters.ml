(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module ARV = Available_ranges_all_vars
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state
module L = Linearize
module SLDL = Simple_location_description_lang
module V = Backend_var

type var_uniqueness = {
  name_is_unique : bool;
  position_is_unique_given_name_is_unique : bool;
}

module Var_name_and_code_range = struct
  type t = string * (Debuginfo.Code_range.t option)

  include Identifiable.Make (struct
    type nonrec t = t

    let compare (name1, range_opt1) (name2, range_opt2) =
      let c = String.compare name1 name2 in
      if c <> 0 then c
      else
        match range_opt1, range_opt2 with
        | None, None -> 0
        | None, Some _ -> -1
        | Some _, None -> 1
        | Some range1, Some range2 ->
          Debuginfo.Code_range.compare range1 range2

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash (name, range_opt) =
      match range_opt with
      | None -> Hashtbl.hash (name, None)
      | Some range ->
        Hashtbl.hash (name, Some (Debuginfo.Code_range.hash range))

    let print _ _ = Misc.fatal_error "Not yet implemented"
    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)
end

type is_variable_phantom = Non_phantom | Phantom

type proto_dies_for_var = {
  is_variable_phantom : is_variable_phantom;
  value_die_lvalue : Proto_die.reference;
  value_die_rvalue : Proto_die.reference;
  type_die : Proto_die.reference;
}

let arch_size_addr = Targetint.of_int_exn Arch.size_addr

(* Note: this function only works for static (toplevel) variables because we
   assume they all occur in a single compilation unit (namely the startup
   one). *)
let calculate_var_uniqueness ~available_ranges_vars =
  let module String = Misc.Stdlib.String in
  let by_name = String.Tbl.create 42 in
  let by_position = Var_name_and_code_range.Tbl.create 42 in
  let update_uniqueness var pos ~module_path =
    let name = Backend_var.name_for_debugger var in
    let name =
      match module_path with
      | None -> name
      | Some module_path -> Format.asprintf "%a.%s" Path.print module_path name
    in
    begin match String.Tbl.find by_name name with
    | exception Not_found ->
      String.Tbl.add by_name name (Backend_var.Set.singleton var)
    | vars ->
      String.Tbl.replace by_name name (Backend_var.Set.add var vars)
    end;
    begin match Var_name_and_code_range.Tbl.find by_position (name, pos) with
    | exception Not_found ->
      Var_name_and_code_range.Tbl.add by_position (name, pos)
        (Backend_var.Set.singleton var)
    | vars ->
      Var_name_and_code_range.Tbl.replace by_position (name, pos)
        (Backend_var.Set.add var vars)
    end
  in
  let result = Backend_var.Tbl.create 42 in
  ARV.iter available_ranges_vars
    ~f:(fun var range ->
      let range_info = ARV.Range.info range in
      let provenance = ARV.Range_info.provenance range_info in
      let module_path =
        (* The startup function may contain phantom let bindings for
           static identifiers that have various different (sub)module paths. *)
        match provenance with
        | None -> None
        | Some provenance ->
          if not (V.Provenance.is_static provenance) then None
          else Some (V.Provenance.module_path provenance)
      in
      let dbg = ARV.Range_info.debuginfo range_info in
      let pos = Debuginfo.position dbg in
      update_uniqueness var pos ~module_path;
      Backend_var.Tbl.replace result var
        { name_is_unique = false;
          position_is_unique_given_name_is_unique = false;
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
  (* Note that [by_position] isn't of type [_ Debuginfo.Code_range.Map.t].
     The reason that the [Backend_var.t] is included in the key is because we
     need to make a judgement as to whether two variables have the same
     position _given also that they have the same name_.  (See the
     computations in [dwarf_for_variable], below.) *)
  Var_name_and_code_range.Tbl.iter (fun _var_and_pos vars ->
      match Backend_var.Set.get_singleton vars with
      | None -> ()
      | Some var ->
        let var_uniqueness = Backend_var.Tbl.find result var in
        Backend_var.Tbl.replace result var
          { var_uniqueness with
            position_is_unique_given_name_is_unique = true;
          })
    by_position;
  result

let proto_dies_for_variable var ~proto_dies_for_vars =
  match Backend_var.Tbl.find proto_dies_for_vars var with
  | exception Not_found -> None
  | result -> Some result

let normal_type_for_var ?reference ~parent ident_for_type is_parameter =
  let name_attribute =
    match ident_for_type with
    | None -> []
    | Some (compilation_unit, var) ->
      let name =
        Dwarf_name_laundry.base_type_die_name_for_var compilation_unit var
          is_parameter
      in
      [DAH.create_name name]
  in
  (* CR mshinwell: This should not create duplicates when the name is
     missing *)
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
(* CR mshinwell: Add proper type for [ident_for_type] *)
let construct_type_of_value_description state ~parent ident_for_type
      ~(phantom_defining_expr : ARV.Range_info.phantom_defining_expr)
      is_parameter ~proto_dies_for_vars ~reference =
  let normal_case () =
    let (_ : Proto_die.t) =
      normal_type_for_var ~reference ~parent ident_for_type is_parameter
    in
    ()
  in
  let name_attribute =
    match ident_for_type with
    | None -> []
    | Some (compilation_unit, var) ->
      let name =
        Dwarf_name_laundry.base_type_die_name_for_var compilation_unit var
          is_parameter
      in
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
          DAH.create_type ~proto_die:(DS.value_type_proto_die state)
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
            | None ->
              DAH.create_type ~proto_die:(DS.value_type_proto_die state)
            | Some var ->
              (* It's ok if [var] is a phantom identifier, since we will
                 be building a composite location expression to describe the
                 structure, and implicit pointers are permitted there.  (That
                 is to say, the problem for [Iphantom_read_var_field] above
                 does not exist here). *)
              match type_die_reference_for_var var ~proto_dies_for_vars with
              | None ->
                DAH.create_type ~proto_die:(DS.value_type_proto_die state)
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

let die_location_of_variable_lvalue state var ~proto_dies_for_vars =
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
        ~compilation_unit_header_label:(DS.compilation_unit_header_label state)
    in
    Some location

let die_location_of_variable_rvalue state var ~proto_dies_for_vars =
  match proto_dies_for_variable var ~proto_dies_for_vars with
  | None -> None
  | Some { value_die_rvalue; _; } ->
    DS.set_rvalue_dies_required_for state
      (V.Set.add var (DS.rvalue_dies_required_for state));
    let location =
      SLDL.Rvalue.location_from_another_die
        ~die_label:value_die_rvalue
        ~compilation_unit_header_label:(DS.compilation_unit_header_label state)
    in
    Some location

type location_description =
  | Simple of Simple_location_description.t
  | Composite of Composite_location_description.t

let reg_location_description reg ~offset_from_cfa_in_bytes
      ~need_rvalue : location_description option =
  match
    Dwarf_reg_locations.reg_location_description reg
      ~offset_from_cfa_in_bytes ~need_rvalue
  with
  | None -> None
  | Some simple_loc_desc -> Some (Simple simple_loc_desc)

(* Phantom-let-bound variables are always immutable, so we don't actually
   need lvalue (in the sense of the [SLDL] module) descriptions for them.
   (For example, we'll never need to watchpoint them.)  However, to be
   consistent with normal variables, we always emit lvalue descriptions (and
   emit rvalue descriptions only when required). *)
let phantom_var_location_description state
      ~(defining_expr : Mach.phantom_defining_expr) ~need_rvalue
      ~proto_dies_for_vars ~parent
      : location_description option =
  let module SLD = Simple_location_description in
  let lvalue lvalue = Some (Simple (SLDL.compile (SLDL.of_lvalue lvalue))) in
  let lvalue_without_address lvalue =
    Some (Simple (SLDL.compile (SLDL.of_lvalue_without_address lvalue)))
  in
  let rvalue rvalue = Some (Simple (SLDL.compile (SLDL.of_rvalue rvalue))) in
  match defining_expr with
  | Iphantom_const_int i ->
    let i = SLDL.Rvalue.signed_int_const i in
    if need_rvalue then rvalue i
    else lvalue_without_address (SLDL.Lvalue_without_address.of_rvalue i)
  | Iphantom_const_symbol symbol ->
    let symbol = SLDL.Rvalue.const_symbol (Asm_symbol.create symbol) in
    if need_rvalue then rvalue symbol
    else lvalue_without_address (SLDL.Lvalue_without_address.of_rvalue symbol)
  | Iphantom_read_symbol_field { sym; field; } ->
    let symbol = Asm_symbol.create sym in
    (* CR-soon mshinwell: Fix [field] to be of type [Targetint.t] *)
    let field = Targetint.of_int field in
    if need_rvalue then rvalue (SLDL.Rvalue.read_symbol_field symbol ~field)
    else lvalue (SLDL.Lvalue.in_symbol_field symbol ~field)
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
       is called.
       Update 2019-04-11: Reading closure variables that are unavailable
       hits this.  You get "cannot read memory at 0x0" or similar in gdb.
       This happens because the variables are phantom, so there may be
       ranges for them across points when they're unavailable. *)
    if need_rvalue then
      begin match
        die_location_of_variable_rvalue state var ~proto_dies_for_vars
      with
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
    else
      begin match
        die_location_of_variable_lvalue state var ~proto_dies_for_vars
      with
      | None -> None
      | Some lvalue ->
        let location = SLDL.compile (SLDL.of_lvalue lvalue) in
        let composite =
          Composite_location_description.
            pieces_of_simple_location_descriptions
              [location, arch_size_addr]
        in
        Some (Composite composite)
      end
  | Iphantom_read_field { var; field; } ->
    begin match is_variable_phantom var ~proto_dies_for_vars with
    | None | Some Phantom ->
      (* For the moment, show this field access as unavailable, since we
         cannot "DW_OP_deref" a value built up with implicit pointers. *)
      None
    | Some Non_phantom ->
      match die_location_of_variable_rvalue state var ~proto_dies_for_vars with
      | None -> None
      | Some block ->
        let field = Targetint.of_int_exn field in
        if need_rvalue then
          let read_field =
            SLDL.compile (SLDL.of_rvalue (SLDL.Rvalue.read_field ~block ~field))
          in
          let composite =
            Composite_location_description.
              pieces_of_simple_location_descriptions
                [read_field, arch_size_addr]
          in
          Some (Composite composite)
        else
          lvalue (SLDL.Lvalue.read_field ~block ~field)
    end
  | Iphantom_offset_var { var; offset_in_words; } ->
    begin match
      die_location_of_variable_lvalue state var ~proto_dies_for_vars
    with
    | None -> None
    | Some location ->
      let offset_in_words = Targetint.of_int_exn offset_in_words in
      if need_rvalue then None
      else lvalue (SLDL.Lvalue.offset_pointer location ~offset_in_words)
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
                die_location_of_variable_rvalue state ident
                  ~proto_dies_for_vars
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
    let offset_in_bytes = Targetint.zero in
    let die_label = Proto_die.reference proto_die in
    let version = DS.dwarf_version state in
    if need_rvalue then
      rvalue (SLDL.Rvalue.implicit_pointer ~offset_in_bytes ~die_label version)
    else
      lvalue_without_address (SLDL.Lvalue_without_address.implicit_pointer
        ~offset_in_bytes ~die_label version)

let single_location_description state (_fundecl : L.fundecl) ~parent ~subrange
      ~proto_dies_for_vars ~need_rvalue =
  let location_description =
    match ARV.Subrange.info subrange with
    | Non_phantom { reg; offset_from_cfa_in_bytes; } ->
      reg_location_description reg ~offset_from_cfa_in_bytes ~need_rvalue
    | Phantom defining_expr ->
      phantom_var_location_description state ~defining_expr
        ~need_rvalue ~proto_dies_for_vars ~parent
  in
  match location_description with
  | None -> None
  | Some (Simple simple) ->
    Some (Single_location_description.of_simple_location_description simple)
  | Some (Composite composite) ->
    Some (Single_location_description.of_composite_location_description
      composite)

let single_phantom_location_description state ~parent ~proto_dies_for_vars
      ~need_rvalue defining_expr =
  let location_description =
    phantom_var_location_description state ~defining_expr
      ~need_rvalue ~proto_dies_for_vars ~parent
  in
  match location_description with
  | None -> None
  | Some (Simple simple) ->
    Some (Single_location_description.of_simple_location_description simple)
  | Some (Composite composite) ->
    Some (Single_location_description.of_composite_location_description
      composite)

type location_list_entry =
  | Dwarf_4 of Dwarf_4_location_list_entry.t
  | Dwarf_5 of Location_list_entry.t

let location_list_entry state ~subrange single_location_description
      : location_list_entry =
  let start_pos =
    Asm_label.create_int Text (ARV.Subrange.start_pos subrange)
  in
  let start_pos_offset = ARV.Subrange.start_pos_offset subrange in
  let end_pos =
    Asm_label.create_int Text (ARV.Subrange.end_pos subrange)
  in
  let end_pos_offset = ARV.Subrange.end_pos_offset subrange in
  match !Clflags.gdwarf_version with
  | Four ->
    let location_list_entry =
      Dwarf_4_location_list_entry.create_location_list_entry
        ~start_of_code_symbol:(DS.start_of_code_symbol state)
        ~first_address_when_in_scope:start_pos
        ~first_address_when_in_scope_offset:(Some start_pos_offset)
        ~first_address_when_not_in_scope:end_pos
        ~first_address_when_not_in_scope_offset:(Some end_pos_offset)
        ~single_location_description
    in
    Dwarf_4 location_list_entry
  | Five ->
    let start_inclusive =
      Address_table.add (DS.address_table state) start_pos
        ~adjustment:start_pos_offset
        ~start_of_code_symbol:(DS.start_of_code_symbol state)
    in
    let end_exclusive =
      Address_table.add (DS.address_table state) end_pos
        ~adjustment:end_pos_offset
        ~start_of_code_symbol:(DS.start_of_code_symbol state)
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
    Dwarf_5 (Location_list_entry.create location_list_entry
      ~start_of_code_symbol:(DS.start_of_code_symbol state))

let dwarf_for_variable state (fundecl : L.fundecl) ~function_proto_die
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
  let is_static =
    match provenance with
    | None -> false
    | Some provenance -> V.Provenance.is_static provenance
  in
  let phantom_defining_expr = ARV.Range_info.phantom_defining_expr range_info in
  let (parent_proto_die : Proto_die.t), hidden =
    if var_is_a_parameter_of_fundecl_itself then
      function_proto_die, hidden
    else if is_static then
      begin match provenance with
      | None -> DS.compilation_unit_proto_die state, hidden
      | Some provenance ->
        let module_path = V.Provenance.module_path provenance in
        Dwarf_modules.dwarf state ~module_path, hidden
      end
    else
      (* Local variables need to be children of "lexical blocks", which in turn
         are children of the function.  It is important to generate accurate
         lexical block information to avoid large numbers of variables, many
         of which may be out of scope, being visible in the debugger at the
         same time. *)
      match provenance with
      | None ->
        (* Any variable without provenance gets hidden. *)
        function_proto_die, true
      | Some provenance ->
        let dbg = Backend_var.Provenance.debuginfo provenance in
        let block_die =
          Dwarf_lexical_blocks_and_inlined_frames.find_scope_die_from_debuginfo
            dbg ~function_proto_die ~scope_proto_dies
        in
        match block_die with
        | Some block_die -> block_die, hidden
        | None ->
          (* There are be no instructions marked with the block in which
             [var] was defined.  For the moment, just hide [var]. *)
          function_proto_die, true
  in
  let location_attribute_value, location_list_in_debug_loc_table =
    if is_static then begin
      (* The location of a static (toplevel) variable is invariant under changes
         to the program counter. As such a location list is not needed. *)
      (* CR-someday mshinwell: In the future we should work out how to make
         static variables be properly scoped with respect to the
         interleaving function definitions.  Then, we would use the actual
         calculated subranges here.  This work requires not only changes in
         the OCaml middle end but also a determination as to whether GDB can
         support the necessary scoping. *)
      match phantom_defining_expr with
      | Non_phantom -> [], None  (* Should have been caught below. *)
      | Phantom defining_expr ->
        if not (Mach.phantom_defining_expr_definitely_static defining_expr)
        then begin
          Misc.fatal_errorf "Variable %a bound by phantom let has defining \
              expression that might not be invariant under changes to the \
              program counter: %a"
            V.print var
            Printmach.phantom_defining_expr defining_expr
        end;
        let single_location_description =
          single_phantom_location_description state
            ~parent:(Some (DS.compilation_unit_proto_die state))
            ~proto_dies_for_vars ~need_rvalue defining_expr
        in
        match single_location_description with
        | None -> [], None
        | Some single_location_description ->
          [DAH.create_single_location_description single_location_description;
           DAH.create_external ~is_visible_externally:true;
          ], None
    end else begin
      (* Build a location list that identifies where the value of [var] may be
         found at runtime, indexed by program counter range. The representations
         of location lists (and range lists, used below to describe lexical
         blocks) changed completely between DWARF-4 and DWARF-5. *)
      let dwarf_4_location_list_entries, location_list =
        ARV.Range.fold range
          ~init:([], Location_list.create ())
          ~f:(fun (dwarf_4_location_list_entries, location_list) subrange ->
            let single_location_description =
              single_location_description state fundecl
                ~parent:(Some function_proto_die)
                ~subrange ~proto_dies_for_vars ~need_rvalue
            in
            match single_location_description with
            | None -> dwarf_4_location_list_entries, location_list
            | Some single_location_description ->
              let location_list_entry =
                location_list_entry state ~subrange single_location_description
              in
              match location_list_entry with
              | Dwarf_4 location_list_entry ->
                let dwarf_4_location_list_entries =
                  location_list_entry :: dwarf_4_location_list_entries
                in
                dwarf_4_location_list_entries, location_list
              | Dwarf_5 location_list_entry ->
                let location_list =
                  Location_list.add location_list location_list_entry
                in
                dwarf_4_location_list_entries, location_list)
      in
      match !Clflags.gdwarf_version with
      | Four ->
        let location_list_entries = dwarf_4_location_list_entries in
        let location_list =
          Dwarf_4_location_list.create ~location_list_entries
        in
        [Debug_loc_table.attribute_to_reference_location_list location_list],
          Some location_list
      | Five ->
        let location_list_index =
          Location_list_table.add (DS.location_list_table state) location_list
        in
        [DAH.create_location location_list_index], None
    end
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
  let type_and_name_attributes =
    match type_die_reference_for_var var ~proto_dies_for_vars with
    | None -> []
    | Some reference ->
      let name_is_unique, position_is_unique_given_name_is_unique =
        match Backend_var.Tbl.find uniqueness_by_var var with
        | exception Not_found ->
          Misc.fatal_errorf "No uniqueness information for %a"
            Backend_var.print var
        | { name_is_unique; position_is_unique_given_name_is_unique; } ->
          name_is_unique, position_is_unique_given_name_is_unique
      in
      let name_for_var =
        if name_is_unique then begin
          Backend_var.name_for_debugger var
        end else if position_is_unique_given_name_is_unique then begin
          match provenance with
          | None -> Backend_var.unique_name_for_debugger var
          | Some provenance ->
            let dbg = Backend_var.Provenance.debuginfo provenance in
            match Debuginfo.position dbg with
            | None -> Backend_var.unique_name_for_debugger var
            | Some position ->
              Format.asprintf "%s[%a]"
                (Backend_var.name_for_debugger var)
                Debuginfo.Code_range.print_compact_without_dirname position
        end else begin
          Backend_var.unique_name_for_debugger var
        end
      in
      (* CR-someday mshinwell: This should be tidied up.  It's only correct by
         virtue of the fact we do the closure-env ones second below. *)
      (* CR mshinwell: re-check this CR-someday *)
      let type_attribute =
        if not need_rvalue then begin
          construct_type_of_value_description state
            ~parent:(Some (DS.compilation_unit_proto_die state))
            ident_for_type is_parameter ~phantom_defining_expr
            ~proto_dies_for_vars ~reference
        end;
        [DAH.create_type_from_reference ~proto_die_reference:reference;
        ]
      in
      let name_attribute =
        if hidden || need_rvalue then []
        else [DAH.create_name name_for_var]
      in
      name_attribute @ type_attribute
  in
  let tag : Dwarf_tag.t =
    match is_parameter with
    | Parameter _index ->
      (* The lvalue DIE is the "normal" one for variables and parameters; it
         is the one that is marked with a name, for example.  To avoid
         erroneous display of, or confusion around, rvalue DIEs we always
         mark them as variables not parameters. *)
      if need_rvalue then Variable
      else Formal_parameter
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
      if need_rvalue then None
      else Some index
  in
  Proto_die.create_ignore ?reference
    ?sort_priority
    ?location_list_in_debug_loc_table
    ~parent:(Some parent_proto_die)
    ~tag
    ~attribute_values:(type_and_name_attributes @ location_attribute_value)
    ()

(* This function covers local variables, parameters, variables in closures
   and other "fun_var"s in the current mutually-recursive set.  (The last
   two cases are handled by the explicit addition of phantom lets way back
   in [Flambda_to_clambda].)  Phantom variables are also covered. *)
let iterate_over_variable_like_things state ~available_ranges_vars
      ~rvalues_only ~f =
  ARV.iter available_ranges_vars ~f:(fun var range ->
    let should_process =
      (not rvalues_only)
        || V.Set.mem var (DS.rvalue_dies_required_for state)
    in
    if should_process then begin
      let range_info = ARV.Range.info range in
      let provenance = ARV.Range_info.provenance range_info in
      let phantom : is_variable_phantom =
        match ARV.Range_info.phantom_defining_expr range_info with
        | Non_phantom ->
          begin match provenance with
          | None -> ()
          | Some provenance ->
            if V.Provenance.is_static provenance then begin
              Misc.fatal_errorf "Variable %a marked as static, but it is not \
                  bound by a phantom let"
                V.print var
            end
          end;
          Non_phantom
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
      (* CR-someday mshinwell: Introduce some flag on Backend_var.t to mark
         identifiers that were generated internally (or vice-versa)?  We
         should probably also hide certain internally-generated identifiers
         that appear in .cmt files but did not come from the source code. *)
      let hidden = Backend_var.is_internal var in
      let ident_for_type =
        match provenance with
        | None ->
          (* In this case the variable won't be given a name in the DWARF,
             so as not to appear in the debugger; but we still need to emit
             a DIE for it, as it may be referenced as part of some chain of
             phantom lets. *)
          let in_startup_file =
            (* The startup file is generated from Cmm code and is therefore
               not expected to link back to any .cmt file via identifier
               names and stamps. *)
            Compilation_unit.equal (Compilation_unit.get_current_exn ())
              Compilation_unit.startup
          in
          if (not hidden) && (not in_startup_file) then begin
            Misc.fatal_errorf "Variable %a is not hidden, but has no \
                provenance\n%!"
              Backend_var.print var
          end;
          None
        | Some provenance ->
          Some (Backend_var.Provenance.ident_for_type provenance)
      in
      f var ~phantom ~hidden ~ident_for_type ~range
    end)

let dwarf state fundecl ~function_proto_die ~scope_proto_dies
      available_ranges_vars =
  let proto_dies_for_vars = Backend_var.Tbl.create 42 in
  let uniqueness_by_var =
    calculate_var_uniqueness ~available_ranges_vars
  in
  iterate_over_variable_like_things state ~available_ranges_vars
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
  DS.set_rvalue_dies_required_for state V.Set.empty;
  (* CR-someday mshinwell: Consider changing [need_rvalue] to use a variant
     type "lvalue or rvalue". *)
  iterate_over_variable_like_things state ~available_ranges_vars
    ~rvalues_only:false
    ~f:(dwarf_for_variable state fundecl ~function_proto_die
      ~scope_proto_dies ~uniqueness_by_var ~proto_dies_for_vars
      ~need_rvalue:false);
  iterate_over_variable_like_things state ~available_ranges_vars
    ~rvalues_only:true
    ~f:(dwarf_for_variable state fundecl ~function_proto_die
      ~scope_proto_dies ~uniqueness_by_var ~proto_dies_for_vars
      ~need_rvalue:true)
