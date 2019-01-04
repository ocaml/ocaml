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

module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state
module SLDL = Simple_location_description_lang

let dwarf_for_toplevel_constant state
      (provenance : Clambda.usymbol_provenance option) ~symbol
      (const : Clambda.preallocated_constant) =
  let symbol = Asm_symbol.create symbol in
  let common_attributes =
    [ DAH.create_const_value_from_symbol ~symbol;
      (* Mark everything as "external" so gdb puts the constants in its
         list of "global symbols". *)
      DAH.create_external ~is_visible_externally:true;
    ]
  in
  let vars_and_module_path =
    match provenance with
    | None -> None
    | Some provenance ->
      Some (provenance.idents_for_types, provenance.module_path)
  in
  match vars_and_module_path with
  | None ->
    (* No name is provided, so the debugger should hide this constant
       definition from the user; but it can still be referenced from
       elsewhere in the DWARF information. *)
    let type_attributes =
      match const.definition with
      | Uconst_float _
      | Uconst_int32 _
      | Uconst_int64 _
      | Uconst_nativeint _
      | Uconst_block _
      | Uconst_float_array _ 
      | Uconst_string _
      | Uconst_closure _ ->
        (* The best we can do here, since we haven't got a source-level
           variable (and hence no OCaml type), is to have the debugger print
           the constant based only on what is found in the heap. *)
        let type_die = DS.value_type_proto_die state in
        [ DAH.create_type_from_reference
            ~proto_die_reference:(Proto_die.reference type_die)
        ]
    in
    Proto_die.create_ignore
      ~parent:(Some (DS.compilation_unit_proto_die state))
      ~tag:Constant
      ~attribute_values:(common_attributes @ type_attributes)
      ()
  | Some (vars, module_path) ->
    (* Give each variable bound to this symbol the same definition for the
       moment. *)
    List.iter (fun var ->
        let name =
          let path = Printtyp.string_of_path module_path in
          let name = Backend_var.name var in
          path ^ "." ^ name
        in
        let type_proto_die =
          Dwarf_variables_and_parameters.normal_type_for_var
            ~parent:(Some (DS.compilation_unit_proto_die state))
            (Some (Compilation_unit.get_current_exn (), var))
        in
        (* The type DIE is noted down so that, when we find a call site one
           of whose arguments contains [symbol], we can link to it. *)
        DS.record_type_die_for_lifted_constant state symbol type_proto_die;
        Proto_die.create_ignore
          ~parent:(Some (DS.compilation_unit_proto_die state))
          ~tag:Constant
          ~attribute_values:(common_attributes @ [
            DAH.create_name name;
            DAH.create_type ~proto_die:type_proto_die;
          ])
          ())
      vars

let dwarf_for_toplevel_constants state constants =
  List.iter (fun (constant : Clambda.preallocated_constant) ->
      match constant.definition with
      | Uconst_closure _ ->
        (* Function declarations are emitted separately.  There's no more
           information that we require in a toplevel constant closure. *)
        ()
      | _ ->
        dwarf_for_toplevel_constant state constant.provenance
          ~symbol:constant.symbol constant)
    constants

let dwarf_for_closure_top_level_module_block state ~module_block_sym
      ~module_block_var =
  assert (not Config.flambda);
  let name = Backend_var.name module_block_var in
  let type_proto_die =
    Dwarf_variables_and_parameters.normal_type_for_var
      ~parent:(Some (DS.compilation_unit_proto_die state))
      (Some (Compilation_unit.get_current_exn (), module_block_var))
  in
  let single_location_description =
    let symbol = Asm_symbol.create module_block_sym in
    let lang = SLDL.Lvalue.const_symbol ~symbol in
    Single_location_description.of_simple_location_description
      (SLDL.compile (SLDL.of_lvalue lang))
  in
  Proto_die.create_ignore ~parent:(Some (DS.compilation_unit_proto_die state))
    ~tag:Variable
    ~attribute_values:[
      DAH.create_name name;
      DAH.create_type ~proto_die:type_proto_die;
      DAH.create_single_location_description single_location_description;
      DAH.create_external ~is_visible_externally:true;
    ]
    ()
