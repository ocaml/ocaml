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

let dwarf_for_toplevel_inconstant state var ~module_path ~symbol =
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
      let symbol = Asm_symbol.create symbol in
      let lang =
        SLDL.Rvalue.read_symbol_field ~symbol ~field:Targetint.zero
      in
      SLDL.compile (SLDL.of_rvalue lang))
  in
  Proto_die.create_ignore ~parent:(Some (DS.compilation_unit_proto_die state))
    ~tag:Variable
    ~attribute_values:[
      DAH.create_name name;
      DAH.create_type ~proto_die:type_proto_die;
      DAH.create_single_location_description single_location_description;
      DAH.create_external ~is_visible_externally:true;  (* see above *)
    ]
    ()

let dwarf_for_toplevel_inconstants state inconstants =
  List.iter (fun (inconstant : Clambda.preallocated_block) ->
      (* CR-soon mshinwell: Should we be discarding toplevel things that don't
         have provenance?  Maybe not -- think. *)
      match inconstant.provenance with
      | None -> ()
      | Some provenance ->
        (* CR-someday mshinwell: Support multi-field preallocated blocks
           (ignored for the moment as the only one is the module block, which
           isn't made visible in the debugger). *)
        match provenance.idents_for_types with
        | [] | _::_::_ -> ()
        | [ident] ->
          dwarf_for_toplevel_inconstant state ident
            ~module_path:provenance.module_path
            ~symbol:inconstant.symbol)
    inconstants
