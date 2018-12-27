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
module DS = Dwarf_state
module L = Linearize
module LB = Lexical_block_ranges
module SLDL = Simple_location_description_lang
module V = Backend_var

let dwarf_for_toplevel_constant state ~vars ~module_path ~symbol =
  (* Give each variable the same definition for the moment. *)
  List.iter (fun var ->
      let name =
        let path = Printtyp.string_of_path module_path in
        let name = Backend_var.name var in
        path ^ "." ^ name
      in
      let type_proto_die =
        Dwarf_variables_and_parameters.normal_type_for_var state
          ~parent:(Some (DS.compilation_unit_proto_die state))
          (Some (Compilation_unit.get_current_exn (), var))
      in
      let symbol = mangle_symbol Data symbol in
      Proto_die.create_ignore
        ~parent:(Some (DS.compilation_unit_proto_die state))
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

let dwarf_for_toplevel_constants state constants =
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
          dwarf_for_toplevel_constant state ~vars:provenance.idents_for_types
            ~module_path:provenance.module_path
            ~symbol)
    constants

let dwarf_for_toplevel_inconstant state var ~module_path ~symbol =
  let name =
    let path = Printtyp.string_of_path module_path in
    let name = Backend_var.name var in
    path ^ "." ^ name
  in
  let type_proto_die =
    Dwarf_variables_and_parameters.normal_type_for_var state
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
        let symbol =
          Symbol.of_global_linkage (Compilation_unit.get_current_exn ())
            (Linkage_name.create inconstant.symbol)
        in
        let symbol = mangle_symbol Data symbol in
        (* CR-someday mshinwell: Support multi-field preallocated blocks
           (ignored for the moment as the only one is the module block, which
           isn't made visible in the debugger). *)
        match provenance.idents_for_types with
        | [] | _::_::_ -> ()
        | [ident] ->
          dwarf_for_toplevel_inconstant state ident
            ~module_path:provenance.module_path
            ~symbol)
    inconstants
