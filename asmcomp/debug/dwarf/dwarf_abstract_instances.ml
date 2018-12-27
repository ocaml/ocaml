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

module DS = Dwarf_state

let attributes fun_dbg =
  let function_name = Debuginfo.Function.name fun_dbg in
  let is_visible_externally =
    Debuginfo.Function.is_visible_externally fun_dbg
  in
  [ DAH.create_name function_name;
    DAH.create_external ~is_visible_externally;
  ]

let add state fun_dbg =
  let abstract_instance_proto_die =
    (* DWARF-5 specification section 3.3.8.1, page 82. *)
    Proto_die.create ~parent:(Some (DS.compilation_unit_proto_die state))
      ~tag:Subprogram
      ~attribute_values:((attributes fun_dbg) @ [
        (* We assume every function might potentially be inlined (and possibly
           in the future), so we choose [DW_INL_inlined] as the most appropriate
           setting for [DW_AT_inline], even if it doesn't seem exactly
           correct.  We must set something here to ensure that the subprogram
           is marked as an abstract instance root. *)
        DAH.create_inline Inlined;
      ])
      ()
  in
  let id = Debuginfo.Function.id fun_dbg in
  let abstract_instance_proto_die_symbol =
    Dwarf_name_laundry.abstract_instance_root_die_name id
  in
  Proto_die.set_name abstract_instance_proto_die
    abstract_instance_proto_die_symbol;
  Debuginfo.Function.Id.Tbl.add (DS.function_abstract_instances state) id
    (abstract_instance_proto_die, abstract_instance_proto_die_symbol);
  abstract_instance_proto_die, abstract_instance_proto_die_symbol

let find_or_add state fun_dbg =
  let id = Debuginfo.Function.id fun_dbg in
  match
    Debuginfo.Function.Id.Tbl.find (DS.function_abstract_instances state) id
  with
  | exception Not_found -> add state fun_dbg
  | existing_instance -> existing_instance

let find_maybe_in_another_unit_or_add state fun_dbg =
  if not (Debuginfo.Function.dwarf_die_present fun_dbg) then
    None
  else
    let id = Debuginfo.Function.id fun_dbg in
    let dbg_comp_unit = Debuginfo.Function.Id.compilation_unit id in
    let this_comp_unit = Compilation_unit.get_current_exn () in
    if Compilation_unit.equal dbg_comp_unit this_comp_unit then
      let _abstract_instance_proto_die, abstract_instance_proto_die_symbol =
        find_or_add state fun_dbg
      in
      Some abstract_instance_proto_die_symbol
    else if can_reference_dies_across_units () then
      Some (Dwarf_name_laundry.abstract_instance_root_die_name id)
    else
      None
