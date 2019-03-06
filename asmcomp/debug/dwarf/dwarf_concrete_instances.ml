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

(* CR mshinwell: Function names are not being disambiguated
   (e.g. Ctype.unify) *)

let for_fundecl state (result : Debug_passes.result) =
  let available_ranges_vars = result.available_ranges_vars in
  let fundecl = result.fundecl in
  let lexical_block_ranges = result.lexical_block_ranges in
  let external_calls_generated_during_emit =
    result.external_calls_generated_during_emit
  in
  let symbol = Asm_symbol.create fundecl.fun_name in
  (* Our "linkage names" in DWARF are in fact normal OCaml qualified paths.
     This is done, instead of using the actual symbol names, because there
     appears to be a presumption in GDB that demangled names can be deduced
     from mangled names.  This is not the case, reliably, using the current
     OCaml mangling scheme.  Since fully-qualified name should be unique across
     a whole program anyway it doesn't seem that there is any real downside
     to using such names as the linkage names. *)
  let linkage_name =
    Linkage_name.create (
      Debuginfo.Function.name_with_module_path fundecl.fun_dbg)
  in
  let start_of_function = DAH.create_low_pc_from_symbol symbol in
  let end_of_function =
    DAH.create_high_pc ~low_pc:symbol
      (Asm_label.create_int Text result.end_of_function_label)
  in
  let _abstract_instance_proto_die, abstract_instance_die_symbol =
    Dwarf_abstract_instances.find_or_add state fundecl.fun_dbg
  in
  let module_path = Debuginfo.Function.module_path fundecl.fun_dbg in
  let parent = Dwarf_modules.dwarf state ~module_path in
  let concrete_instance_proto_die =
    Proto_die.create ~parent:(Some parent)
      ~tag:Subprogram
      ~attribute_values:[
        start_of_function;
        end_of_function;
        DAH.create_entry_pc_from_symbol symbol;
        DAH.create_abstract_origin ~die_symbol:abstract_instance_die_symbol;
        DAH.create_linkage_name linkage_name;
      ]
      ()
  in
  Proto_die.set_name concrete_instance_proto_die
    (Dwarf_name_laundry.concrete_instance_die_name
      (Debuginfo.Function.id fundecl.fun_dbg));
  if Clflags.debug_thing Debug_dwarf_scopes then begin
    let scope_proto_dies =
      Profile.record "dwarf_lexical_blocks_and_inlined_frames"
        (fun () ->
          Dwarf_lexical_blocks_and_inlined_frames.dwarf state fundecl
            lexical_block_ranges ~function_proto_die:concrete_instance_proto_die)
        ~accumulate:true
        ()
    in
    if Clflags.debug_thing Debug_dwarf_vars then begin
      Profile.record "dwarf_variables_and_parameters" (fun () ->
          Dwarf_variables_and_parameters.dwarf state fundecl
            ~function_proto_die:concrete_instance_proto_die
            ~scope_proto_dies available_ranges_vars)
        ~accumulate:true
        ();
      if (DS.supports_call_sites state) then begin
        let found_self_tail_calls =
          Profile.record "dwarf_call_sites" (fun () ->
              Dwarf_call_sites.dwarf state ~scope_proto_dies fundecl
                ~external_calls_generated_during_emit ~function_symbol:symbol
                ~function_proto_die:concrete_instance_proto_die)
            ~accumulate:true
            ()
        in
        if not found_self_tail_calls then begin
          Proto_die.add_or_replace_attribute_value concrete_instance_proto_die
            (DAH.create_call_all_calls ())
        end
      end
    end
  end
