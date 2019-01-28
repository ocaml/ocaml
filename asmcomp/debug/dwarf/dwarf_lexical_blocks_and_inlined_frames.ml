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
module L = Linearize
module LB = Lexical_block_ranges

let find_scope_die_from_debuginfo dbg ~function_proto_die ~scope_proto_dies =
  let innermost_block = Debuginfo.innermost_block dbg in
  match Debuginfo.Current_block.to_block innermost_block with
  | Toplevel -> Some function_proto_die
  | Block block ->
    let module B = Debuginfo.Block in
    match B.Map.find block scope_proto_dies with
    | exception Not_found -> None
    | proto_die -> Some proto_die

let create_range_list_and_summarise state (_fundecl : L.fundecl) range =
  LB.Range.fold range
    ~init:([], Range_list.create (), Address_index.Pair.Set.empty)
    ~f:(fun (dwarf_4_range_list_entries, range_list, summary) subrange ->
      let start_pos = LB.Subrange.start_pos subrange in
      (* CR mshinwell: Deal with [start_pos_offset] for completeness, or
         assert that it's zero and explain why. *)
      let end_pos = LB.Subrange.end_pos subrange in
      let end_pos_offset = LB.Subrange.end_pos_offset subrange in
      let start_inclusive =
        Address_table.add (DS.address_table state)
          (Asm_label.create_int Text start_pos)
          ~start_of_code_symbol:(DS.start_of_code_symbol state)
      in
      let end_exclusive =
        Address_table.add (DS.address_table state)
          (Asm_label.create_int Text end_pos)
          ~adjustment:(LB.Subrange.end_pos_offset subrange)
          ~start_of_code_symbol:(DS.start_of_code_symbol state)
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
          ~start_of_code_symbol:(DS.start_of_code_symbol state)
      in
      (* We still use the [Range_list] when emitting DWARF-4 (even though
         it is a DWARF-5 structure) for the purposes of de-duplicating
         ranges. *)
      let range_list = Range_list.add range_list range_list_entry in
      let summary =
        Address_index.Pair.Set.add (start_inclusive, end_exclusive) summary
      in
      let dwarf_4_range_list_entries =
        match !Clflags.dwarf_version with
        | Four ->
          let range_list_entry =
            Dwarf_4_range_list_entry.create_range_list_entry
              ~start_of_code_symbol:(DS.start_of_code_symbol state)
              ~first_address_when_in_scope:(Asm_label.create_int Text start_pos)
              ~first_address_when_not_in_scope:
                (Asm_label.create_int Text end_pos)
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

let die_for_lexical_block parent range_list_attribute =
  Proto_die.create ~parent:(Some parent)
    ~tag:Lexical_block
    ~attribute_values:[
      range_list_attribute;
    ]
    ()

let die_for_inlined_frame state parent call_site range range_list_attribute =
  let fun_dbg = Debuginfo.Call_site.fun_dbg call_site in
  let abstract_instance_symbol =
    Dwarf_abstract_instances.find_maybe_in_another_unit_or_add state fun_dbg
  in
  let entry_pc =
    (* CR-someday mshinwell: The "entry PC" is supposed to be the address of the
       "temporally first" instruction of the inlined function. We assume here
       that we don't do transformations which might cause the first instruction
       of the inlined function to not be the one at the lowest address amongst
       all instructions of the inlined function. If this assumption is wrong the
       most likely outcome seems to be breakpoints being slightly in the wrong
       place, although still in the correct function. Making this completely
       accurate will necessitate more tracking of instruction ordering from
       earlier in the compiler. *)
    match LB.Range.lowest_address range with
    | None -> []
    | Some lowest_address -> [
        DAH.create_entry_pc (Asm_label.create_int Text lowest_address);
      ]
  in
  (* Note that with Flambda, this DIE may not be in the scope of the referenced
     abstract instance DIE, as inline expansions may be made out of the scope of
     the function declaration. *)
  let abstract_instance =
    match abstract_instance_symbol with
    | None ->
      (* If the abstract instance DIE cannot be referenced, reconstitute as much
         of its attributes as we can and put them directly into the DIE for the
         inlined frame, making use of DWARF-5 spec page 85, line 30 onwards. *)
      Dwarf_abstract_instances.attributes fun_dbg
    | Some abstract_instance_symbol ->
      (* This appears to be the source of a bogus error from "dwarfdump
         --verify" on macOS:

         error: <address>: DIE has tag Unknown DW_TAG constant: 0x4109 has
         DW_AT_abstract_origin that points to DIE <address> with incompatible
         tag TAG_subprogram

         The error seems to be bogus because:

         (a) 0x4109 is DW_TAG_GNU_call_site which has no DW_AT_abstract_origin
             attribute (it does in a child, but not in itself); and

         (b) When DW_AT_abstract_origin is used to reference an abstract
             instance then it is expected that the tags of the referring and
             referred-to DIEs differ. DWARF-5 spec page 85, lines 4--8.

         Since this complaint does not appear during a normal build process we
         do not attempt to work around it. *)
      [DAH.create_abstract_origin ~die_symbol:abstract_instance_symbol]
  in
  let code_range = Debuginfo.Call_site.position call_site in
  let file_name = Debuginfo.Code_range.file code_range in
  Proto_die.create ~parent:(Some parent)
    ~tag:Inlined_subroutine
    ~attribute_values:(entry_pc @ abstract_instance @ [
      range_list_attribute;
      DAH.create_call_file (Emitaux.file_num_for ~file_name);
      DAH.create_call_line (Debuginfo.Code_range.line code_range);
      DAH.create_call_column (Debuginfo.Code_range.char_start code_range);
    ])
    ()

let dwarf state (fundecl : L.fundecl) lexical_block_ranges ~function_proto_die =
  let module B = Debuginfo.Block in
  let all_blocks = LB.all_indexes lexical_block_ranges in
  let scope_proto_dies, _all_summaries =
    B.Set.fold (fun block (scope_proto_dies, all_summaries) ->
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
            let range = LB.find lexical_block_ranges block in
            let range_list_attribute, all_summaries =
              let dwarf_4_range_list_entries, range_list, summary =
                create_range_list_and_summarise state fundecl range
              in
              match All_summaries.Map.find summary all_summaries with
              | exception Not_found ->
                let range_list_index =
                  Range_list_table.add (DS.range_list_table state) range_list
                in
                let range_list_attribute =
                  match !Clflags.dwarf_version with
                  | Four ->
                    let range_list =
                      Dwarf_4_range_list.create
                        ~range_list_entries:dwarf_4_range_list_entries
                    in
                    Debug_ranges_table.insert (DS.debug_ranges_table state)
                      ~range_list
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
                die_for_lexical_block parent range_list_attribute
              | Inlined_frame call_site ->
                die_for_inlined_frame state parent call_site range
                  range_list_attribute
            in
            let scope_proto_dies = B.Map.add block proto_die scope_proto_dies in
            proto_die, scope_proto_dies, all_summaries
        in
        let _proto_die, scope_proto_dies, all_summaries =
          create_up_to_root block scope_proto_dies all_summaries
        in
        scope_proto_dies, all_summaries)
      all_blocks
      (B.Map.empty, All_summaries.Map.empty)
  in
  scope_proto_dies
