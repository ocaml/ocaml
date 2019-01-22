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
module SLDL = Simple_location_description_lang

let arch_size_addr = Targetint.of_int_exn Arch.size_addr

let add_call_site_argument state ~call_site_die ~is_tail ~arg_index
      ~(arg : Reg.t) ~stack_offset (insn : L.instruction) =
  let param_location =
    let offset_from_cfa_in_bytes =
      match arg.loc with
      | Stack stack_loc ->
        Dwarf_reg_locations.offset_from_cfa_in_bytes arg stack_loc ~stack_offset
      | Reg _ -> None
      | Unknown ->
        Misc.fatal_errorf "Register without location: %a"
          Printmach.reg arg
    in
    let param_location =
      Dwarf_reg_locations.reg_location_description arg ~offset_from_cfa_in_bytes
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
        let create_composite_location_description arg_location =
          let composite =
            Composite_location_description.
              pieces_of_simple_location_descriptions
                [arg_location, arch_size_addr]
          in
          [DAH.create_composite_call_value_location_description
            composite;
          ]
        in
        match holds_value_of with
        | Var holds_value_of ->
          let type_attribute =
            match Reg_with_debug_info.Debug_info.provenance debug_info with
            | None -> []
            | Some provenance ->
              let type_die =
                (* CR mshinwell: This should reuse DIEs which were created
                   previously to describe these vars.  Also, shouldn't these
                   DIEs be parented higher up? *)
                Dwarf_variables_and_parameters.normal_type_for_var
                  ~parent:(Some call_site_die)
                  (Some (Backend_var.Provenance.ident_for_type provenance))
                  (Backend_var.Provenance.is_parameter provenance)
              in
              [DAH.create_type_from_reference
                ~proto_die_reference:(Proto_die.reference type_die)
              ]
          in
          let arg_location =
            if is_tail then begin
              (* If this is a tail call site, no arguments of the call will be
                 available with certainty in the callee, since any argument we
                 describe in DWARF (see comment below) will be on the
                 stack---and our stack frame may have been overwritten by the
                 time such argument is queried in the debugger. *)
              []
            end else begin
              let everywhere_holding_var =
                Reg_availability_set.find_all_holding_value_of
                  insn.available_before (Var holds_value_of)
              in
              (* Only registers spilled at the time of the call will be
                 available with certainty (in the caller's frame) during the
                 execution of the callee. *)
              let on_stack =
                Misc.Stdlib.List.filter_map (fun rd ->
                    let reg = Reg_with_debug_info.reg rd in
                    match reg.loc with
                    | Stack stack_loc ->
                      Some (reg,
                        Dwarf_reg_locations.offset_from_cfa_in_bytes reg
                          stack_loc ~stack_offset)
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
                  Dwarf_reg_locations.reg_location_description reg
                    ~offset_from_cfa_in_bytes ~need_rvalue:true
                in
                match arg_location_rvalue with
                | None -> []
                | Some arg_location_rvalue ->
                  (* gdb does not seem to accept a simple location description
                     here -- it complains about the use of
                     [DW_OP_stack_value]. *)
                  create_composite_location_description arg_location_rvalue
            end
          in
          arg_location, type_attribute
        | Const_int i ->
          let arg_location =
            create_composite_location_description
              (SLDL.compile (SLDL.of_rvalue (SLDL.Rvalue.signed_int_const i)))
          in
          let type_attribute =
            [ DAH.create_type ~proto_die:(DS.value_type_proto_die state)
            ]
          in
          (* CR mshinwell: It looks like gdb is taking the type from the
             parameter.  Maybe we should alter gdb to pass both the
             argument's type and the parameter's type through, so that
             libmonda can choose which is best (e.g. for a polymorphic
             function we might prefer the argument). *)
          arg_location, type_attribute
        | Const_naked_float f ->
          (* CR mshinwell: This shouldn't happen at the moment *)
          let arg_location =
            create_composite_location_description
              (SLDL.compile (SLDL.of_rvalue (SLDL.Rvalue.float_const f)))
          in
          let type_attribute =
            [ DAH.create_type ~proto_die:(DS.naked_float_type_proto_die state)
            ]
          in
          arg_location, type_attribute
        | Const_symbol symbol ->
          let symbol = Asm_symbol.create symbol in
          let arg_location =
            create_composite_location_description
              (SLDL.compile (SLDL.of_rvalue (SLDL.Rvalue.const_symbol symbol)))
          in
          let type_attribute =
            (* CR-someday mshinwell: Work out what we need to do in order to
               know whether a given symbol, in another compilation unit, has
               a DWARF DIE. *)
            match DS.type_die_for_lifted_constant state symbol with
            | None ->
              (* In this case the debugger will have to print the value using
                 only the contents of the heap, and not the OCaml type. *)
              (* CR mshinwell: Except that it seems to use the type from the
                 parameter -- see above *)
              [ DAH.create_type ~proto_die:(DS.value_type_proto_die state)
              ]
            | Some type_die ->
              (* This case may arise where the defining expression of a
                 variable was lifted, but an occurrence of the variable as one
                 of the call site arguments was substituted out.  Despite the
                 substitution, we can still retrieve the OCaml type. *)
              [ DAH.create_type ~proto_die:type_die
              ]
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

let add_call_site state ~scope_proto_dies ~function_proto_die
      ~stack_offset ~is_tail ~args ~(call_labels : Mach.call_labels)
      (insn : L.instruction) attrs =
  let dbg = insn.dbg in
  let block_die =
    Dwarf_lexical_blocks_and_inlined_frames.find_scope_die_from_debuginfo dbg
      ~function_proto_die ~scope_proto_dies
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
        | Five ->
          let file_name = Debuginfo.Code_range.file code_range in
          [ DAH.create_call_file (Emitaux.file_num_for ~file_name);
            DAH.create_call_line (Debuginfo.Code_range.line code_range);
            DAH.create_call_column (Debuginfo.Code_range.char_start code_range);
          ]
    in
    let call_site_die =
      let dwarf_5_only =
        match !Clflags.dwarf_version with
        | Four -> [
            DAH.create_low_pc (Asm_label.create_int Text call_labels.after);
          ]
        | Five -> [
            DAH.create_call_pc (Asm_label.create_int Text call_labels.before);
            DAH.create_call_return_pc (
              Asm_label.create_int Text call_labels.after);
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
          add_call_site_argument state ~call_site_die ~is_tail ~arg_index ~arg
            ~stack_offset insn)
        args
    end

type direct_callee =
  | Ocaml of Asm_symbol.t * Debuginfo.Function.t
  | External of Asm_symbol.t * Linkage_name.t

let call_target_for_direct_callee state (callee : direct_callee) =
  (* If we cannot reference DIEs across compilation units, then we treat direct
     calls to OCaml functions in other compilation units as if they were to an
     external function, fabricating our own subprogram DIE for each such
     function and providing the "low PC" and "entry PC" value that GDB looks for
     (or may look for in the future). See
     gdb/dwarf2read.c:read_call_site_scope. *)
  let callee =
    match callee with
    | Ocaml (callee_symbol, callee_dbg)
        when (not (DS.can_reference_dies_across_units state))
          && (not (Compilation_unit.equal
            (Asm_symbol.compilation_unit callee_symbol)
            (Compilation_unit.get_current_exn ()))) ->
      let linkage_name =
        (* See comment about linkage names in dwarf_concrete_instances.ml. *)
        Linkage_name.create (
          Debuginfo.Function.name_with_module_path callee_dbg)
      in
      External (callee_symbol, linkage_name)
    | _ -> callee
  in
  let die_symbol =
    match callee with
    | Ocaml (_callee_symbol, callee_dbg) ->
      if not (Debuginfo.Function.dwarf_die_present callee_dbg) then None
      else
        let id = Debuginfo.Function.id callee_dbg in
        Some (Dwarf_name_laundry.concrete_instance_die_name id)
    | External (callee, linkage_name) ->
      match
        Asm_symbol.Tbl.find (DS.die_symbols_for_external_declarations state)
          callee
      with
      | exception Not_found ->
        (* CR-someday mshinwell: dedup DIEs for runtime, "caml_curry", etc.
           functions across compilation units (maybe only generate the DIEs
           when compiling the startup file)? *)
        let callee_die =
          Proto_die.create ~parent:(Some (DS.compilation_unit_proto_die state))
            ~tag:Subprogram
            ~attribute_values:[
              DAH.create_declaration ();
              (* We use the linkage name rather than the actual symbol address
                 [of the target function] because dsymutil helpfully
                 replaces such symbol references, when they refer to other
                 compilation units, with zeros. *)
              DAH.create_linkage_name linkage_name;
            ]
            ()
        in
        let die_symbol =
          Dwarf_name_laundry.external_declaration_die_name callee
            (Compilation_unit.get_current_exn ())
        in
        Proto_die.set_name callee_die die_symbol;
        Asm_symbol.Tbl.add (DS.die_symbols_for_external_declarations state)
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
      let offset_from_cfa_in_bytes =
        Dwarf_reg_locations.offset_from_cfa_in_bytes callee stack_loc
          ~stack_offset
      in
      offset_from_cfa_in_bytes, false
    | Reg _ -> None, true
    | Unknown ->
      Misc.fatal_errorf "Register without location: %a" Printmach.reg callee
  in
  let simple_location_desc =
    Dwarf_reg_locations.reg_location_description callee
      ~offset_from_cfa_in_bytes ~need_rvalue:false
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

let dwarf state ~scope_proto_dies (fundecl : L.fundecl)
      ~external_calls_generated_during_emit ~function_symbol
      ~function_proto_die =
  let found_self_tail_calls = ref false in
  let add_call_site =
    add_call_site ~scope_proto_dies ~function_proto_die
  in
  let add_indirect_ocaml_call ~stack_offset ~callee ~args ~call_labels insn =
    add_call_site state ~stack_offset ~is_tail:false ~args ~call_labels insn
      (call_target_for_indirect_callee ~callee ~stack_offset)
  in
  let add_direct_ocaml_call ~stack_offset ~callee ~callee_dbg ~args
        ~call_labels insn =
    match callee_dbg with
    | None -> ()
    | Some callee_dbg ->
      add_call_site state ~stack_offset ~is_tail:false ~args ~call_labels insn
        (call_target_for_direct_callee state (Ocaml (callee, callee_dbg)))
  in
  let add_indirect_ocaml_tail_call ~stack_offset ~callee ~args ~call_labels
        insn =
    add_call_site state ~stack_offset ~is_tail:true ~args ~call_labels insn
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
        add_call_site state ~stack_offset ~is_tail:true ~args ~call_labels insn
          (call_target_for_direct_callee state (Ocaml (callee, callee_dbg)))
      end
  in
  let add_external_call ~stack_offset ~callee ~args ~call_labels insn =
    let linkage_name = Asm_symbol.linkage_name callee in
    add_call_site state ~stack_offset ~is_tail:false ~args ~call_labels insn
      (call_target_for_direct_callee state (External (callee, linkage_name)))
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
        | Icall_imm { func = callee; callee_dbg; call_labels; _ } ->
          let callee = Asm_symbol.create callee in
          add_direct_ocaml_call ~stack_offset ~callee
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
