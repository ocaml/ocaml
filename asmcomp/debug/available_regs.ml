(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Mark Shinwell and Thomas Refis, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 2013--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* XCR pchambart: I'm still not convinced by the name of the pass.
   unclobered_regs maybe ?
   mshinwell: I think it's the usual name.  It matches what's used in
   CompCert for something similar IIUC.
*)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module M = Mach
module R = Reg
module RAS = Reg_availability_set
module RD = Reg_with_debug_info

(* XCR pchambart is this only needed for debugging ?
   mshinwell: yes, removed *)

(* XCR pchambart I'm not certain that exits numbers cannot be nested:
   every other pass uses a list for that I assume
   mshinwell: we rely on the Hashtbl shadowing semantics now. *)

(* This pass treats [avail_at_exit] like a "result" structure whereas the
   equivalent in [Liveness] is like an "environment".  (Which means we need
   to be careful not to throw away information about further-out catch
   handlers collected in [avail_at_exit].) *)
let avail_at_exit = Hashtbl.create 42
(* XCR pchambart: None here is in some way the same thing as all_regs
   mshinwell: (now called Unreachable) *)
let avail_at_raise = ref RAS.Unreachable

(* XCR pchambart: not 'interessting' but contain source level relevant value.
   This may be easier to just filter later (when printing ?).
   Keeping those in gdb might help debug the code generation such
   as corrupted values due to unregistered roots.
   mshinwell: filtering on name now moved to [Available_filtering]
   (although maybe that's still too early?).
*)

(* XCR-pchambart it does not really convey the fact that it's
   unreachable. Having a sum type to represent that may be cleaner.
   I don't think this allocation cost would really matter *)

let augment_availability_at_raise avail =
  avail_at_raise := RAS.inter avail !avail_at_raise

(* [available_regs ~instr ~avail_before] calculates, given the registers
   "available before" an instruction [instr], the registers that are available
   after [instr].  This is a forwards dataflow analysis.

   "available before" can be thought of, at the assembly level, as the set of
   registers available when the program counter is equal to the address of the
   particular instruction under consideration (that is to say, immediately
   prior to the instruction being executed).  Inputs to that instruction are
   available at this point even if the instruction will clobber them.  Results
   from the previous instruction are also available at this point.

   The [available_before] field of each instruction is updated by this
   function.
*)
let rec available_regs (instr : M.instruction)
      ~(avail_before : RAS.t) : RAS.t =
  let avail_before : RAS.t =
    (* If this instruction might expand into multiple machine instructions
       and clobber registers during that code sequence, make sure any
       such clobbered registers are marked as "unavailable before". *)
    (* CR-someday mshinwell: We could maybe try to improve on this in the
       future, so e.g. when stopped immediately before an allocation,
       registers aren't marked unavailable when not yet clobbered by the
       allocation code.
       Should we be using "available across" instead of "available before"?
       It's not clear that actually solves the problem. *)
    (* XCR pchambart: This test should probably surround everything,
       especialy in the case where it is changed to a sum type. *)
    match avail_before with
    | Unreachable -> Unreachable
    | Ok avail_before ->
      (* Every register that is live across an instruction should also be
         available before the instruction.  (We cannot assert that a register
         should not be an input to an instruction unless it is available due
         to the special case below.) *)
      (* XCR pchambart: The difference between live and avail_before is
         probably due to the 'interesting' filter. *)
      if not (R.Set.subset instr.live (RD.Set.forget_debug_info avail_before))
      then begin
        Misc.fatal_errorf "Live registers not a subset of available registers: \
            live={%a} avail_before=%a missing={%a} insn=%a"
          Printmach.regset instr.live
          (RAS.print ~print_reg:Printmach.reg)
          (RAS.Ok avail_before)
          Printmach.regset (R.Set.diff instr.live
            (RD.Set.forget_debug_info avail_before))
          Printmach.instr ({ instr with M. next = M.end_instr (); })
      end;
      match instr.desc with
      | Iop (Ialloc _) ->
        let made_unavailable =
          RD.Set.made_unavailable_by_clobber avail_before
            ~regs_clobbered:(Proc.destroyed_at_oper instr.desc)
            ~register_class:Proc.register_class
        in
        Ok (RD.Set.diff avail_before made_unavailable)
      (* CR mshinwell: should have a hook for Ispecific cases *)
      | _ -> Ok avail_before
  in
  (* XCR pchambart: Given how it's used I would rename it to
     available_across rather than available_before.
     mshinwell: I don't think I want to change this at the moment.  Everything
     has been thought about with this field meaning the registers available
     immediately prior to the instruction. *)
  instr.available_before <- avail_before;
  let avail_after : RAS.t =
    match avail_before with
    | Unreachable -> Unreachable
    | Ok avail_before ->
      match instr.desc with
      | Iend -> Ok avail_before
      | Ireturn | Iop (Itailcall_ind _) | Iop (Itailcall_imm _) ->
        Unreachable
      | Iop (Iname_for_debugger { ident; which_parameter; provenance;
          is_assignment; }) ->
        (* First forget about any existing debug info to do with [ident]
           if the naming corresponds to an assignment operation. *)
        let forgetting_ident =
          if not is_assignment then
            avail_before
          else
            RD.Set.map (fun reg ->
                match RD.debug_info reg with
                | None -> reg
                | Some debug_info ->
                  if Ident.same (RD.Debug_info.holds_value_of debug_info) ident
                  then RD.clear_debug_info reg
                  else reg)
              avail_before
        in
        let avail_after = ref forgetting_ident in
        let num_parts_of_value = Array.length instr.arg in
        (* Add debug info about [ident], but only for registers that are known
           to be available. *)
        for part_of_value = 0 to num_parts_of_value - 1 do
          let reg = instr.arg.(part_of_value) in
          if RD.Set.mem_reg forgetting_ident reg then begin
            let regd =
              RD.create ~reg
                ~holds_value_of:ident
                ~part_of_value
                ~num_parts_of_value
                ~which_parameter
                ~provenance
            in
            avail_after := RD.Set.add regd (RD.Set.filter_reg !avail_after reg)
          end
        done;
        Ok !avail_after
      | Iop (Imove | Ireload | Ispill) ->
        (* Moves are special: they enable us to propagate names.
           No-op moves need to be handled specially---in this case, we may
           learn that a given hard register holds the value of multiple
           pseudoregisters (all of which have the same value).  This makes us
           match up properly with [Liveness]. *)
        let move_to_same_location =
          let move_to_same_location = ref true in
          for i = 0 to Array.length instr.arg - 1 do
            let arg = instr.arg.(i) in
            let res = instr.res.(i) in
            (* Note that the register classes must be the same, so we don't
                need to check that. *)
            if arg.loc <> res.loc then begin
              move_to_same_location := false
            end
          done;
          !move_to_same_location
        in
        let made_unavailable =
          if move_to_same_location then
            RD.Set.empty
          else
            RD.Set.made_unavailable_by_clobber avail_before
              ~regs_clobbered:instr.res
              ~register_class:Proc.register_class
        in
        let results =
          assert (Array.length instr.arg = Array.length instr.res);
          Array.mapi (fun index result_reg ->
              let arg_reg = instr.arg.(index) in
              match RD.Set.find_reg_exn avail_before arg_reg with
              | exception Not_found ->
                RD.create_without_debug_info ~reg:result_reg
              | arg_reg ->
                RD.create_copying_debug_info ~reg:result_reg
                  ~debug_info_from:arg_reg)
            instr.res
        in
        Ok (RD.Set.union (RD.Set.diff avail_before made_unavailable)
          (RD.Set.of_array results))
      | Iop op ->
        if M.operation_can_raise op then begin
          augment_availability_at_raise (Ok avail_before)
        end;
        (* We split the calculation of registers that become unavailable after
           a call into two parts.  First: anything that the target marks as
           destroyed by the operation, combined with any registers that will
           be clobbered by the operation writing out its results. *)
        let made_unavailable_1 =
            let regs_clobbered =
              Array.append (Proc.destroyed_at_oper instr.desc) instr.res
            in
            RD.Set.made_unavailable_by_clobber avail_before ~regs_clobbered
              ~register_class:Proc.register_class
        in
        (* Second: the case of OCaml to OCaml function calls.  In this case,
           registers always become unavailable unless:
           (a) they are "live across" the call; and/or
           (b) they hold immediates and are assigned to the stack. *)
        (* CR-someday mshinwell: Consider factoring this out from here and
           [Available_ranges.Make_ranges.end_pos_offset]. *)
        let made_unavailable_2 =
          match op with
          | Icall_ind _ | Icall_imm _ ->
            RD.Set.filter (fun reg ->
                let holds_immediate = RD.holds_non_pointer reg in
                let on_stack = RD.assigned_to_stack reg in
                let live_across = Reg.Set.mem (RD.reg reg) instr.live in
                let remains_available =
                  live_across
                    || (holds_immediate && on_stack)
                in
                not remains_available)
              avail_before
          | _ -> RD.Set.empty
        in
        let made_unavailable =
          RD.Set.union made_unavailable_1 made_unavailable_2
        in
        (* Subtract out the registers made unavailable; then add in the
           registers that are made *available* -- namely the results of the
           operation. *)
        Ok (RD.Set.union
          (RD.Set.without_debug_info (Reg.set_of_array instr.res))
          (RD.Set.diff avail_before made_unavailable))
      | Iifthenelse (_, ifso, ifnot) -> join [ifso; ifnot] ~avail_before
      | Iswitch (_, cases) -> join (Array.to_list cases) ~avail_before
      | Iloop body ->
        let avail_after = ref (RAS.Ok avail_before) in
        (* XCR pchambart: This should probably be instr.available_before to do a
           single loop in case of nested Iloop.
           mshinwell: I _think_ this is fixed, but not completely sure what
           was being referred to.  Please check. *)
        begin try
          while true do
            (* XCR pchambart: I don't see any situations where a second
               round would remove more registers from the set. Hence
               I'm not convinced that there are situations where a
               fixpoint is required. But this is necessary for marking
               each instruction in the loop with its real
               available_before. An assertion should check that never
               more than 2 rounds are required.
               mshinwell: I suspect this isn't the case with multiple
               handlers.  (This Iloop case should go away soon.) *)
            let avail_after' =
              RAS.inter !avail_after
                (available_regs body ~avail_before:!avail_after)
            in
            if RAS.equal !avail_after avail_after' then begin
              raise Exit
              end;
            avail_after := avail_after'
          done
        with Exit -> ()
        end;
        Unreachable
      | Icatch (recursive, handlers, body) ->
        List.iter (fun (nfail, _handler) ->
            (* In case there are nested [Icatch] expressions with the same
               handler numbers, we rely on the [Hashtbl] shadowing
               semantics. *)
            Hashtbl.add avail_at_exit nfail RAS.Unreachable)
          handlers;
        let avail_after_body =
          available_regs body ~avail_before:(RAS.Ok avail_before)
        in
        let aux (nfail, handler) (nfail', avail_at_top_of_handler) =
          assert (nfail = nfail');
          (* This [available_regs] call may cause [avail_at_exit] to be
             updated. *)
          available_regs handler ~avail_before:avail_at_top_of_handler
        in
        let aux_equal (nfail, avail_before_handler)
              (nfail', avail_before_handler') =
          assert (nfail = nfail');
          RAS.equal avail_before_handler avail_before_handler'
        in
        let rec fixpoint avail_at_top_of_handlers =
          let avail_after_handlers =
            List.map2 aux handlers avail_at_top_of_handlers
          in
          let avail_at_top_of_handlers' =
            List.map (fun (nfail, _handler) ->
                match Hashtbl.find avail_at_exit nfail with
                | exception Not_found -> assert false  (* see above *)
                | avail_at_top_of_handler -> nfail, avail_at_top_of_handler)
              handlers
          in
          match recursive with
          | Nonrecursive -> avail_after_handlers
          | Recursive ->
            if List.for_all2 aux_equal avail_at_top_of_handlers
              avail_at_top_of_handlers'
            then avail_after_handlers
            else fixpoint avail_at_top_of_handlers'
        in
        let init_avail_at_top_of_handlers =
          List.map (fun (nfail, _handler) ->
              match Hashtbl.find avail_at_exit nfail with
              | exception Not_found -> assert false  (* see above *)
              | avail_at_top_of_handler -> nfail, avail_at_top_of_handler)
            handlers
        in
        let avail_after_handlers = fixpoint init_avail_at_top_of_handlers in
        List.iter (fun (nfail, _handler) ->
            Hashtbl.remove avail_at_exit nfail)
          handlers;
        List.fold_left (fun avail_at_join avail_after_handler ->
            RAS.inter avail_at_join avail_after_handler)
          avail_after_body
          avail_after_handlers
      | Iexit nfail ->
        let avail_before = RAS.Ok avail_before in
        let avail_at_top_of_handler =
          match Hashtbl.find avail_at_exit nfail with
          | exception Not_found ->  (* also see top of [Icatch] clause above *)
            Misc.fatal_errorf "Iexit %d not in scope of Icatch" nfail
          | avail_at_top_of_handler -> avail_at_top_of_handler
        in
        let avail_at_top_of_handler =
          RAS.inter avail_at_top_of_handler avail_before
        in
        Hashtbl.replace avail_at_exit nfail avail_at_top_of_handler;
        Unreachable
      | Itrywith (body, handler) ->
        let saved_avail_at_raise = !avail_at_raise in
        avail_at_raise := RAS.Unreachable;
        let avail_before = RAS.Ok avail_before in
        let after_body = available_regs body ~avail_before in
        let avail_before_handler = !avail_at_raise in
        avail_at_raise := saved_avail_at_raise;
        RAS.inter after_body
          (available_regs handler ~avail_before:avail_before_handler)
      | Iraise _ ->
        let avail_before : RAS.t = Ok avail_before in
        augment_availability_at_raise avail_before;
        Unreachable
  in
  match instr.desc with
  | Iend -> avail_after
  | _ -> available_regs instr.next ~avail_before:avail_after

and join branches ~avail_before =
  let avail_before = RAS.Ok avail_before in
  let avails = List.map (available_regs ~avail_before) branches in
  begin match avails with
  | [] -> avail_before
  | avail::avails -> List.fold_left RAS.inter avail avails
  end

let fundecl (f : M.fundecl) =
  (* XCR pchambart: this should be cleaned by the Icatch instruction.
     It should be replaced by an assertion *)
  assert (Hashtbl.length avail_at_exit = 0);
  avail_at_raise := RAS.Unreachable;
  let fun_args = R.set_of_array f.fun_args in
  let avail_before = RAS.Ok (RD.Set.without_debug_info fun_args) in
  ignore ((available_regs f.fun_body ~avail_before) : RAS.t);
  f
