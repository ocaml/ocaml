(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module L = Linearize

module Subrange_state : sig
  include Subrange_state_intf

  val reg : t -> Reg.t

  val stack_offset : t -> int
end = struct
  type t = {
    reg : Reg.t;
    stack_offset : int;
  }

  let create ... =
    { reg = ...;
      stack_offset = Proc.initial_stack_offset;
    }

  let advance_over_instruction t (insn : L.instruction) =
    let stack_offset =
      match insn.desc with
      | Lop (Istackoffset delta) -> t.stack_offset + delta
      | Lpushtrap -> t.stack_offset + Proc.trap_frame_size_in_bytes
      | Lpoptrap -> t.stack_offset - Proc.trap_frame_size_in_bytes
      | Lend | Lprologue | Lop _ | Lreloadretaddr | Lreturn
      | Llabel _ | Lbranch _ | Lcondbranch _ | Lcondbranch3 _
      | Lswitch _ | Lsetuptrap _ | Lraise _ -> t.stack_offset
    in
    { stack_offset;
    }
end

module Subrange_info : sig
  include Subrange_info_intf
    with type subrange_state = Subrange_state.t

  val offset_from_stack_ptr_in_bytes : t -> int
struct
  type t = {
    reg : Reg.t;
    start_insn : L.instruction;
    offset_from_stack_ptr_in_bytes : int option;
  }

  type type_info =
    | From_cmt_file of Backend_var.Provenance.t option
    | Phantom of
        Backend_var.Provenance.t option * Mach.phantom_defining_expr option

  type is_parameter =
    | Local
    | Parameter of { index : int; }

  let create ~reg (* XXX *) ~start_insn ~subrange_state =
    let reg = Subrange_state.??? in
    let offset_from_stack_ptr_in_bytes =
      match reg.loc with
      | Stack loc ->
        let frame_size = Proc.frame_size ~stack_offset in
        let slot_offset =
          Proc.slot_offset loc ~reg_class:(Proc.register_class reg)
            ~stack_offset
        in
        Some (frame_size - slot_offset)
      | Reg _ | Unknown -> None
    in
    { reg : reg;
      start_insn : start_insn;
      offset_from_stack_ptr_in_bytes;
    }

  let type_info t =
    ...

  let is_parameter t =
    ...

  let offset_from_stack_ptr_in_bytes t =
    match t.offset_from_stack_ptr_in_bytes with
    | Some offset -> offset
    | None ->
      Misc.fatal_error "No offset from stack pointer available (register \
        not assigned to the stack)"
end

module Range_info = struct
  type t =

  type 'a location =
    | Reg of Reg.t * 'a
    | Phantom

  let lexical_scope t =

  let location t : unit location =
    let convert_location (start_insn : start_insn_or_phantom)
        : unit location =
      match start_insn with
      | Reg (reg, _insn) -> Reg (reg, ())
      | Phantom -> Phantom
    in
    convert_location t.start_insn

  let offset_from_stack_ptr_in_bytes t =

end

module Regs = Calculate_ranges.Make (struct
  module RD = Reg_with_debug_info

  (* By the time this pass has run, register stamps are irrelevant; indeed,
     there may be multiple registers with different stamps assigned to the
     same location.  As such, we quotient register sets by the equivalence
     relation that varifies two registers iff they have the same name and
     location. *)
  module Key = struct
    type t = RD.t

    (* CR mshinwell: check this *)
    let assert_valid (_t : t) = ()
     (*  assert (t.name <> None);  (* cf. [Filtering] *) *)

    module Map = RD.Map_distinguishing_names_and_locations
    module Set = RD.Set_distinguishing_names_and_locations
  end

  (* CR-soon mshinwell: pull this type forward so other passes can use it *)
  type is_parameter =
    | Local
    | Parameter of { index : int; }

  (* CR mshinwell: improve efficiency *)
  let available_before (insn : L.instruction) =
    match insn.available_before with
    | Unreachable -> Key.Set.empty
    | Ok available_before -> Key.Set.of_list (RD.Set.elements available_before)

  let end_pos_offset ~prev_insn ~key:reg =
    (* If the range is for a register destroyed by a call and which
       ends immediately after a call instruction, move the end of the
       range back very slightly.  The effect is that the register is seen
       in the debugger as available when standing on the call instruction
       but unavailable when we are in the callee (and move to the previous
       frame). *)
    (* CR-someday mshinwell: I wonder if this should be more
       conservative for Iextcall.  If the C callee is compiled with
       debug info then it should describe where any callee-save
       registers have been saved, so when we step back to the OCaml frame
       in the debugger, the unwinding procedure should get register
       values correct.  (I think.)  However if it weren't compiled with
       debug info, this wouldn't happen, and when looking back up into
       the OCaml frame I suspect registers would be wrong.  This may
       not be a great problem once libmonda is hardened, although it
       is possible for this to be subtle and misleading (e.g. an integer
       value being 1 instead of 2 or something.) *)
    match prev_insn with
    | None -> None
    | Some prev_insn ->
      match prev_insn.L.desc with
      | Lop ((Icall_ind _ | Icall_imm _ | Iextcall _) as op) ->
        let destroyed_locations =
          Array.map (fun (reg : Reg.t) -> reg.loc)
            (Proc.destroyed_at_oper (Iop op))
        in
        let holds_immediate = RD.holds_non_pointer reg in
        let on_stack = RD.assigned_to_stack reg in
        let live_across = Reg.Set.mem (RD.reg reg) prev_insn.L.live in
        let remains_available =
          live_across
            || (holds_immediate && on_stack)
        in
        if Array.mem (RD.location reg) destroyed_locations
            || not remains_available
        then
          Some (-1)
        else
          None
      | _ -> None

  let range_info ~fundecl:_ ~key:reg ~start_insn:_ =
    match RD.debug_info reg with
    | None -> None
    | Some debug_info ->
      let is_parameter =
        match RD.Debug_info.which_parameter debug_info with
        | None -> Local
        | Some index -> Parameter { index; }
      in
      let var = RD.Debug_info.holds_value_of debug_info in
      let provenance = RD.Debug_info.provenance debug_info in
      let type_info = From_cmt_file provenance in
      (* CR mshinwell: naming etc. *)
      let var_location =
        match provenance with
        | None -> Debuginfo.none
        | Some provenance -> Backend_var.Provenance.location provenance
      in
      let range_info =
        { type_info;
          is_parameter;
          var_location;
        }
      in
      Some (var, range_info)

  let create_subrange ~fundecl:_ ~key:reg ~start_pos ~start_insn ~end_pos
        ~end_pos_offset ~subrange_state =
    Subrange.create ~reg:(RD.reg reg)
      ~start_pos ~start_insn
      ~end_pos ~end_pos_offset
      ~subrange_state
end)

include Regs
