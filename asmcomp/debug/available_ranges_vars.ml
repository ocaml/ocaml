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

include Calculate_ranges.Make (struct
  module RD = Reg_with_debug_info

  (* By the time this pass has run, register stamps are irrelevant; indeed,
     there may be multiple registers with different stamps assigned to the
     same location.  As such, we quotient register sets by the equivalence
     relation that identifies two registers iff they have the same name and
     location. *)
  module Key = struct
    include RD

    module Set = RD.Set_distinguishing_names_and_locations
    module Map = RD.Map_distinguishing_names_and_locations
  end

  module Index = Backend_var

  module Subrange_state : sig
    include Compute_ranges_intf.S_subrange_state

    val stack_offset : t -> int
  end = struct
    type t = {
      stack_offset : int;
    }

    let create () =
      { stack_offset = Proc.initial_stack_offset;
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

    let stack_offset t = t.stack_offset
  end

  module Subrange_info : sig
    include Compute_ranges_intf.S_subrange_info
      with type key := Key.t
      with type subrange_state := Subrange_state.t

    val offset_from_cfa_in_bytes : t -> int
  struct
    type t = {
      reg : Reg.t;
      start_insn : L.instruction;
      offset_from_cfa_in_bytes : int option;
    }

    let create reg ~start_insn ~subrange_state =
      let reg = RD.reg reg in
      let stack_offset = Subrange_state.stack_offset subrange_state in
      let offset_from_cfa_in_bytes =
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
      { reg;
        start_insn;
        offset_from_cfa_in_bytes;
      }

    (* Available subranges are allowed to cross points at which the stack
       pointer changes, since we reference the stack slots as an offset from the
       CFA, not from the stack pointer. *)

    let offset_from_cfa_in_bytes t = t.offset_from_cfa_in_bytes
  end

  module Range_info :
    Compute_ranges_intf.S_range_info
      with type key := Key.t
      with type index := Index.t
  = struct
    type t = {
      provenance : Backend_var.Provenance.t option;
      is_parameter : Is_parameter.t;
    }

    let create _fundecl reg ~start_insn:_ =
      match RD.debug_info reg with
      | None -> None
      | Some debug_info ->
        let var = RD.Debug_info.holds_value_of debug_info in
        let provenance = RD.Debug_info.provenance debug_info in
        let is_parameter : Is_parameter.t =
          match RD.Debug_info.which_parameter debug_info with
          | None -> Local
          | Some index -> Parameter { index; }
        in
        let t =
          { provenance;
            is_parameter;
          }
        in
        Some (var, t)

    let provenance t = t.provenance
    let is_parameter t = t.is_parameter
  end

  let availability_set_to_key_set avail =
    match avail with
    | Unreachable -> Key.Set.empty
    | Ok available_before ->
      RD.Set.fold (fun reg result -> Key.Set.add reg result)
        available_before
        Key.Set.empty

  let available_before (insn : L.instruction) =
    availability_set_to_key_set insn.available_before

  let available_across (insn : L.instruction) =
    availability_set_to_key_set insn.available_across

  let must_restart_ranges_upon_any_change () =
    match !Clflags.debug_full with
    | Some Gdb -> false
    | Some Lldb ->
      (* Work at OCamlPro suggested that lldb requires ranges to be
         completely restarted in the event of any change. *)
      true
    | None -> Misc.fatal_error "Shouldn't be here without [debug_full]"
end)
