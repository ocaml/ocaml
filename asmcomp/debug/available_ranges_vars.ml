(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module L = Linearize
module V = Backend_var

module Vars = struct
  module RD = Reg_with_debug_info

  (* By the time this pass runs, register stamps are irrelevant; indeed,
     there may be multiple registers with different stamps assigned to the
     same location.  As such, we quotient register sets by the equivalence
     relation that identifies two registers iff they have the same name and
     location. *)
  module Key = struct
    include RD.For_compute_ranges

    let all_parents _t = []
  end

  module Index = V

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

    val reg : t -> Reg.t
    val offset_from_cfa_in_bytes : t -> int option
  end = struct
    type t = {
      reg : Reg.t;
      offset_from_cfa_in_bytes : int option;
    }

    let create reg subrange_state =
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
        offset_from_cfa_in_bytes;
      }

    let reg t = t.reg

    (* Available subranges are allowed to cross points at which the stack
       pointer changes, since we reference the stack slots as an offset from the
       CFA, not from the stack pointer. *)

    let offset_from_cfa_in_bytes t = t.offset_from_cfa_in_bytes
  end

  module Range_info : sig
    include Compute_ranges_intf.S_range_info
      with type key := Key.t
      with type index := Index.t

    val provenance : t -> V.Provenance.t option
    val is_parameter : t -> Is_parameter.t
  end = struct
    type t = {
      provenance : V.Provenance.t option;
      is_parameter : Is_parameter.t;
    }

    let create _fundecl reg ~start_insn:_ =
      match RD.debug_info reg with
      | None -> None
      | Some debug_info ->
        match RD.Debug_info.holds_value_of debug_info with
        | Const_int _ | Const_naked_float _ | Const_symbol _ -> None
        | Var var ->
          let provenance = RD.Debug_info.provenance debug_info in
  (*
          Format.eprintf "Reg being created with var %a, provenance %a\n%!"
            V.print var
            (Misc.Stdlib.Option.print V.Provenance.print) provenance;
  *)
          let is_parameter = RD.Debug_info.is_parameter debug_info in
          let t =
            { provenance;
              is_parameter;
            }
          in
          Some (var, t)

    let provenance t = t.provenance

    let is_parameter t = t.is_parameter
  end

  (* CR mshinwell: update comment to explain what "subset inclusion" means
     here *)
  (* Important note: [Reg_availability_set.canonicalise] does not preserve
     subset inclusion.  This means in particular that a canonicalised
     [available_across] set may not be a subset of the corresponding
     canonicalised [available_before].  [Compute_ranges] can cope with
     this. *)

  let availability_set_to_key_set (avail : Reg_availability_set.t) =
    Reg_availability_set.canonicalise avail

  let available_before (insn : L.instruction) =
    availability_set_to_key_set (Insn_debuginfo.available_before insn.dbg)

  let available_across (insn : L.instruction) =
    match Insn_debuginfo.available_across insn.dbg with
    | None -> available_before insn
    | Some across -> availability_set_to_key_set across

  let must_restart_ranges_upon_any_change () =
    (* Work at OCamlPro suggested that lldb requires ranges to be
       completely restarted in the event of any change.  We may wish to add
       code here in the future if that hypothesis is validated. *)
    false
end

module Subrange_state = Vars.Subrange_state
module Subrange_info = Vars.Subrange_info
module Range_info = Vars.Range_info

include Compute_ranges.Make (Vars)
