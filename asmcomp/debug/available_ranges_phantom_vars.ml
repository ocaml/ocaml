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
  type t =

  let create ... =

  let advance_over_instruction t (insn : L.instruction) =
end

module Subrange_info : sig
  include Subrange_info_intf
    with type subrange_state = Subrange_state.t

struct
  type t =

(* For subrange creation: *)
    let create_phantom ~start_pos ~end_pos =
      { start_insn = Phantom;
        start_pos;
        end_pos;
        end_pos_offset = None;
        offset_from_stack_ptr_in_bytes = None;
      }
end

module Range_info = struct
  type t =

end

module Phantom_vars0 = Calculate_ranges.Make (struct
  module Key = struct
    include Ident

    let assert_valid _t = ()
  end

  let available_before (insn : L.instruction) = insn.phantom_available_before

  let end_pos_offset ~prev_insn:_ ~key:_ = None

  let range_info ~(fundecl : L.fundecl) ~key ~start_insn:_ =
    match Backend_var.Map.find key fundecl.fun_phantom_lets with
    | exception Not_found ->
      Misc.fatal_errorf "Available_ranges.Make_phantom_ranges.create_range: \
          phantom variable occurs in [phantom_available_before] but not in \
          [fun_phantom_lets]: %a"
        Key.print key
    | provenance, defining_expr ->
      (* CR-someday mshinwell: Presumably the "Local" only changes to indicate
         a parameter when we can represent the inlined frames properly in
         DWARF.  (Otherwise the function into which an inlining occurs ends up
         having more parameters than it should in the debugger.)
      *)
      let is_parameter = Local in
      let type_info = Phantom (provenance, Some defining_expr) in
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

  let create_subrange ~fundecl:_ ~key:_ ~start_pos ~start_insn:_ ~end_pos
        ~end_pos_offset:_ ~stack_offset:_ =
    (* Ranges for phantom variables are emitted as contiguous blocks
       which are designed to approximately indicate their scope.
       Some such phantom variables' values may ultimately be derived
       from the values of normal variables (e.g. "Read_var_field") and
       thus will be unavailable when those normal variables are
       unavailable.  This effective intersecting of available ranges
       is handled automatically in the debugger since we emit DWARF that
       explains properly how the phantom variables relate to other
       (normal or phantom) ones. *)
    Available_subrange.create_phantom ~start_pos ~end_pos
end)

include Phantom_vars0

let post_process t =
  (* It is unfortunately the case that variables can be named in the defining
     expressions of phantom ranges without actually having any available range
     themselves.  This might be caused by, for example, a "name for debugger"
     operation on a register assigned to %rax immediately before an allocation
     on x86-64 (which clobbers %rax).  The register is explicitly removed from
     the availability sets by [Available_regs], and the name never appears on
     any available range.
     To prevent this situation from causing problems later on, we add empty
     ranges for any such variables.  We assume such variables are local
     variables. *)
  let variables_without_ranges =
    fold t ~init:[]
      ~f:(fun acc ~var:_ ~name_is_unique:_ ~location_is_unique:_ ~range ->
        match Available_range.type_info range with
        | From_cmt_file _ -> acc
        | Phantom (_, defining_expr) ->
          let vars =
            match defining_expr with
            | None -> []
            | Some defining_expr ->
              match defining_expr with
              | Iphantom_const_int _
              | Iphantom_const_symbol _
              | Iphantom_read_symbol_field _ -> []
              | Iphantom_var var
              | Iphantom_read_field { var; _ }
              | Iphantom_offset_var { var; _ } -> [var]
              | Iphantom_block { fields; _ } ->
                Misc.Stdlib.List.filter_map (fun field -> field) fields
          in
          let without_ranges =
            List.filter (fun var -> not (Backend_var.Tbl.mem t.ranges var))
              vars
          in
          acc @ without_ranges)
  in
  List.iter (fun var ->
      let range =
        Available_range.create ~type_info:(Phantom (None, None))
          ~is_parameter:Local
      in
      add ranges var range)
    variables_without_ranges

let create fundecl =
  let t, fundecl = create fundecl in
  post_process t;
  t, fundecl
