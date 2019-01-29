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
module V = Backend_var

module Phantom_vars = struct
  module Key = struct
    include V
    let all_parents _t = []
  end

  module Index = V

  module Subrange_state :
    Compute_ranges_intf.S_subrange_state
  = struct
    type t = unit

    let create () = ()
    let advance_over_instruction _ _ = ()
  end

  module Subrange_info :
    Compute_ranges_intf.S_subrange_info
      with type key := Key.t
      with type subrange_state := Subrange_state.t
  = struct
    type t = unit

    let create _var _subrange_state = ()
  end

  module Range_info : sig
    include Compute_ranges_intf.S_range_info
      with type key := Key.t
      with type index := Index.t

    val provenance : t -> V.Provenance.t option
    val is_parameter : t -> Is_parameter.t
    val defining_expr : t -> Mach.phantom_defining_expr
  end = struct
    type t = {
      provenance : V.Provenance.t option;
      is_parameter : Is_parameter.t;
      defining_expr : Mach.phantom_defining_expr;
    }

    let create (fundecl : L.fundecl) var ~start_insn:_ =
      match V.Map.find var fundecl.fun_phantom_lets with
      | exception Not_found ->
        Misc.fatal_errorf "Available_ranges_phantom_vars.Range_info.create: \
            phantom variable occurs in [phantom_available_before] but not in \
            [fun_phantom_lets]: %a"
          V.print var
      | provenance, defining_expr ->
        (* Static phantom variables are sent via a different path (see
           [Available_ranges_all_vars]) since they are always available. *)
        match provenance with
        | Some provenance when V.Provenance.is_static provenance -> None
        | _ ->
          (* CR mshinwell: We need to explicitly clarify that this
             [is_parameter] relates to the *current function* only and not
             to inlined functions (which the [is_parameter] function on
             [Backend_var.Provenance returns). *)
          let is_parameter = Is_parameter.local in
          let t =
            { provenance;
              is_parameter;
              defining_expr;
            }
          in
          Some (var, t)

    let provenance t = t.provenance
    let is_parameter t = t.is_parameter
    let defining_expr t = t.defining_expr
  end

  let available_before (insn : L.instruction) =
    Insn_debuginfo.phantom_available_before insn.dbg

  let available_across insn =
    (* Phantom variable availability never changes during the execution
       of a [Linearize] instruction. *)
    available_before insn

  let must_restart_ranges_upon_any_change () =
    (* See [Available_ranges_vars]. *)
    false
end

module Subrange_state = Phantom_vars.Subrange_state
module Subrange_info = Phantom_vars.Subrange_info
module Range_info = Phantom_vars.Range_info

include Compute_ranges.Make (Phantom_vars)
