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
  module Key = Backend_var
  module Index = Backend_var

  module Subrange_state :
    Compute_ranges_intf.S_subrange_state
  = struct
    type t = unit

    let create () = ()
  end

  module Subrange_info :
    Compute_ranges_intf.S_subrange_info
      with type key := Key.t
      with type subrange_state := Subrange_state.t
  = struct
    type t = unit

    let create _var ~start_insn:_ ~subrange_state:_ = ()
  end

  module Range_info :
    Compute_ranges_intf.S_range_info
      with type key := Key.t
      with type index := Index.t
  = struct
    type is_parameter =
      | Local
      | Parameter of { index : int; }

    type t = {
      provenance : Backend_var.Provenance.t option;
      is_parameter : is_parameter;
      defining_expr : Mach.phantom_defining_expr;
    }

    let create (fundecl : L.fundecl) var ~start_insn:_ =
      match Backend_var.Map.find var fundecl.fun_phantom_lets with
      | exception Not_found ->
        Misc.fatal_errorf "Available_ranges_phantom_vars.Range_info.create: \
            phantom variable occurs in [phantom_available_before] but not in \
            [fun_phantom_lets]: %a"
          Backend_var.print var
      | provenance, defining_expr ->
        (* CR-someday mshinwell: [Local] should presumably change to
           [Parameter] when we emit DWARF inlined function information. *)
        let t =
          { provenance;
            is_parameter = Local;
            defining_expr;
          }
        in
        Some (var, t)
  end

  let available_before (insn : L.instruction) = insn.phantom_available_before

  let available_across insn =
    (* Phantom variable availability never changes during the execution
       of a [Linearize] instruction. *)
    available_before insn

  let must_restart_ranges_upon_any_change () =
    (* See [Available_ranges_vars]. *)
    match !Clflags.debug_full with
    | Some Gdb -> false
    | Some Lldb -> true
    | None -> Misc.fatal_error "Shouldn't be here without [debug_full]"
end)
