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

include Compute_ranges.Make (struct
  module Key = Debuginfo.Block
  module Index = Debuginfo.Block

  module Available_subrange_state_for_blocks : Subrange_state_intf = struct
    type t = unit

    let create () = ()

    let advance_over_instruction () _insn = ()
  end

  module Available_subrange_info_for_blocks
    : Subrange_info_intf
      with type subrange_state = Available_subrange_state_for_blocks.t =
  struct
    type t = unit

    let create ~subrange_state:_ = ()
  end

  module Available_range_info_for_blocks = struct
    type t = unit
  end

  let available_before (insn : L.instruction) =
    match Debuginfo.innermost_block insn.dbg with
    | None -> Debuginfo.Block.Set.empty
    | Some block -> Debuginfo.Block.Set.singleton block

  let range_continues_for_this_many_bytes ~current_insn:_ _key = None

  let maybe_restart_ranges ~proto_births:_ ~proto_deaths:_ = false
end)
