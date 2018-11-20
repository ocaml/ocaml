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

module Int = Numbers.Int
module L = Linearize

module Lexical_blocks = struct
  module Key = Debuginfo.Block
  module Index = Debuginfo.Block

  module Subrange_state :
    Compute_ranges_intf.S_subrange_state
  = struct
    type t = unit

    let create () = ()
    let advance_over_instruction () _ = ()
  end

  module Subrange_info :
    Compute_ranges_intf.S_subrange_info
      with type key := Key.t
      with type subrange_state := Subrange_state.t
  = struct
    type t = unit

    let create _var _subrange_state = ()
  end

  module Range_info :
    Compute_ranges_intf.S_range_info
      with type key := Key.t
      with type index := Index.t
  = struct
    type t = unit

    let create _fundecl block ~start_insn:_ = Some (block, ())
  end

  module Cache = struct
    type t = Debuginfo.Block.Set.t Int.Tbl.t

    let create () = Int.Tbl.create 1000
  end

  let available_before cache (insn : L.instruction) =
    let innermost = Debuginfo.innermost_block insn.dbg in
    match Debuginfo.Current_block.to_block innermost with
    | Toplevel -> Debuginfo.Block.Set.empty
    | Block block ->
      let block_id = Debuginfo.Block.unique_id block in
      match Int.Tbl.find cache block_id with
      | exception Not_found ->
        let available_before = Debuginfo.Block.block_and_all_parents block in
        Int.Tbl.replace cache block_id available_before;
        available_before
      | available_before -> available_before

  let available_across cache insn =
    (* Block scoping never changes during the execution of a [Linearize]
       instruction. *)
    available_before cache insn

  let must_restart_ranges_upon_any_change () = false
end

module Subrange_state = Lexical_blocks.Subrange_state
module Subrange_info = Lexical_blocks.Subrange_info
module Range_info = Lexical_blocks.Range_info

include Compute_ranges.Make (Lexical_blocks)
