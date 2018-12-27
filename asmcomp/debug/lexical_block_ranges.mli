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

(** Possibly-discontiguous ranges of code representing lexical blocks.  These
    are calculated in the same way as we calculate available ranges for
    variables (see [Available_ranges_vars]).
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module L = Linearize

module Subrange_state : sig
  type t

  val create : unit -> t
  val advance_over_instruction : t -> Linearize.instruction -> t
end

module Subrange_info : sig
  type t

  val create : Debuginfo.Block.t -> Subrange_state.t -> t
end

module Range_info : sig
  type t

  val create
     : Linearize.fundecl
    -> Debuginfo.Block.t
    -> start_insn:Linearize.instruction
    -> (Debuginfo.Block.t * t) option
end

include Compute_ranges_intf.S
  with module Index := Debuginfo.Block
  with module Key := Debuginfo.Block
  with module Subrange_state := Subrange_state
  with module Subrange_info := Subrange_info
  with module Range_info := Range_info

val range_covering_whole_function : t -> Range.t
