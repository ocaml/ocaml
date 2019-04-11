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

(** Given a variable [x] and a function, an "available subrange" is in the
    normal case a contiguous subset of that function's code paired with a
    register [r], such that at all times during the block's execution the value
    of [x] is available in [r]. ([r] may end up being a hard register or a
    location on the stack.)
 
    Available subranges associated with non-phantom variables are computed by
    this pass based on the information from the dataflow analysis in
    [Regs]. (The linearized code is updated so that it contains the
    necessary labels for delimiting such ranges.)
 
    An "available range" is then a set of available subranges that do not
    overlap in code space, again for a single variable and function.
*)

module Subrange_state : sig
  type t

  val create : unit -> t
  val advance_over_instruction : t -> Linearize.instruction -> t
end

module Subrange_info : sig
  type t

  val create : Reg_with_debug_info.t -> Subrange_state.t -> t
  val reg : t -> Reg.t
  val offset_from_cfa_in_bytes : t -> int option
end

module Range_info : sig
  type t

  val create
     : Linearize.fundecl
    -> Reg_with_debug_info.t
    -> start_insn:Linearize.instruction
    -> (Backend_var.t * t) option

  val provenance : t -> Backend_var.Provenance.t option
  val is_parameter : t -> Is_parameter.t
end

include Compute_ranges_intf.S
  with module Index := Backend_var
  with module Key := Reg_with_debug_info.For_compute_ranges
  with module Subrange_state := Subrange_state
  with module Subrange_info := Subrange_info
  with module Range_info := Range_info
