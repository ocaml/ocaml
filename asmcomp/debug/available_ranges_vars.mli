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

module Subrange_info : sig
  type t

  (** [offset_from_stack_ptr_in_bytes] must be called only when the
      corresponding register is assigned to the stack. *)
  val offset_from_stack_ptr_in_bytes : t -> int
end

module Range_info : sig
  type is_parameter = private
    | Local
    | Parameter of { index : int; }

  type t

  val provenance : t -> Backend_var.Provenance.t option
  val debuginfo : t -> Debuginfo.t
  val is_parameter : t -> is_parameter
end

include Compute_ranges.S
  with module Index := Backend_var
  with module Key := Backend_var
  with module Subrange_info := Subrange_info
  with module Range_info := Range_info
