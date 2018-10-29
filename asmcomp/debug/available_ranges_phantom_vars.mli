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

(** Like [Available_ranges_vars], but with phantom variables associated to
    subranges, rather than normal (non-phantom) variables.

    Phantom variables correspond to variables that once bound computations that
    have now been optimised out.
*)

module Subrange_state : sig
  type t

  val create : unit -> t
  val advance_over_instruction : t -> Linearize.instruction -> t
end

module Subrange_info : sig
  type t

  val create : Backend_var.t -> Subrange_state.t -> t
end

module Range_info : sig
  type t

  val create
     : Linearize.fundecl
    -> Backend_var.t
    -> start_insn:Linearize.instruction
    -> (Backend_var.t * t) option

  val provenance : t -> Backend_var.Provenance.t option
  val is_parameter : t -> Is_parameter.t
  val defining_expr : t -> Mach.phantom_defining_expr
end

include Compute_ranges_intf.S
  with module Index := Backend_var
  with module Key := Backend_var
  with module Subrange_state := Subrange_state
  with module Subrange_info := Subrange_info
  with module Range_info := Range_info
