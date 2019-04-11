(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell: Update to talk about set / map distinction. *)

(** Register availability sets. This module is just a version of
    [Reg_with_debug_info.Availability_map] whose type is lifted to have an
    additional top element. This element corresponds to availability information
    known at any unreachable point in the generated code. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The type of register availability sets. *)
type t = private
  | Unreachable
  | Ok of Reg_with_debug_info.Availability_map.t

(** The top element. *)
val unreachable : t

(** Create a register availability set (that is not the top element). *)
val create : Reg_with_debug_info.Availability_map.t -> t

(** Canonicalise the given register availability map.  The result can be
    used for [Compute_ranges], etc.  See the documentation for
    [Reg_with_debug_info.Canonical_availability_map] with regard to the
    definition of "canonical". *)
val canonicalise : t -> Reg_with_debug_info.Canonical_availability_map.t

(** The functions below are lifted versions of the corresponding ones in
    [Reg_with_debug_info.Availability_map]. *)

(** Print a value of type [t] to a formatter.
    [print_reg] should be [Printmach.reg]. *)
val print
   : print_reg:(Format.formatter -> Reg.t -> unit)
  -> Format.formatter
  -> t
  -> unit

(** Test for equality. *)
val equal : t -> t -> bool

(** The empty set. *)
val empty : t

(** Set disjoint union.  "Disjoint" refers to the sets of [Reg.t] values
    rather than the associated debugging information. *)
val disjoint_union : t -> t -> t

(** Set intersection. *)
val inter : t -> t -> t

(** Non-strict subset inclusion. *)
val subset : t -> t -> bool

(** Map the [Reg_with_debug_info.Availability_map.t] value contained within some
    values of type [t]. *)
val map
   : t
  -> f:(Reg_with_debug_info.Availability_map.t
    -> Reg_with_debug_info.Availability_map.t)
  -> t

(** Find the debug info component of an element of the set given the underlying
    [Reg.t]. This function returns [None] if the supplied [t] is [Unreachable],
    if the given register does not occur in the supplied [t], or if the
    given register does occur but is not associated with any debug info. *)
val find_debug_info : t -> Reg.t -> Reg_with_debug_info.Debug_info.t option

(** [made_unavailable_by_clobber t ~regs_clobbered ~register_class] returns
    the largest subset of [t] whose locations do not overlap with any
    registers in [regs_clobbered].  (Think of [t] as a set of available
    registers.)
    [register_class] should always be [Proc.register_class]. *)
val made_unavailable_by_clobber
   : t
  -> regs_clobbered:Reg.t array
  -> register_class:(Reg.t -> int)
  -> t
