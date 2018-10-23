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

module Subrange_info : sig
  type t
end

module Range_info : sig
  type t
end

include Compute_ranges.S
  with type subrange_info := Subrange_info.t
  with type range_info := Range_info.t
  with type index := Debuginfo.Block.t
