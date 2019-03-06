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

(** Coalescing of per-instruction information into possibly-discontiguous
    regions of code delimited by labels.  This is used for collating
    register availability and lexical block scoping information into a
    concise form. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (S : Compute_ranges_intf.S_functor)
  : Compute_ranges_intf.S
      with module Index := S.Index
      with module Key := S.Key
      with module Subrange_state := S.Subrange_state
      with module Subrange_info := S.Subrange_info
      with module Range_info := S.Range_info
