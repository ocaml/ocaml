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

module Subrange_info : sig
  type t

  type type_info = private
    | Phantom of
        Backend_var.Provenance.t option * Mach.phantom_defining_expr option

  type is_parameter = private
    | Local
    | Parameter of { index : int; }

  val type_info : t -> type_info
  val is_parameter : t -> is_parameter
end

module Range_info : sig
  type t

  val debuginfo : t -> Debuginfo.t

  val lexical_scope : t -> Debuginfo.Block.t option
end

include Compute_ranges.S
  with type subrange_info := Subrange_info.t
  with type range_info := Range_info.t
  with type index := Backend_var.t
