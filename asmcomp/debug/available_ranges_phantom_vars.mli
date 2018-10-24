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
