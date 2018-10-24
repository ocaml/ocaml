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

(** Unified interface to [Available_ranges_vars] and
    [Available_ranges_phantom_vars] for a consumer. *)

module Subrange_info : sig
  type t = private
    | Non_phantom of {
        reg : Reg.t;
        offset_from_cfa_in_bytes : int option;
      }
    | Phantom of Mach.phantom_defining_expr
end

module Range_info : sig
  type t

  val provenance : t -> Backend_var.Provenance.t option
  val is_parameter : t -> Is_parameter.t
end

include Compute_ranges.S
  with module Index := Backend_var
  with module Key := Backend_var
  with module Subrange_info := Subrange_info
  with module Range_info := Range_info

val create
   : available_ranges_vars:Available_ranges_vars.t
  -> available_ranges_phantom_vars:Available_ranges_phantom_vars.t
  -> t
