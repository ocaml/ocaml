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

module Subrange : sig
  type t

  val info : t -> Subrange_info.t

  val start_pos : t -> Linearize.label
  val start_pos_offset : t -> int
  val end_pos : t -> Linearize.label
  val end_pos_offset : t -> int
end

module Range_info : sig
  type t

  val provenance : t -> Backend_var.Provenance.t option
  val debuginfo : t -> Debuginfo.t
  val is_parameter : t -> Is_parameter.t

  type phantom_defining_expr = private
    | Non_phantom
    | Phantom of Mach.phantom_defining_expr

  val phantom_defining_expr : t -> phantom_defining_expr
end

module Range : sig
  type t

  val info : t -> Range_info.t

  val extremities : t -> (Linearize.label * Linearize.label) option

  val fold
     : t
    -> init:'a
    -> f:('a -> Subrange.t -> 'a)
    -> 'a
end

type t

val empty : t

val create
   : available_ranges_vars:Available_ranges_vars.t
  -> available_ranges_phantom_vars:Available_ranges_phantom_vars.t
  -> Linearize.fundecl
  -> t

val iter : t -> f:(Backend_var.t -> Range.t -> unit) -> unit

val fold : t -> init:'a -> f:('a -> Backend_var.t -> Range.t -> 'a) -> 'a
