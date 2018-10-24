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

module ARV = Available_ranges_vars
module ARPV = Available_ranges_phantom_vars

module Subrange_info : sig
  type t =
    | Non_phantom of {
        reg : Reg.t;
        offset_from_cfa_in_bytes : int option;
      }
    | Phantom of Mach.phantom_defining_expr
end

module Range_info : sig
  type t = {
    provenance : Backend_var.Provenance.t option;
    is_parameter : Is_parameter.t;
  }

  let provenance t = t.provenance
  let is_parameter t = t.is_parameter
end

type t = {
  available_ranges_vars : Available_ranges_vars.t;
  available_ranges_phantom_vars : Available_ranges_phantom_vars.t;
}

module Subrange : sig
  type t

  val info : t -> subrange_info

  val start_pos : t -> Linearize.label
  val end_pos : t -> Linearize.label
  val end_pos_offset : t -> int option
end

module Range = struct
  type t =
    | Var of ARV.Range.t
    | Phantom_var of ARPV.Range.t

  let create_var range = Var range
  let create_phantom_var range = Phantom_var range


  val info : t -> range_info

  val extremities : t -> Linearize.label * Linearize.label

  val iter
     : t
    -> f:(Subrange.t -> unit)
    -> unit

  val fold
     : t
    -> init:'a
    -> f:('a -> Subrange.t -> 'a)
    -> 'a
end

let create ~available_ranges_vars ~available_ranges_phantom_vars =
  { available_ranges_vars;
    available_ranges_phantom_vars;
  }

val find : t -> index -> Range.t option

val iter : t -> f:(index -> Range.t -> unit) -> unit

let fold t ~init ~f =
  ARV.fold t.available_ranges_vars ~init ~f:(fun index range ->
    let range = Range.create_var range in
    f index range);
  ARPV.fold t.available_ranges_phantom_vars ~init ~f:(fun index range ->
    let range = Range.create_phantom_var range in
    f index range)
