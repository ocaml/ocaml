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

module Subrange_info = struct
  type t =
    | Non_phantom of {
        reg : Reg.t;
        offset_from_cfa_in_bytes : int option;
      }
    | Phantom of Mach.phantom_defining_expr
end

module Range_info = struct
  type t =
    | Var of ARV.Range_info.t
    | Phantom_var of ARPV.Range_info.t

  let provenance t =
    match t with
    | Var range_info -> ARV.Range_info.provenance range_info
    | Phantom_var range_info -> ARPV.Range_info.provenance range_info

  let is_parameter t =
    match t with
    | Var range_info -> ARV.Range_info.is_parameter range_info
    | Phantom_var range_info -> ARPV.Range_info.is_parameter range_info
end

type t = {
  available_ranges_vars : Available_ranges_vars.t;
  available_ranges_phantom_vars : Available_ranges_phantom_vars.t;
}

module Subrange = struct
  type t =
    | Var of ARV.Subrange.t
    | Phantom_var of Mach.phantom_defining_expr * ARPV.Subrange.t

  let create_var subrange = Var subrange

  let create_phantom_var subrange = Phantom_var subrange

  let info t =
    match t with
    | Var subrange ->
      let subrange_info = ARV.Subrange.subrange_info subrange in
      Non_phantom {
        reg = ARV.Subrange_info.reg subrange_info;
        offset_from_cfa_in_bytes =
          ARV.Subrange_info.offset_from_cfa_in_bytes subrange_info;
      }
    | Phantom_var (defining_expr, _subrange) ->
      Phantom defining_expr

  let start_pos t =
    match t with
    | Var subrange -> ARV.Subrange.start_pos subrange
    | Phantom_var subrange -> ARPV.Subrange.start_pos subrange

  let end_pos t =
    match t with
    | Var subrange -> ARV.Subrange.end_pos subrange
    | Phantom_var subrange -> ARPV.Subrange.end_pos subrange

  let end_pos_offset t =
    match t with
    | Var subrange -> ARV.Subrange.end_pos_offset subrange
    | Phantom_var subrange -> ARPV.Subrange.end_pos_offset subrange
end

module Range = struct
  type t =
    | Var of ARV.Range.t
    | Phantom_var of ARPV.Range.t

  let create_var range = Var range
  let create_phantom_var range = Phantom_var range

  let info t =
    match t with
    | Var range ->
      Range_info.create_var (ARV.Range.range_info range)
    | Phantom_var range ->
      Range_info.create_phantom_var (ARPV.Range.range_info range)

  let extremities t =
    match t with
    | Var range -> ARV.Range.extremities range
    | Phantom_var range -> ARPV.Range.extremities range

  let fold t ~init ~f =
    match t with
    | Var range -> ARV.Range.fold range ~init ~f
    | Phantom_var range -> ARPV.Range.fold range ~init ~f
end

let create ~available_ranges_vars ~available_ranges_phantom_vars =
  let vars = ARV.all_indexes available_ranges_vars in
  let phantom_vars = ARPV.all_indexes available_ranges_phantom_vars in
  if not (Backend_var.Set.is_empty (Backend_var.Set.inter vars phantom_vars))
  then begin
    Misc.fatal_error "Available_ranges_all_vars.create: sets not disjoint:@ \
        non-phantom = {%a},@ phantom = {%a}"
      Backend_var.Set.print vars
      Backend_var.Set.print phantom_vars
  end;
  { available_ranges_vars;
    available_ranges_phantom_vars;
  }

let iter t ~f =
  ARV.iter t.available_ranges_vars ~f:(fun index range ->
    let range = Range.create_var range in
    f index range);
  ARPV.iter t.available_ranges_phantom_vars ~f:(fun index range ->
    let range = Range.create_phantom_var range in
    f index range)

let fold t ~init ~f =
  ARV.fold t.available_ranges_vars ~init ~f:(fun index range ->
    let range = Range.create_var range in
    f index range);
  ARPV.fold t.available_ranges_phantom_vars ~init ~f:(fun index range ->
    let range = Range.create_phantom_var range in
    f index range)
