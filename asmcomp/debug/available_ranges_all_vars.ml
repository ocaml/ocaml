(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module ARV = Available_ranges_vars
module ARPV = Available_ranges_phantom_vars
module L = Linearize
module V = Backend_var

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
    | Static_phantom_var of {
        provenance : V.Provenance.t option;
        is_parameter : Is_parameter.t;
        defining_expr : Mach.phantom_defining_expr;
      }

  let create_var info = Var info

  let create_phantom_var info = Phantom_var info

  let create_static_phantom_var provenance is_parameter defining_expr =
    Static_phantom_var {
      provenance;
      is_parameter;
      defining_expr;
    }

  let provenance t =
    match t with
    | Var range_info -> ARV.Range_info.provenance range_info
    | Phantom_var range_info -> ARPV.Range_info.provenance range_info
    | Static_phantom_var { provenance; _ } -> provenance

  let debuginfo t =
    match provenance t with
    | None -> Debuginfo.none
    | Some provenance -> Backend_var.Provenance.debuginfo provenance

  let is_parameter t =
    match t with
    | Var range_info -> ARV.Range_info.is_parameter range_info
    | Phantom_var range_info -> ARPV.Range_info.is_parameter range_info
    | Static_phantom_var { is_parameter; _ } -> is_parameter

  type phantom_defining_expr =
    | Non_phantom
    | Phantom of Mach.phantom_defining_expr

  let phantom_defining_expr (t : t) : phantom_defining_expr =
    match t with
    | Var _ -> Non_phantom
    | Phantom_var range_info ->
      Phantom (ARPV.Range_info.defining_expr range_info)
    | Static_phantom_var { defining_expr; _ } -> Phantom defining_expr
end

module Subrange = struct
  type t =
    | Var of ARV.Subrange.t
    | Phantom_var of Mach.phantom_defining_expr * ARPV.Subrange.t

  let create_var subrange = Var subrange

  let create_phantom_var defining_expr subrange =
    Phantom_var (defining_expr, subrange)

  let info t : Subrange_info.t =
    match t with
    | Var subrange ->
      let subrange_info = ARV.Subrange.info subrange in
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
    | Phantom_var (_, subrange) -> ARPV.Subrange.start_pos subrange

  let start_pos_offset t =
    match t with
    | Var subrange -> ARV.Subrange.start_pos_offset subrange
    | Phantom_var (_, subrange) -> ARPV.Subrange.start_pos_offset subrange

  let end_pos t =
    match t with
    | Var subrange -> ARV.Subrange.end_pos subrange
    | Phantom_var (_, subrange) -> ARPV.Subrange.end_pos subrange

  let end_pos_offset t =
    match t with
    | Var subrange -> ARV.Subrange.end_pos_offset subrange
    | Phantom_var (_, subrange) -> ARPV.Subrange.end_pos_offset subrange
end

module Range = struct
  type t =
    | Var of ARV.Range.t
    | Phantom_var of ARPV.Range.t
    | Static_phantom_var of Range_info.t

  let create_var range = Var range

  let create_phantom_var range = Phantom_var range

  let create_static_phantom_var provenance is_parameter defining_expr =
    Static_phantom_var (Range_info.create_static_phantom_var provenance
      is_parameter defining_expr)

  let info t =
    match t with
    | Var range ->
      Range_info.create_var (ARV.Range.info range)
    | Phantom_var range ->
      Range_info.create_phantom_var (ARPV.Range.info range)
    | Static_phantom_var range_info -> range_info

  let extremities t =
    match t with
    | Var range -> Some (ARV.Range.extremities range)
    | Phantom_var range -> Some (ARPV.Range.extremities range)
    | Static_phantom_var _ -> None

  let fold t ~init ~f =
    match t with
    | Var range ->
      ARV.Range.fold range ~init ~f:(fun acc subrange ->
        f acc (Subrange.create_var subrange))
    | Phantom_var range ->
      let defining_expr =
        ARPV.Range_info.defining_expr (ARPV.Range.info range)
      in
      ARPV.Range.fold range ~init ~f:(fun acc subrange ->
        f acc (Subrange.create_phantom_var defining_expr subrange))
    | Static_phantom_var _ -> init
end

type t = {
  available_ranges_vars : Available_ranges_vars.t;
  available_ranges_phantom_vars : Available_ranges_phantom_vars.t;
  static_phantom_vars : Range.t V.Map.t;
}

let empty = {
  available_ranges_vars = Available_ranges_vars.empty;
  available_ranges_phantom_vars = Available_ranges_phantom_vars.empty;
  static_phantom_vars = V.Map.empty;
}

let create ~available_ranges_vars ~available_ranges_phantom_vars
      (fundecl : L.fundecl) =
  let vars = ARV.all_indexes available_ranges_vars in
  let phantom_vars = ARPV.all_indexes available_ranges_phantom_vars in
  if not (Backend_var.Set.is_empty (Backend_var.Set.inter vars phantom_vars))
  then begin
    Misc.fatal_errorf "Available_ranges_all_vars.create: sets not disjoint:@ \
        non-phantom = {%a},@ phantom = {%a},@ inter = {%a}"
      Backend_var.Set.print vars
      Backend_var.Set.print phantom_vars
      Backend_var.Set.print (Backend_var.Set.inter vars phantom_vars)
  end;
  let static_phantom_vars =
    V.Map.fold (fun var (provenance, defining_expr) static_phantom_vars ->
        match provenance with
        | None -> static_phantom_vars
        | Some provenance ->
          if not (V.Provenance.is_static provenance) then static_phantom_vars
          else
            let range =
              Range.create_static_phantom_var (Some provenance)
                Is_parameter.local defining_expr
            in
            V.Map.add var range static_phantom_vars)
      fundecl.fun_phantom_lets
      V.Map.empty
  in
  { available_ranges_vars;
    available_ranges_phantom_vars;
    static_phantom_vars;
  }

let iter t ~f =
  ARV.iter t.available_ranges_vars ~f:(fun index range ->
    let range = Range.create_var range in
    f index range);
  ARPV.iter t.available_ranges_phantom_vars ~f:(fun index range ->
    let range = Range.create_phantom_var range in
    f index range);
  V.Map.iter (fun var range -> f var range) t.static_phantom_vars

let fold t ~init ~f =
  let init =
    ARV.fold t.available_ranges_vars ~init ~f:(fun acc index range ->
      let range = Range.create_var range in
      f acc index range)
  in
  let init =
    ARPV.fold t.available_ranges_phantom_vars ~init ~f:(fun acc index range ->
      let range = Range.create_phantom_var range in
      f acc index range)
  in
  V.Map.fold (fun var range acc -> f acc var range) t.static_phantom_vars init
