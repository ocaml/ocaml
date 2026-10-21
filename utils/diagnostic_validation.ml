(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


module Label_map = Misc.Stdlib.String.Map
module H = Diagnostic_history
module D = Diagnostic
module R = D.Record_introspection

type version =
  | Downward_compatible of H.version
  | Exact of H.version
let reference_version (Exact v | Downward_compatible v) = v
let exact_version = function
  | Exact v -> Some v
  | Downward_compatible _ -> None
let is_downward_compatible = function
  | Downward_compatible _ -> true
  | _ -> false

open Diagnostic


type _ extension += Version: version extension
let version_ty =
  let pull _ (v:H.version) = v.major, v.minor in
  Custom { id = Version; pull; default = Pair (Int,Int) }

module Metadata = struct
  let v1 = Diagnostic.Metadata_versions.v1
  include Diagnostic.Metadata
  let version = new_field v1 "version" version_ty
  let downward_compatible = new_field v1 "downward_compatible" Bool
  module Validity = struct
    include New_sum(Metadata_versions)(struct
        let name = "validity"
        let update = v1
        let description =
          "Status of the current diagnostic: \
             - Full: fully valid
             - Deprecated: some of the contents is deprecated according to the \
               current scheme
             - Invalid: invalid data\
          "
        end
      )()
      let full = new_constr0 v1 "Full"
      let deprecated = new_constr0 v1 "Deprecated"
      let invalid = new_constr0 v1 "Invalid"
      let () = seal v1
  end
  let valid: Validity.raw_type field = new_field v1 "valid" Validity.raw_type
  let path = List String
  let invalid_paths = new_field_opt v1 "invalid_paths" (List path)
  let deprecated_paths = new_field_opt v1 "deprecated_paths" (List path)
  let () = seal v1
end

type path = string list
type report_paths = { deprecated: path list; invalid: path list }
let (@^) h l = {
  deprecated = h.deprecated @ l.deprecated;
  invalid = h.invalid @ l.invalid
}
let none =  { invalid = []; deprecated=[]}
let invalid x = { invalid = [x]; deprecated = [] }
let deprecated x = { deprecated = [x]; invalid = [] }
let qualify name l = {
  deprecated = List.map (List.cons name) l.deprecated;
  invalid = List.map (List.cons name) l.invalid;
}
let concat_map f l = List.fold_left (fun acc x -> f x @^ acc) none l

let rec possibly_invalid: type a. a typ -> bool = function
  | Unit -> false
  | Int -> false
  | String -> false
  | Bool -> false
  | Float -> false
  | Pair (x,y) -> possibly_invalid x || possibly_invalid y
  | Triple (x,y,z) ->
      possibly_invalid x || possibly_invalid y || possibly_invalid z
  | Quadruple (x,y,z,w) ->
      possibly_invalid x
      || possibly_invalid y
      || possibly_invalid z
      || possibly_invalid w
  | List elt -> possibly_invalid elt
  | Custom r -> possibly_invalid r.default
  | Sum _ -> true
  | Record _ -> true

let rec record: type id.
  version:version -> id t -> id record -> report_paths =
  fun ~version sch st -> fields ~version (D.field_infos ~version:None sch) st
and fields: type id.
  version:version -> (Label_map.key * label_metadata) list
  -> id record -> report_paths
  = fun ~version metadata r ->
    concat_map (fun (k, kmd) ->
        match H.Lifetime.stage_at (Some version) kmd.status with
        | Future | Deletion -> none (* those fields will be elided *)
        | Deprecation ->
            deprecated [k]  @^
            field  ~version ~optional:(is_optional kmd) k
              (R.dynamic_get r k)
        | Preview | Publication | Expansion ->
            field  ~version ~optional:(is_optional kmd) k
              (R.dynamic_get r k)
      ) metadata
and field:
  version:version -> optional:bool -> string -> typed_val option
  -> report_paths = fun ~version ~optional name k ->
  match optional, k with
  | true, None -> none
  | false, None -> invalid [name]
  | _, Some (V (ty,v)) ->
      qualify name (value ~version v ty)
and value: type a. version:version -> a -> a typ -> report_paths =
  fun ~version v typ ->
  match typ with
  | Record m -> record ~version m v
  | Int -> none
  | Bool -> none
  | String -> none
  | Float -> none
  | Custom _ -> none
  | Unit -> none
  | List elt ->
      if possibly_invalid elt then
        concat_map (fun v -> value ~version v elt) v
      else none
  | Pair (x,y) ->
      let vx, vy = v in
      value ~version vx x @^ value ~version vy y
  | Triple (x,y,z) ->
      let vx, vy, vz = v in
      value ~version vx x
      @^ value ~version vy y
      @^ value ~version vz z
  | Quadruple (x,y,z,w) ->
      let vx, vy, vz, vw = v in
      value ~version vx x
      @^ value ~version vy y
      @^ value ~version vz z
      @^ value ~version vw w
  | Sum def ->
      D.destruct v (fun approx ->
          let name, V(ty,arg) = approx.(Array.length approx - 1) in
          match D.field_dyninfo def name with
          | None -> none
          | Some lmd ->
              begin match H.Lifetime.stage_at (Some version) lmd.status with
              | Preview | Publication | Expansion -> value ~version arg ty
              | Future | Deletion -> invalid [name]
              | Deprecation -> deprecated [name] @^ value ~version arg ty
              end
        )

let diagnostic ~version:v sch st =
  let open Metadata in
  let version = reference_version v in
  let r = record ~version sch st in
  let valid = match r.deprecated, r.invalid with
    | [], [] -> Validity.full
    | _::_, [] -> Validity.deprecated
    | _, _ :: _  -> Validity.invalid
  in
  let v1 = H.v Metadata.v1 in
  let valid = Metadata.Validity.app (Some v1) valid () in
  let metadata =
    make (Some v1) [
      Metadata.version ^= version;
      Metadata.downward_compatible ^= is_downward_compatible v;
      Metadata.valid ^= valid;
      Metadata.invalid_paths ^= r.invalid;
      Metadata.deprecated_paths ^= r.deprecated;
    ]
  in
  R.set st None
    ~field:(Diagnostic.universal_metafield ())
    metadata;
  r
