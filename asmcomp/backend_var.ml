(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Ident

type backend_var = t

let name_for_debugger t =
  match is_optional_parameter t with
  | None -> name t
  | Some name -> name

let unique_name_for_debugger t =
  let name =
    match is_optional_parameter t with
    | None -> name t
    | Some name -> name
  in
  Printf.sprintf "%s/%d" name (stamp t)

(* CR mshinwell: We need more command-line flags to control printing of
   debugging information. *)

module Provenance = struct
  type t = {
    module_path : Path.t;
    debuginfo : Debuginfo.t;
    ident_for_type : Compilation_unit.t * Ident.t;
    is_parameter : Is_parameter.t;
  }

  let print ppf { module_path; debuginfo;
        ident_for_type = (compilation_unit, ident_for_type); is_parameter; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(module_path@ %a)@]@ \
        @[<hov 1>(debuginfo@ %a)@]@ \
        @[<hov 1>(ident_for_type@ %a.%a)@]@ \
        @[<hov 1>(is_parameter@ %a)@]\
        )@]"
      Path.print module_path
      Debuginfo.print debuginfo
      Compilation_unit.print compilation_unit
      Ident.print ident_for_type
      Is_parameter.print is_parameter

  let create ~module_path ~debuginfo ~ident_for_type is_parameter =
    { module_path;
      debuginfo;
      ident_for_type;
      is_parameter;
    }

  let module_path t = t.module_path
  let debuginfo t = t.debuginfo
  let ident_for_type t = t.ident_for_type
  let is_parameter t = t.is_parameter

  let replace_debuginfo t debuginfo =
    { t with debuginfo; }

  let replace_ident_for_type t ident_for_type =
    { t with ident_for_type; }

  let replace_is_parameter t is_parameter =
    { t with is_parameter; }
end

module With_provenance = struct
  type t =
    | Without_provenance of backend_var
    | With_provenance of {
        var : backend_var;
        provenance : Provenance.t;
      }

  let create ?provenance var =
    match provenance with
    | None -> Without_provenance var
    | Some provenance -> With_provenance { var; provenance; }

  let var t =
    match t with
    | Without_provenance var
    | With_provenance { var; provenance = _; } -> var

  let provenance t =
    match t with
    | Without_provenance _ -> None
    | With_provenance { var = _; provenance; } -> Some provenance

  let name t = name (var t)

  let is_optional_parameter t = is_optional_parameter (var t)

  let rename ?provenance t =
    let var = rename (var t) in
    match provenance with
    | None ->
      begin match t with
      | Without_provenance _ -> Without_provenance var
      | With_provenance { var = _; provenance; } ->
        With_provenance { var; provenance; }
      end
    | Some provenance -> With_provenance { var; provenance; }

  let print ppf t =
    match provenance t with
    | None -> print ppf (var t)
    | Some provenance ->
      Format.fprintf ppf "%a[%a]"
        print (var t)
        Provenance.print provenance
end
