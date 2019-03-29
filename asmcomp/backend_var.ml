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

let is_internal t =
  let name = name t in
  String.length name >= 1
    && String.get name 0 = '*'
    && String.get name (String.length name - 1) = '*'

(* CR mshinwell: We need more command-line flags to control printing of
   debugging information. *)

module Provenance = struct
  type t = {
    module_path : Path.t;
    debuginfo : Debuginfo.t;
    ident_for_type : Compilation_unit.t * Ident.t;
    is_parameter : Is_parameter.t;
    is_static : bool;
  }

  let print ppf { module_path; debuginfo;
        ident_for_type = (compilation_unit, ident_for_type); is_parameter;
        is_static; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(module_path@ %a)@]@ \
        @[<hov 1>(debuginfo@ %a)@]@ \
        @[<hov 1>(ident_for_type@ %a.%a)@]@ \
        @[<hov 1>(is_parameter@ %a)@]\
        @[<hov 1>(is_static@ %b)@]\
        )@]"
      Path.print module_path
      Debuginfo.print debuginfo
      Compilation_unit.print compilation_unit
      Ident.print ident_for_type
      Is_parameter.print is_parameter
      is_static

  let create ?static ~module_path ~debuginfo ~ident_for_type is_parameter =
    let is_static =
      match static with
      | None -> false
      | Some () -> true
    in
    { module_path;
      debuginfo;
      ident_for_type;
      is_parameter;
      is_static;
    }

  let module_path t = t.module_path
  let debuginfo t = t.debuginfo
  let ident_for_type t = t.ident_for_type
  let is_parameter t = t.is_parameter
  let is_static t = t.is_static

  let replace_debuginfo t debuginfo =
    { t with debuginfo; }

  let replace_ident_for_type t ident_for_type =
    { t with ident_for_type; }

  let replace_is_parameter t is_parameter =
    { t with is_parameter; }

  let compare
        { module_path = module_path1; debuginfo = debuginfo1;
          ident_for_type = ident_for_type1; is_parameter = is_parameter1;
          is_static = is_static1;
        }
        { module_path = module_path2; debuginfo = debuginfo2;
          ident_for_type = ident_for_type2; is_parameter = is_parameter2;
          is_static = is_static2;
        } =
    let c = Path.compare module_path1 module_path2 in
    if c <> 0 then c
    else
      let c = Debuginfo.compare debuginfo1 debuginfo2 in
      if c <> 0 then c
      else
        let comp_unit1, ident_for_type1 = ident_for_type1 in
        let comp_unit2, ident_for_type2 = ident_for_type2 in
        let c = Compilation_unit.compare comp_unit1 comp_unit2 in
        if c <> 0 then c
        else
          let c = Ident.compare ident_for_type1 ident_for_type2 in
          if c <> 0 then c
          else
            let c = Is_parameter.compare is_parameter1 is_parameter2 in
            if c <> 0 then c
            else
              Stdlib.compare is_static1 is_static2
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

  type rename_provenance =
    | Clear
    | Keep
    | Replace_with of Provenance.t

  let rename t ~(provenance : rename_provenance) =
    let var = rename (var t) in
    match provenance with
    | Clear -> Without_provenance var
    | Keep ->
      begin match t with
      | Without_provenance _ -> Without_provenance var
      | With_provenance { var = _; provenance; } ->
        With_provenance { var; provenance; }
      end
    | Replace_with provenance -> With_provenance { var; provenance; }

  let print ppf t =
    match provenance t with
    | None -> print ppf (var t)
    | Some provenance ->
      Format.fprintf ppf "%a[%a]"
        print (var t)
        Provenance.print provenance
end
