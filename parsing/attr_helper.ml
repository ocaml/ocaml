(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jeremie Dimino, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree

type error =
  | Multiple_attributes of string
  | No_payload_expected of string

exception Error of Location.t * error

let get_no_payload_attribute alt_names attrs =
  match List.filter (fun a -> List.mem a.attr_name.txt alt_names) attrs with
  | [] -> None
  | [ {attr_name = name; attr_payload = PStr []; attr_loc = _} ] -> Some name
  | [ {attr_name = name; _} ] ->
    raise (Error (name.loc, No_payload_expected name.txt))
  | _ :: {attr_name = name; _} :: _ ->
    raise (Error (name.loc, Multiple_attributes name.txt))

let has_no_payload_attribute alt_names attrs =
  match get_no_payload_attribute alt_names attrs with
  | None   -> false
  | Some _ -> true

let report_error ppf = function
  | Multiple_attributes name ->
    I18n.fprintf ppf "Too many `%s' attributes" name
  | No_payload_expected name ->
    I18n.fprintf ppf "Attribute `%s' does not accept a payload" name

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
