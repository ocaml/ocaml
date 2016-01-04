(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                 Jeremie Dimino, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Asttypes
open Parsetree

type error =
  | Multiple_attributes of string
  | No_payload_expected of string

exception Error of Location.t * error

type identifier =
  { names: string list; neighbouring_names: string list; max_distance:int }

let std_namespace name = [ name; "ocaml." ^ name]
let create ?(neighbouring_names=[]) ?(max_distance=2) name =
  {names = std_namespace name; neighbouring_names; max_distance }

let attribute_max_distance = ref 2

let min_distance cutoff_distance txt x name_y =
  let dy = Misc.edit_distance txt name_y cutoff_distance in
  match x, dy  with
  |  _ , None -> x
  | Some (dx, _), Some dy when dx <= dy -> x
  | _, Some dy -> Some ( dy, name_y)

let set_projector cutoff_distance set txt =
  List.fold_left (min_distance cutoff_distance txt) None set

let is_warning_active () =
  let open Warnings in
  is_active ( Misspelled_attribute ("","") )

let is_attribute ?(warn=true) {names;neighbouring_names;max_distance}
    ({txt;loc}, _ )  =
  let result = List.mem txt names in
  let () = (* misspelling check *)
    if not result && warn && is_warning_active () then
      let nearest_name = set_projector max_distance names txt
      and nearest_neighborhood =
        set_projector max_distance neighbouring_names txt in
      let warn name =
        Location.prerr_warning loc
          (Warnings.Misspelled_attribute (txt,name)) in
      match nearest_name, nearest_neighborhood with
      | Some (0,_), _ -> ()
      | Some (dx,x) , Some (dy,y) ->
          if dx <= dy then
            warn x
      | Some (dx,x), None -> warn x
      | _ -> () in
  result

let get_no_payload_attribute ?(warn=true) identifier attrs =
  match List.filter (is_attribute ~warn identifier) attrs with
  | [] -> None
  | [ (name, PStr []) ] -> Some name
  | [ (name, _) ] ->
    raise (Error (name.loc, No_payload_expected name.txt))
  | _ :: (name, _) :: _ ->
    raise (Error (name.loc, Multiple_attributes name.txt))

let has_no_payload_attribute ?(warn=true) identifier attrs =
  match get_no_payload_attribute ~warn identifier attrs with
  | None   -> false
  | Some _ -> true

open Format

let report_error ppf = function
  | Multiple_attributes name ->
    fprintf ppf "Too many `%s' attributes" name
  | No_payload_expected name ->
    fprintf ppf "Attribute `%s' does not accept a payload" name

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )
