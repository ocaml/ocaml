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

type identifier = { names: string list; context: string list}

let std_namespace name = [ name; "ocaml." ^ name]
let create ?(context=[]) name = {names = std_namespace name; context}

let attribute_max_distance = ref 2

let distance txt_1 txt_2 =
  Misc.edit_distance txt_1 txt_2 !attribute_max_distance

let min_distance txt x name_y =
  let dy = distance txt name_y in
  match x, dy  with
  |  _ , None -> x
  | Some (dx, _), Some dy when dx <= dy -> x
  | _, Some dy -> Some ( dy, name_y)

let set_projector set txt =
  List.fold_left (min_distance txt) None set

let is_warning_active () =
  let open Warnings in
  is_active ( Misspelled_attribute ("","") )

let is_attribute ?(warn=true) {names;context} ({txt;loc}, _ )  =
  if not warn || not (is_warning_active ())then
    List.mem txt names
  else
    let nearest_name = set_projector names txt
    and nearest_context = set_projector context txt in
    let warn name =
            Location.prerr_warning loc
              (Warnings.Misspelled_attribute (txt,name)) in
    match nearest_name, nearest_context with
    | Some (0, _), _ -> true
    | _ , Some(0, _) -> false
    | Some (dx,x) , Some (dy,y) ->
        if dx <= dy then
          warn x;
             false
    | Some (dx,x), None -> warn x; false
    | _ -> false


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
