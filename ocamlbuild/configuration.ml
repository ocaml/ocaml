(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
open My_std
open Log
open Lexers

type t = Lexers.conf

let acknowledge_config config =
  List.iter
    (fun (_, config) -> List.iter Param_tags.acknowledge config.plus_tags)
    config

let cache = Hashtbl.create 107
let (configs, add_config) =
  let configs = ref [] in
  (fun () -> !configs),
  (fun config ->
     acknowledge_config config;
     configs := config :: !configs;
     Hashtbl.clear cache)

let parse_string s =
  let conf = Lexers.conf_lines None 1 (Printf.sprintf "string: %S" s) (Lexing.from_string s) in
  add_config conf

let parse_file ?dir file =
  try
    with_input_file file begin fun ic ->
      let conf = Lexers.conf_lines dir 1 (Printf.sprintf "file: %S" file) (Lexing.from_channel ic) in
      add_config conf
    end
  with Lexers.Error msg -> raise (Lexers.Error (file ^ ": " ^ msg))

let key_match = Glob.eval

let apply_config s (config : t) init =
  List.fold_left begin fun tags (key, v) ->
    if key_match key s then
      List.fold_right Tags.add v.plus_tags (List.fold_right Tags.remove v.minus_tags tags)
    else tags
  end init config

let apply_configs s = List.fold_right (apply_config s) (configs ()) Tags.empty

let tags_of_filename s =
  try Hashtbl.find cache s
  with Not_found ->
    let res = apply_configs s in
    let () = Hashtbl.replace cache s res in
    res

let has_tag tag = Tags.mem tag (tags_of_filename "")

let tag_file file tags =
  if tags <> [] then parse_string (Printf.sprintf "%S: %s" file (String.concat ", " tags));;

let tag_any tags =
  if tags <> [] then parse_string (Printf.sprintf "true: %s" (String.concat ", " tags));;
