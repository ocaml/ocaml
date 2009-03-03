(***********************************************************************)
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

type flag_list = (string * string) list

type t = Lexers.conf

let cache = Hashtbl.create 107
let (configs, add_config) =
  let configs = ref [] in
  (fun () -> !configs),
  (fun config -> configs := config :: !configs; Hashtbl.clear cache)

let parse_string s =
  let conf = Lexers.conf_lines None 1 (Printf.sprintf "string: %S" s) (Lexing.from_string s) in
  add_config conf

let parse_file ?dir file =
  with_input_file file begin fun ic ->
    let conf = Lexers.conf_lines dir 1 (Printf.sprintf "file: %S" file) (Lexing.from_channel ic) in
    add_config conf
  end

let key_match = Glob.eval

let apply_config s (config : t) init =
  List.fold_left begin fun (tags, flags as acc) (key, v) ->
    if key_match key s then
      (List.fold_right Tags.add v.plus_tags (List.fold_right Tags.remove v.minus_tags tags),
       List.fold_right Flags.add v.plus_flags (List.fold_right Flags.remove v.minus_flags flags))
    else acc
  end init config

let apply_configs s =
  let (tags, flags) =
    List.fold_right (apply_config s) (configs ()) (Tags.empty, [])
  in (tags, Flags.to_spec flags)

let tags_and_flags_of_filename s =
  try Hashtbl.find cache s
  with Not_found ->
    let res = apply_configs s in
    let () = Hashtbl.replace cache s res in
    res

let tags_of_filename x = fst (tags_and_flags_of_filename x)
let flags_of_filename x = snd (tags_and_flags_of_filename x)

let has_tag tag = Tags.mem tag (tags_of_filename "")

let tag_file file tags =
  if tags <> [] then parse_string (Printf.sprintf "%S: %s" file (String.concat ", " tags));;

let tag_any tags =
  if tags <> [] then parse_string (Printf.sprintf "true: %s" (String.concat ", " tags));;
