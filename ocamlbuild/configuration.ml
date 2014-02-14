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

let parse_lexbuf ?dir source lexbuf =
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = source };
  let conf = Lexers.conf_lines dir lexbuf in
  add_config conf

let parse_string s = parse_lexbuf (Printf.sprintf "String %S" s) (Lexing.from_string s)

let parse_file ?dir file =
  with_input_file file begin fun ic ->
    parse_lexbuf ?dir (Printf.sprintf "File %S" file) (Lexing.from_channel ic)
  end

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

let global_tags () = tags_of_filename ""
let has_tag tag = Tags.mem tag (global_tags ())

let tag_file file tags =
  if tags <> [] then parse_string (Printf.sprintf "%S: %s" file (String.concat ", " tags));;

let tag_any tags =
  if tags <> [] then parse_string (Printf.sprintf "true: %s" (String.concat ", " tags));;
