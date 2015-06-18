(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type file = string
type part =
  | All
  | Parsing of file
  | Typing of file
  | Transl of file
  | Generate of file
  | Assemble of file
  | Flambda_middle_end of file
  | Flambda_backend of file
  | Cmm of file
  | Compile_phrases of file

let timings : (part, float * float option) Hashtbl.t = Hashtbl.create 20
let reset () = Hashtbl.clear timings

let start part =
  assert(not (Hashtbl.mem timings part));
  let time = Sys.time () in
  Hashtbl.add timings part (time, None)

let start_id part x =
  start part; x

let stop part =
  assert(Hashtbl.mem timings part);
  let time = Sys.time () in
  let (start, stop) = Hashtbl.find timings part in
  assert(stop = None);
  Hashtbl.replace timings part (start, Some time)

let stop_id part x =
  stop part; x

let time part f x =
  start part;
  let r = f x in
  stop part;
  r

let get part =
  match Hashtbl.find timings part with
  | start, Some stop -> Some (stop -. start)
  | _, None -> None
  | exception Not_found -> None

let part_name = function
  | All -> "all"
  | Parsing file -> Printf.sprintf "parsing(%s)" file
  | Typing file -> Printf.sprintf "typing(%s)" file
  | Transl file -> Printf.sprintf "transl(%s)" file
  | Generate file -> Printf.sprintf "generate(%s)" file
  | Assemble file -> Printf.sprintf "assemble(%s)" file
  | Flambda_middle_end file -> Printf.sprintf "flambda-mid(%s)" file
  | Flambda_backend file -> Printf.sprintf "flambda-back(%s)" file
  | Cmm file -> Printf.sprintf "cmm(%s)" file
  | Compile_phrases file -> Printf.sprintf "compile_phrases(%s)" file

let print ppf =
  let current_time = Sys.time () in
  Hashtbl.iter (fun part -> function
      | start, Some stop ->
          Format.fprintf ppf "%s: %.03fs@." (part_name part) (stop -. start)
      | start, None ->
          Format.fprintf ppf "%s: running since %.03fs@." (part_name part)
            (current_time -. start))
    timings

