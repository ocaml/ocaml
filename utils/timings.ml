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
  | Clambda of file
  | Cmm of file
  | Compile_phrases of file
  | Regalloc

let timings : (part, float * float option) Hashtbl.t = Hashtbl.create 20
let reset () = Hashtbl.clear timings

let start part =
  (* Cannot assert it is not here: a source file can be compiled
     multiple times on the same command line *)
  (* assert(not (Hashtbl.mem timings part)); *)
  let time = Sys.time () in
  Hashtbl.add timings part (time, None)

let start_id part x =
  start part; x

let stop part =
  assert(Hashtbl.mem timings part);
  let time = Sys.time () in
  let (start, stop) = Hashtbl.find timings part in
  assert(stop = None);
  Hashtbl.replace timings part (start, Some (time -. start))

let stop_id part x =
  stop part; x

let time part f x =
  start part;
  let r = f x in
  stop part;
  r

let restart part =
  let previous_duration =
    match Hashtbl.find timings part with
    | exception Not_found -> 0.
    | (_, Some duration) -> duration
    | _, None -> assert false
  in
  let time = Sys.time () in
  Hashtbl.replace timings part (time, Some previous_duration)

let accumulate part =
  let time = Sys.time () in
  match Hashtbl.find timings part with
  | exception Not_found -> assert false
  | _, None -> assert false
  | (start, Some duration) ->
    let duration = duration +. (time -. start) in
    Hashtbl.replace timings part (start, Some duration)

let accumulate_time part f x =
  restart part;
  let r = f x in
  accumulate part;
  r

let get part =
  match Hashtbl.find timings part with
  | _start, Some duration -> Some duration
  | _, None -> None
  | exception Not_found -> None

let part_name = function
  | All -> "all"
  | Parsing file -> Printf.sprintf "parsing(%s)" file
  | Typing file -> Printf.sprintf "typing(%s)" file
  | Transl file -> Printf.sprintf "transl(%s)" file
  | Generate file -> Printf.sprintf "generate(%s)" file
  | Assemble file -> Printf.sprintf "assemble(%s)" file
  | Clambda file -> Printf.sprintf "clambda(%s)" file
  | Cmm file -> Printf.sprintf "cmm(%s)" file
  | Compile_phrases file -> Printf.sprintf "compile_phrases(%s)" file
  | Regalloc -> Printf.sprintf "regalloc"

let timings_list () =
  let l = Hashtbl.fold (fun part times l -> (part, times) :: l) timings [] in
  List.sort (fun (_, (start1, _)) (_, (start2, _)) -> compare start1 start2) l

let print ppf =
  let current_time = Sys.time () in
  List.iter (fun (part, (start, stop)) ->
      match stop with
      | Some duration ->
        Format.fprintf ppf "%s: %.03fs@." (part_name part) duration
      | None ->
        Format.fprintf ppf "%s: running since %.03fs@." (part_name part)
          (current_time -. start))
    (timings_list ())

