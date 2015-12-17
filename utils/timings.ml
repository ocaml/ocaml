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

type build_kind =
  | File of file
  | Pack of string
  | Startup

type compiler_pass =
  | All
  | Parsing of file
  | Preprocessing of file
  | Typing of file
  | Transl of file
  | Generate of file
  | Assemble of file
  | Clambda of file
  | Cmm of file
  | Compile_phrases of file
  | Selection of build_kind
  | Comballoc of build_kind
  | CSE of build_kind
  | Liveness of build_kind
  | Deadcode of build_kind
  | Spill of build_kind
  | Split of build_kind
  | Regalloc of build_kind
  | Linearize of build_kind
  | Scheduling of build_kind
  | Emit of build_kind

let timings : (compiler_pass, float * float option) Hashtbl.t = Hashtbl.create 20
let reset () = Hashtbl.clear timings

let start pass =
  (* Cannot assert it is not here: a source file can be compiled
     multiple times on the same command line *)
  (* assert(not (Hashtbl.mem timings pass)); *)
  let time = Sys.time () in
  Hashtbl.add timings pass (time, None)

let stop pass =
  assert(Hashtbl.mem timings pass);
  let time = Sys.time () in
  let (start, stop) = Hashtbl.find timings pass in
  assert(stop = None);
  Hashtbl.replace timings pass (start, Some (time -. start))

let time pass f x =
  start pass;
  let r = f x in
  stop pass;
  r

let restart pass =
  let previous_duration =
    match Hashtbl.find timings pass with
    | exception Not_found -> 0.
    | (_, Some duration) -> duration
    | _, None -> assert false
  in
  let time = Sys.time () in
  Hashtbl.replace timings pass (time, Some previous_duration)

let accumulate pass =
  let time = Sys.time () in
  match Hashtbl.find timings pass with
  | exception Not_found -> assert false
  | _, None -> assert false
  | (start, Some duration) ->
    let duration = duration +. (time -. start) in
    Hashtbl.replace timings pass (start, Some duration)

let accumulate_time pass f x =
  restart pass;
  let r = f x in
  accumulate pass;
  r

let get pass =
  match Hashtbl.find timings pass with
  | _start, Some duration -> Some duration
  | _, None -> None
  | exception Not_found -> None

let kind_name = function
  | File f -> Printf.sprintf "sourcefile(%s)" f
  | Pack p -> Printf.sprintf "pack(%s)" p
  | Startup -> "startup"

let pass_name = function
  | All -> "all"
  | Parsing file -> Printf.sprintf "parsing(%s)" file
  | Preprocessing file -> Printf.sprintf "preprocessing(%s)" file
  | Typing file -> Printf.sprintf "typing(%s)" file
  | Transl file -> Printf.sprintf "transl(%s)" file
  | Generate file -> Printf.sprintf "generate(%s)" file
  | Assemble file -> Printf.sprintf "assemble(%s)" file
  | Clambda file -> Printf.sprintf "clambda(%s)" file
  | Cmm file -> Printf.sprintf "cmm(%s)" file
  | Compile_phrases file -> Printf.sprintf "compile_phrases(%s)" file
  | Selection k -> Printf.sprintf "selection(%s)" (kind_name k)
  | Comballoc k -> Printf.sprintf "comballoc(%s)" (kind_name k)
  | CSE k -> Printf.sprintf "cse(%s)" (kind_name k)
  | Liveness k -> Printf.sprintf "liveness(%s)" (kind_name k)
  | Deadcode k -> Printf.sprintf "deadcode(%s)" (kind_name k)
  | Spill k -> Printf.sprintf "spill(%s)" (kind_name k)
  | Split k -> Printf.sprintf "split(%s)" (kind_name k)
  | Regalloc k -> Printf.sprintf "regalloc(%s)" (kind_name k)
  | Linearize k -> Printf.sprintf "linearize(%s)" (kind_name k)
  | Scheduling k -> Printf.sprintf "scheduling(%s)" (kind_name k)
  | Emit k -> Printf.sprintf "emit(%s)" (kind_name k)

let timings_list () =
  let l = Hashtbl.fold (fun pass times l -> (pass, times) :: l) timings [] in
  List.sort (fun (_, (start1, _)) (_, (start2, _)) -> compare start1 start2) l

let print ppf =
  let current_time = Sys.time () in
  List.iter (fun (pass, (start, stop)) ->
      match stop with
      | Some duration ->
        Format.fprintf ppf "%s: %.03fs@." (pass_name pass) duration
      | None ->
        Format.fprintf ppf "%s: running since %.03fs@." (pass_name pass)
          (current_time -. start))
    (timings_list ())

