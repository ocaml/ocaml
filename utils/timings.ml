(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type file = string

type source_provenance =
  | File of file
  | Pack of string
  | Startup
  | Toplevel

type compiler_pass =
  | All
  | Parsing of file
  | Preprocessing of file
  | Typing of file
  | Transl of file
  | Generate of file
  | Assemble of source_provenance
  | Clambda of source_provenance
  | Cmm of source_provenance
  | Compile_phrases of source_provenance
  | Selection of source_provenance
  | Comballoc of source_provenance
  | CSE of source_provenance
  | Liveness of source_provenance
  | Deadcode of source_provenance
  | Spill of source_provenance
  | Split of source_provenance
  | Regalloc of source_provenance
  | Linearize of source_provenance
  | Scheduling of source_provenance
  | Emit of source_provenance
  | Flambda_pass of string * source_provenance

let timings : (compiler_pass, float * float option) Hashtbl.t =
  Hashtbl.create 20

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
  | Toplevel  -> "toplevel"

let pass_name = function
  | All -> "all"
  | Parsing file -> Printf.sprintf "parsing(%s)" file
  | Preprocessing file -> Printf.sprintf "preprocessing(%s)" file
  | Typing file -> Printf.sprintf "typing(%s)" file
  | Transl file -> Printf.sprintf "transl(%s)" file
  | Generate file -> Printf.sprintf "generate(%s)" file
  | Assemble k -> Printf.sprintf "assemble(%s)" (kind_name k)
  | Clambda k -> Printf.sprintf "clambda(%s)" (kind_name k)
  | Cmm k -> Printf.sprintf "cmm(%s)" (kind_name k)
  | Compile_phrases k -> Printf.sprintf "compile_phrases(%s)" (kind_name k)
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
  | Flambda_pass (pass, file) ->
    Printf.sprintf "flambda(%s)(%s)" pass (kind_name file)

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
