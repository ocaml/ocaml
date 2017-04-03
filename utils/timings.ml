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

external time_include_children: bool -> float = "caml_sys_time_include_children"
let cpu_time () = time_include_children true

type times = { start : float; duration : float }
type hierarchy =
  | E of (string, times * hierarchy) Hashtbl.t
[@@unboxed]

let hierarchy = ref (E (Hashtbl.create 2))
let reset () = hierarchy := E (Hashtbl.create 2)

let time_call ?(accumulate = false) name f =
  let E prev_hierarchy = !hierarchy in
  let this_times, this_table =
    (* We allow the recording of multiple categories by the same name, for tools like
       ocamldoc that use the compiler libs but don't care about timings information,
       and so may record, say, "parsing" multiple times. *)
    if accumulate
    then
      match Hashtbl.find prev_hierarchy name with
      | exception Not_found -> None, Hashtbl.create 2
      | times, E table ->
        Hashtbl.remove prev_hierarchy name;
        Some times, table
    else None, Hashtbl.create 2
  in
  hierarchy := E this_table;
  let start = cpu_time () in
  Misc.try_finally f
    (fun () ->
       hierarchy := E prev_hierarchy;
       let end_ = cpu_time () in
       let times =
         match this_times with
         | None -> { start; duration = end_ -. start }
         | Some { start = initial_start; duration } ->
           { start = initial_start; duration = duration +. end_ -. start }
       in
       Hashtbl.add prev_hierarchy name (times, E this_table))

let time ?accumulate pass f x = time_call ?accumulate pass (fun () -> f x)

let timings_list (E table) =
  let l = Hashtbl.fold (fun k d l -> (k, d) :: l) table [] in
  List.sort (fun (pass1, (start1, _)) (pass2, (start2, _)) ->
    compare (start1, pass1) (start2, pass2)) l

(* Because indentation is meaningful, and because the durations are
   the first element of each row, we can't pad them with spaces. *)
let duration_as_string ~pad duration = Printf.sprintf "%0*.03f" pad duration

let rec print ppf hierarchy ~total ~nesting =
  let total_of_children = ref 0. in
  let list = timings_list hierarchy in
  let max_duration_width =
    List.fold_left
      (fun acc (_, (times, _)) ->
         max acc (String.length (duration_as_string ~pad:0 times.duration)))
      0 list
  in
  let print_pass ~duration ~pass =
    let duration_as_string =
      duration_as_string ~pad:max_duration_width duration in
    if float_of_string duration_as_string <> 0. then
      Format.fprintf ppf "%s%ss %s@\n"
        (String.make (nesting * 2) ' ') duration_as_string pass
  in
  List.iter (fun (pass, ({ start = _; duration }, sub_hierarchy)) ->
    print_pass ~duration ~pass;
    print ppf sub_hierarchy ~total:duration ~nesting:(nesting + 1);
    total_of_children := !total_of_children +. duration;
  ) list;
  if list <> [] || nesting = 0 then
    print_pass ~duration:(total -. !total_of_children) ~pass:"other";
;;

let print ?(total = cpu_time ()) ppf =
  print ppf !hierarchy ~total ~nesting:0

let generate = "generate"
let transl = "transl"
let typing = "typing"
