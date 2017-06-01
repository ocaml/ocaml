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
  | E of (string, info) Hashtbl.t
[@@unboxed]
and info = {
    times : times;
    minor_words : float;
    max_live_words : int option;
    hierarchy : hierarchy;
  }

let hierarchy = ref (E (Hashtbl.create 2))
let reset () = hierarchy := E (Hashtbl.create 2)

let time_call ?(accumulate = false) name f =
  let E prev_hierarchy = !hierarchy in
  let this_times_and_minor_words, this_table =
    (* We allow the recording of multiple categories by the same name, for tools like
       ocamldoc that use the compiler libs but don't care about timings information,
       and so may record, say, "parsing" multiple times. *)
    if accumulate
    then
      match Hashtbl.find prev_hierarchy name with
      | exception Not_found -> None, Hashtbl.create 2
      | { times; minor_words; max_live_words; hierarchy = E table } ->
        Hashtbl.remove prev_hierarchy name;
        Some (times, minor_words, max_live_words), table
    else None, Hashtbl.create 2
  in
  hierarchy := E this_table;
  let max_live_words = ref None in
  let alarm =
    if !Clflags.record_max_live_words then
      let alarm () =
        let stat = Gc.stat () in
        match !max_live_words with
        | None ->
          max_live_words := Some stat.Gc.live_words
        | Some live_words ->
          max_live_words := Some (max live_words stat.Gc.live_words)
      in
      Some (Gc.create_alarm alarm)
    else
      None
  in
  let start = cpu_time () in
  let start_minor_words = Gc.minor_words () in
  Misc.try_finally f
    (fun () ->
       hierarchy := E prev_hierarchy;
       let end_minor_words = Gc.minor_words () in
       let end_ = cpu_time () in
       (match alarm with
        | None -> ()
        | Some alarm ->
          Gc.delete_alarm alarm);
       let times, minor_words, max_live_words =
         match this_times_and_minor_words with
         | None ->
           { start; duration = end_ -. start },
           end_minor_words -. start_minor_words,
           !max_live_words
         | Some ({ start = initial_start; duration },
                 previous_minor_words, max_heap) ->
           { start = initial_start; duration = duration +. end_ -. start },
           previous_minor_words +. end_minor_words -. start_minor_words,
           match !max_live_words, max_heap with
           | None, v | v, None -> v
           | Some max_live_words, Some max_heap ->
             Some (max max_live_words max_heap)
       in
       Hashtbl.add prev_hierarchy name
         { times; minor_words; max_live_words; hierarchy = E this_table })

let time ?accumulate pass f x = time_call ?accumulate pass (fun () -> f x)

let timings_list (E table) =
  let l = Hashtbl.fold (fun k d l -> (k, d) :: l) table [] in
  List.sort (fun (pass1, info1) (pass2, info2 ) ->
    compare (info1.times.start, pass1) (info2.times.start, pass2)) l

(* Because indentation is meaningful, and because the durations are
   the first element of each row, we can't pad them with spaces. *)
let duration_as_string ~pad duration = Printf.sprintf "%0*.03f" pad duration

let rec print ppf hierarchy ~total:(total_time, total_words) ~nesting =
  let total_of_children = ref 0. in
  let list = timings_list hierarchy in
  let max_duration_width =
    List.fold_left
      (fun acc (_, { times }) ->
         max acc (String.length (duration_as_string ~pad:0 times.duration)))
      0 list
  in
  let print_max_live_words ppf = function
    | None ->
      if !Clflags.record_max_live_words then
        Format.fprintf ppf " (/)"
      else
        ()
    | Some max_live_words ->
      Format.fprintf ppf " %iw" max_live_words
  in
  let print_pass ~duration ~words ~max_live_words ~pass =
    let duration_as_string =
      duration_as_string ~pad:max_duration_width duration in
    if float_of_string duration_as_string <> 0. then
      Format.fprintf ppf "%s%ss %gw%a %s@\n"
        (String.make (nesting * 2) ' ') duration_as_string words
        print_max_live_words max_live_words pass
  in
  List.iter (fun (pass, info) ->
    print_pass ~duration:info.times.duration ~words:info.minor_words
      ~max_live_words:info.max_live_words ~pass;
    print ppf info.hierarchy ~total:(info.times.duration, info.minor_words)
      ~nesting:(nesting + 1);
    total_of_children := !total_of_children +. info.times.duration;
  ) list;
  if list <> [] || nesting = 0 then
    print_pass ~duration:(total_time -. !total_of_children)
      ~words:total_words ~max_live_words:None ~pass:"other";
;;

let print ?(total = (cpu_time (), Gc.minor_words ())) ppf =
  print ppf !hierarchy ~total ~nesting:0

let generate = "generate"
let transl = "transl"
let typing = "typing"
