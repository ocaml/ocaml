(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(******************************* Breakpoints ***************************)

open Checkpoints
open Debugcom
open Instruct
open Events
open Printf

(*** Debugging. ***)
let debug_breakpoints = ref false

(*** Data. ***)

(* Number of the last added breakpoint. *)
let breakpoint_number = ref 0

(* Breakpoint number -> event. *)
type breakpoint_id = int
let breakpoints = ref ([] : (breakpoint_id * code_event) list)

(* Program counter -> breakpoint count. *)
let positions = ref ([] : (pc * int ref) list)

(* Versions of the breakpoint list. *)
let current_version = ref 0
let max_version = ref 0

(*** Miscellaneous. ***)

(* Mark breakpoints as installed in current checkpoint. *)
let copy_breakpoints () =
  !current_checkpoint.c_breakpoints <- !positions;
  !current_checkpoint.c_breakpoint_version <- !current_version

(* Announce a new version of the breakpoint list. *)
let new_version () =
  incr max_version;
  current_version := !max_version

(*** Information about breakpoints. ***)

let breakpoints_count () =
  List.length !breakpoints

(* List of breakpoints at `pc'. *)
let rec breakpoints_at_pc pc =
  begin match Symbols.event_at_pc pc with
  | {ev_frag = frag; ev_ev = {ev_repr = Event_child {contents = pos}}} ->
     breakpoints_at_pc {frag; pos}
  | _ -> []
  | exception Not_found -> []
  end
    @
  List.map fst (List.filter
                  (function (_, {ev_frag = frag; ev_ev = {ev_pos = pos}}) ->
                            {frag; pos} = pc)
                  !breakpoints)

(* Is there a breakpoint at `pc' ? *)
let breakpoint_at_pc pc =
  breakpoints_at_pc pc <> []

(*** Set and remove breakpoints ***)

let print_pc out {frag;pos} = fprintf out "%d:%d" frag pos

(* Remove all breakpoints. *)
let remove_breakpoints pcs =
  if !debug_breakpoints then
    printf "Removing breakpoints...\n%!";
  List.iter
    (function (pc, _) ->
       if !debug_breakpoints then printf "%a\n%!" print_pc pc;
       reset_instr pc;
       Symbols.set_event_at_pc pc)
    pcs

(* Set all breakpoints. *)
let set_breakpoints pcs =
  if !debug_breakpoints then
    printf "Setting breakpoints...\n%!";
  List.iter
    (function (pc, _) ->
       if !debug_breakpoints then printf "%a\n%!" print_pc pc;
       set_breakpoint pc)
    pcs

(* Ensure the current version is installed in current checkpoint. *)
let update_breakpoints () =
  if !debug_breakpoints then begin
    prerr_string "Updating breakpoints... ";
    prerr_int !current_checkpoint.c_breakpoint_version;
    prerr_string " ";
    prerr_int !current_version;
    prerr_endline ""
  end;
  if !current_checkpoint.c_breakpoint_version <> !current_version then
    Exec.protect
      (function () ->
         remove_breakpoints !current_checkpoint.c_breakpoints;
         set_breakpoints !positions;
         copy_breakpoints ())

(* Execute given function with no breakpoint in current checkpoint. *)
(* --- `goto' runs faster this way (does not stop on each breakpoint). *)
let execute_without_breakpoints f =
  Misc.protect_refs [Misc.R (Debugger_config.break_on_load, false);
                     Misc.R (current_version, 0);
                     Misc.R (positions, []);
                     Misc.R (breakpoints, []);
                     Misc.R (breakpoint_number, 0)]
                    f

(* Add a position in the position list. *)
(* Change version if necessary. *)
let insert_position pos =
  try
    incr (List.assoc pos !positions)
  with
    Not_found ->
      positions := (pos, ref 1) :: !positions;
      new_version ()

(* Remove a position in the position list. *)
(* Change version if necessary. *)
let remove_position pos =
  let count = List.assoc pos !positions in
    decr count;
    if !count = 0 then begin
      positions := List.remove_assoc pos !positions;
      new_version ()
    end

(* Insert a new breakpoint in lists. *)
let rec new_breakpoint event =
  match event with
    {ev_frag=frag; ev_ev={ev_repr=Event_child pos}} ->
      new_breakpoint (Symbols.any_event_at_pc {frag; pos=(!pos)})
  | {ev_frag=frag; ev_ev={ev_pos=pos}} ->
    let pc = {frag; pos} in
    Exec.protect
      (function () ->
         incr breakpoint_number;
         insert_position pc;
         breakpoints := (!breakpoint_number, event) :: !breakpoints);
    if !Parameters.breakpoint then
      printf "Breakpoint %d at %a: %s\n%!" !breakpoint_number print_pc pc
             (Pos.get_desc event)

(* Remove a breakpoint from lists. *)
let remove_breakpoint number =
  try
    let ev = List.assoc number !breakpoints in
    let pc = {frag = ev.ev_frag; pos=ev.ev_ev.ev_pos} in
    Exec.protect
      (function () ->
         breakpoints := List.remove_assoc number !breakpoints;
         remove_position pc;
         if !Parameters.breakpoint then
           printf "Removed breakpoint %d at %a: %s\n%!" number print_pc pc
                  (Pos.get_desc ev))
  with
    Not_found ->
      prerr_endline ("No breakpoint number " ^ (Int.to_string number) ^ ".");
      raise Not_found

let remove_all_breakpoints () =
  List.iter (function (number, _) -> remove_breakpoint number) !breakpoints

(*** Temporary breakpoints. ***)

(* Temporary breakpoint position. *)
let temporary_breakpoint_position = ref (None : pc option)

(* Execute `funct' with a breakpoint added at `pc'. *)
(* --- Used by `finish'. *)
let exec_with_temporary_breakpoint pc funct =
  let previous_version = !current_version in
    let remove () =
      temporary_breakpoint_position := None;
      current_version := previous_version;
      let count = List.assoc pc !positions in
        decr count;
        if !count = 0 then begin
          positions := List.remove_assoc pc !positions;
          reset_instr pc;
          Symbols.set_event_at_pc pc
        end

    in
      Exec.protect (function () -> insert_position pc);
      temporary_breakpoint_position := Some pc;
      Fun.protect ~finally:(fun () -> Exec.protect remove) funct
