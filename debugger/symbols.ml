(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Handling of symbol tables (globals and events) *)

open Instruct
open Debugger_config (* Toplevel *)

let verbose = ref true

let modules =
  ref ([] : string list)

let events =
  ref ([] : debug_event list)
let events_by_pc =
  (Hashtbl.create 257 : (int, debug_event) Hashtbl.t)
let events_by_module =
  (Hashtbl.create 17 : (string, debug_event array) Hashtbl.t)

let read_symbols' bytecode_file =
  let ic = open_in_bin bytecode_file in
  let pos_trailer =
    in_channel_length ic - 16 - String.length Config.exec_magic_number in
  seek_in ic pos_trailer;
  let code_size = input_binary_int ic in
  let data_size = input_binary_int ic in
  let symbol_size = input_binary_int ic in
  let debug_size = input_binary_int ic in
  let magic = String.create (String.length Config.exec_magic_number) in
  really_input ic magic 0 (String.length Config.exec_magic_number);
  if magic <> Config.exec_magic_number then begin
    prerr_string bytecode_file; prerr_endline " is not a bytecode file.";
    exit 2
  end;
  if debug_size = 0 then begin
    prerr_string bytecode_file; prerr_endline " has no debugging info.";
    exit 2
  end;
  seek_in ic (pos_trailer - debug_size - symbol_size);
  Symtable.restore_state (input_value ic);
  let all_events = (input_value ic : debug_event list list) in
    close_in ic;
    all_events

let read_symbols bytecode_file =
  let all_events = read_symbols' bytecode_file in
  List.iter
    (fun evl ->
      List.iter
        (fun ev ->
	  events := ev :: !events;
      	  Hashtbl.add events_by_pc ev.ev_pos ev)
        evl)
    all_events;

  List.iter
    (function
        [] -> ()
      | ev :: _ as evl ->
      	  let md = ev.ev_module
          and sorted_evl =
            Sort.list (fun ev1 ev2 -> ev1.ev_char <= ev2.ev_char) evl in
	  modules := md :: !modules;
          Hashtbl.add events_by_module md (Array.of_list sorted_evl))
    all_events

let event_at_pc pc =
  Hashtbl.find events_by_pc pc
(*
  try
    Hashtbl.find events_by_pc pc
  with Not_found ->
    prerr_string "No event at pc="; prerr_int pc; prerr_endline ".";
    raise Toplevel
*)

(* Return the list of events at `pc' *)
let events_at_pc =
  Hashtbl.find_all events_by_pc

(* List all events in module *)
let events_in_module mdle =
  try
    Hashtbl.find events_by_module mdle
  with Not_found ->
    [||]

(* Binary search of event at or just after char *)
let find_event ev char =
  let rec bsearch lo hi =
    if lo >= hi then
      if hi + 1 < Array.length ev then hi+1 else hi
    else begin
      let pivot = (lo + hi) / 2 in
      let e = ev.(pivot) in
      if char = e.ev_char then pivot else
      if char < e.ev_char then bsearch lo (pivot - 1)
                          else bsearch (pivot + 1) hi
    end
  in bsearch 0 (Array.length ev - 1)

(* Return first event after the given position. *)
(* Raise [Not_found] if module is unknown. *)
let event_at_pos md char =
  let ev = Hashtbl.find events_by_module md in
  ev.(find_event ev char)

(* Return event closest to given position *)
(* Raise [Not_found] if module is unknown. *)
let event_near_pos md char =
  let ev = Hashtbl.find events_by_module md in
  let pos = find_event ev char in
  (* Desired event is either ev.(pos) or ev.(pos - 1), whichever is closest *)
  if pos > 0 && char - ev.(pos - 1).ev_char <= char - ev.(pos).ev_char
  then ev.(pos - 1)
  else ev.(pos)

(* Flip "event" bit on all instructions *)
let set_all_events () =
  Hashtbl.iter
    (fun pc ev -> Debugcom.set_event ev.ev_pos)
    events_by_pc
