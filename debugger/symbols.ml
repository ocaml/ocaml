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

let modules =
  ref ([] : string list)

let events =
  ref ([] : debug_event list)
let events_by_pc =
  (Hashtbl.create 257 : (int, debug_event) Hashtbl.t)
let events_by_module =
  (Hashtbl.create 17 : (string, debug_event array) Hashtbl.t)
let all_events_by_module =
  (Hashtbl.create 17 : (string, debug_event list) Hashtbl.t)

let read_symbols' bytecode_file =
  let ic = open_in_bin bytecode_file in
  let pos_trailer =
    in_channel_length ic - 20 - String.length Config.exec_magic_number in
  seek_in ic pos_trailer;
  let code_size = input_binary_int ic in
  let prim_size = input_binary_int ic in
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

  modules := []; events := [];
  Hashtbl.clear events_by_pc; Hashtbl.clear events_by_module;
  Hashtbl.clear all_events_by_module;

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
          let md = ev.ev_module in
          let sorted_evl =
            Sort.list (fun ev1 ev2 -> ev1.ev_char <= ev2.ev_char) evl in
          modules := md :: !modules;
          Hashtbl.add all_events_by_module md sorted_evl;
          let real_evl =
            Primitives.filter
              (function
                 {ev_kind = Event_function | Event_return _} -> false
               | _                                           -> true)
              sorted_evl
          in
          Hashtbl.add events_by_module md (Array.of_list real_evl))
    all_events

let any_event_at_pc pc =
  Hashtbl.find events_by_pc pc

let event_at_pc pc =
  let ev = any_event_at_pc pc in
  match ev.ev_kind with
    Event_function | Event_return _ -> raise Not_found
  | _                               -> ev

(* List all events in module *)
let events_in_module mdle =
  try
    Hashtbl.find all_events_by_module mdle
  with Not_found ->
    []

(* Binary search of event at or just after char *)
let find_event ev char =
  let rec bsearch lo hi =
    if lo >= hi then begin
      if ev.(hi).ev_char < char then raise Not_found;
      hi
    end else begin
      let pivot = (lo + hi) / 2 in
      let e = ev.(pivot) in
      if char <= e.ev_char then bsearch lo pivot
                           else bsearch (pivot + 1) hi
    end
  in
  bsearch 0 (Array.length ev - 1)

(* Return first event after the given position. *)
(* Raise [Not_found] if module is unknown or no event is found. *)
let event_at_pos md char =
  let ev = Hashtbl.find events_by_module md in
  ev.(find_event ev char)

(* Return event closest to given position *)
(* Raise [Not_found] if module is unknown or no event is found. *)
let event_near_pos md char =
  let ev = Hashtbl.find events_by_module md in
  try
    let pos = find_event ev char in
    (* Desired event is either ev.(pos) or ev.(pos - 1),
       whichever is closest *)
    if pos > 0 && char - ev.(pos - 1).ev_char <= ev.(pos).ev_char - char
    then ev.(pos - 1)
    else ev.(pos)
  with Not_found ->
    let pos = Array.length ev - 1 in
    if pos < 0 then raise Not_found;
    ev.(pos)

(* Flip "event" bit on all instructions *)
let set_all_events () =
  Hashtbl.iter
    (fun pc ev ->
       match ev.ev_kind with
         Event_function | Event_return _ -> ()
       | _                               -> Debugcom.set_event ev.ev_pos)
    events_by_pc
