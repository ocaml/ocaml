(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
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

let relocate_event orig ev =
  ev.ev_pos <- orig + ev.ev_pos;
  match ev.ev_repr with
    Event_parent repr -> repr := ev.ev_pos
  | _                 -> ()

let read_symbols' bytecode_file =
  let ic = open_in_bin bytecode_file in
  begin try
    Bytesections.read_toc ic;
    ignore(Bytesections.seek_section ic "SYMB");
  with Bytesections.Bad_magic_number | Not_found ->
    prerr_string bytecode_file; prerr_endline " is not a bytecode file.";
    raise Toplevel
  end;
  Symtable.restore_state (input_value ic);
  begin try
    ignore (Bytesections.seek_section ic "DBUG")
  with Not_found ->
    prerr_string bytecode_file; prerr_endline " has no debugging info.";
    raise Toplevel
  end;
  let num_eventlists = input_binary_int ic in
  let eventlists = ref [] in
  for i = 1 to num_eventlists do
    let orig = input_binary_int ic in
    let evl = (input_value ic : debug_event list) in
    (* Relocate events in event list *)
    List.iter (relocate_event orig) evl;
    eventlists := evl :: !eventlists
  done;
  close_in_noerr ic;
  !eventlists

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
          let cmp ev1 ev2 = compare ev1.ev_char.Lexing.pos_cnum
                                    ev2.ev_char.Lexing.pos_cnum
          in
          let sorted_evl = List.sort cmp evl in
          modules := md :: !modules;
          Hashtbl.add all_events_by_module md sorted_evl;
          let real_evl =
            Primitives.filter
              (function
                 {ev_kind = Event_pseudo} -> false
               | _                        -> true)
              sorted_evl
          in
          Hashtbl.add events_by_module md (Array.of_list real_evl))
    all_events

let any_event_at_pc pc =
  Hashtbl.find events_by_pc pc

let event_at_pc pc =
  let ev = any_event_at_pc pc in
  match ev.ev_kind with
    Event_pseudo -> raise Not_found
  | _            -> ev

let set_event_at_pc pc =
 try ignore(event_at_pc pc); Debugcom.set_event pc
 with Not_found -> ()

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
      if ev.(hi).ev_char.Lexing.pos_cnum < char then raise Not_found;
      hi
    end else begin
      let pivot = (lo + hi) / 2 in
      let e = ev.(pivot) in
      if char <= e.ev_char.Lexing.pos_cnum then bsearch lo pivot
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
    if pos > 0 && char - ev.(pos - 1).ev_char.Lexing.pos_cnum
                  <= ev.(pos).ev_char.Lexing.pos_cnum - char
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
         Event_pseudo -> ()
       | _            -> Debugcom.set_event ev.ev_pos)
    events_by_pc
