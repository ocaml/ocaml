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

(********************************* Events ******************************)

open Instruct
open Primitives
open Checkpoints

(* Previous `pc'. *)
(* Save time if `update_current_event' is called *)
(* several times at the same point. *)
let old_pc = ref (None : int option)

(*** Current events. ***)

(* Event at current position *)
let current_event =
  ref (None : debug_event option)

(* Recompute the current event *)
let update_current_event () =
  match current_pc () with
    None ->
      current_event := None;
      old_pc := None
  | (Some pc) as opt_pc ->
      if opt_pc <> !old_pc then begin
      	current_event := Some (Symbols.event_at_pc pc);
      	old_pc := opt_pc
	end

(* Current position in source. *)
(* Raise `Not_found' if not on an event (beginning or end of program). *)
let current_point () =
  match !current_event with
    None ->
      raise Not_found
  | Some {ev_char = point; ev_file = mdle} ->
      (mdle, point)

let current_event_is_before () =
  match !current_event with
    None ->
      raise Not_found
  | Some {ev_kind = Event_before} ->
      true
  | _ ->
      false

(*** Finding events. ***)

(* List the events in `module'. *)
let events_in_module mdle =
  let filename = String.uncapitalize mdle ^ ".ml" in
  filter
    (function {ev_file = f} -> f = filename)
    !Symbols.events

(* First event after the given position. *)
(* Raise `Not_found' if no such event. *)
let event_after_pos mdle position =
  match
    List.fold_right
      (function
      	 ({ev_char = pos1} as ev) ->
	   if pos1 < position then
	     function x -> x
	   else
	     function
	       None ->
	         Some ev
             | (Some {ev_char = pos2} as old) ->
	         if pos1 < pos2 then
		   Some ev
		 else
		   old)
      (events_in_module mdle)
      None
  with
    None ->
      raise Not_found
  | Some x ->
      x

(* Nearest event from given position. *)
(* Raise `Not_found' if no such event. *)
let event_near_pos mdle position =
  match events_in_module mdle with
    [] ->
      raise Not_found
  | [event] ->
      event
  | a::l ->
      List.fold_right
      	(fun ({ev_char = pos1} as ev) ({ev_char = pos2} as old) ->
	   if abs (position - pos1) < abs (position - pos2) then
	     ev
	   else
	     old)
	l
	a
