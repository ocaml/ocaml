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
  | (Some pc) as opt_pc when opt_pc <> !old_pc ->
      current_event := begin try
                         Some (Symbols.event_at_pc pc)
                       with Not_found ->
                         None
                       end;
      old_pc := opt_pc
  | _ ->
      ()

(* Current position in source. *)
(* Raise `Not_found' if not on an event (beginning or end of program). *)
let current_point () =
  match !current_event with
    None ->
      raise Not_found
  | Some {ev_char = point; ev_module = mdle} ->
      (mdle, point.Lexing.pos_cnum)

let current_event_is_before () =
  match !current_event with
    None ->
      raise Not_found
  | Some {ev_kind = Event_before} ->
      true
  | _ ->
      false
