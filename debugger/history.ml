(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Checkpoints
open Misc
open Primitives
open Debugger_config

let history = ref ([] : int list)

let empty_history () =
  history := []

let add_current_time () =
  let time = current_time () in
    if history = ref [] then
      history := [time]
    else if time <> List.hd !history then
      history := list_truncate !history_size (time::!history)

let previous_time_1 () =
  match !history with
    _::((time::_) as hist) ->
      history := hist; time
  | _ ->
      prerr_endline "No more information."; raise Toplevel

let rec previous_time n =
  if n = 1
  then previous_time_1()
  else begin ignore(previous_time_1()); previous_time(n-1) end
