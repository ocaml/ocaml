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

(******************************* Breakpoints ***************************)

open Instruct
open Primitives
open Debugcom
open Checkpoints
open Source

(*** Debugging. ***)
let debug_breakpoints = ref true

(*** Data. ***)

(* Number of the last added breakpoint. *)
let breakpoint_number = ref 0

(* Breakpoint number -> event. *)
let breakpoints = ref ([] : (int * debug_event) list)

(* Program counter -> breakpoint count. *)
let positions = ref ([] : (int * int ref) list)

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
  current_version := !max_version;
  copy_breakpoints ()

(*** Information about breakpoints. ***)

let breakpoints_count () =
  List.length !breakpoints

(* Is there a breakpoint at `pc' ? *)
let breakpoint_at_pc pc =
  List.mem_assoc pc !positions

(* List of breakpoints at `pc'. *)
let breakpoints_at_pc pc =
  List.map fst (filter (function (_, {ev_pos = pos}) -> pos = pc) !breakpoints)

(*** Set and remove breakpoints ***)

(* Remove all breakpoints. *)
let remove_breakpoints pos =
  if !debug_breakpoints then
    (print_string "Removing breakpoints..."; print_newline ());
  List.iter
    (function (pos, _) ->
       if !debug_breakpoints then begin
         print_int pos;
         print_newline()
       end;
       reset_instr pos)
    pos

(* Set all breakpoints. *)
let set_breakpoints pos =
  if !debug_breakpoints then
    (print_string "Setting breakpoints..."; print_newline ());
  List.iter
    (function (pos, _) ->
       if !debug_breakpoints then begin
         print_int pos;
      	 print_newline()
       end;
       set_breakpoint pos)
    pos

(* Ensure the current version in installed in current checkpoint. *)
let update_breakpoints () =
  if !current_checkpoint.c_breakpoint_version <> !current_version then
    Exec.protected
      (function () ->
         remove_breakpoints !current_checkpoint.c_breakpoints;
         set_breakpoints !positions;
         copy_breakpoints ())

let change_version version pos =
  Exec.protected
    (function () ->
       current_version := version;
       positions := pos)
      
(* Execute given function with no breakpoint in current checkpoint. *)
(* --- `goto' runs faster this way (does not stop on each breakpoint). *)
let execute_without_breakpoints f =
  let version = !current_version
  and pos = !positions
  in
    change_version 0 [];
    try
      f ();
      change_version version pos
    with
      x ->
        change_version version pos

(* Add a position in the position list. *)
(* Change version if necessary. *)
let insert_position pos =
  try
    incr (List.assoc pos !positions)
  with
    Not_found ->
      set_breakpoint pos;
      positions := (pos, ref 1) :: !positions;
      new_version ()

(* Remove a position in the position list. *)
(* Change version if necessary. *)
let remove_position pos =
  let count = List.assoc pos !positions in
    decr count;
    if !count = 0 then begin
      positions := assoc_remove !positions pos;
      new_version ();
      reset_instr pos;
      ()
    end

(* Insert a new breakpoint in lists. *)
let new_breakpoint event =
  Exec.protected
    (function () ->
       incr breakpoint_number;
       insert_position event.ev_pos;
       breakpoints := (!breakpoint_number, event) :: !breakpoints);
  print_string "Breakpoint ";
  print_int !breakpoint_number;
  print_string " at ";
  print_int event.ev_pos;
  print_string " : file ";
  print_string event.ev_module;
  print_string ", line ";
  let (start, line) = line_of_pos (get_buffer event.ev_module) event.ev_char in
  print_int line;
  print_string " column ";
  print_int (event.ev_char - start + 1);
  print_newline ()

(* Remove a breakpoint from lists. *)
let remove_breakpoint number =
  try
    let pos = (List.assoc number !breakpoints).ev_pos in
      Exec.protected
      	(function () ->
           breakpoints := assoc_remove !breakpoints number;
           remove_position pos)
  with
    Not_found ->
      prerr_endline ("No breakpoint number " ^ (string_of_int number) ^ ".");
      raise Not_found

let remove_all_breakpoints () =
  List.iter (function (number, _) -> remove_breakpoint number) !breakpoints

(*** Temporary breakpoints. ***)

(* Temporary breakpoint position. *)
let temporary_breakpoint_position = ref (None : int option)

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
          positions := assoc_remove !positions pc;
          reset_instr pc;
      	  ()
	  end

    in
      Exec.protected (function () -> insert_position pc);
      temporary_breakpoint_position := Some pc;
      try
        funct ();
        Exec.protected remove
      with
        x ->
          Exec.protected remove;
          raise x
