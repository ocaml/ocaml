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

open Instruct
open Format
open Primitives
open Debugcom
open Checkpoints
open Events
open Symbols
open Frames
open Show_source
open Breakpoints

(* Display information about the current event. *)
let show_current_event () =
  print_string "Time : "; print_int (current_time ());
  (match current_pc () with
     Some pc ->
       print_string " - pc : "; print_int pc
   | _ -> ());
  update_current_event ();
  reset_frame ();
  match current_report ()  with
    None ->
      print_newline ();
      print_string "Beginning of program."; print_newline ();
      show_no_point ()
  | Some {rep_type = (Event | Breakpoint); rep_program_pointer = pc} -> 
     let (mdle, point) = current_point () in
	print_string (" - module " ^ mdle);
      	print_newline ();
	(match breakpoints_at_pc pc with
	   [] ->
      	     ()
	 | [breakpoint] ->
	     print_string "Breakpoint : "; print_int breakpoint;
             print_newline ()
	 | breakpoints ->
	     print_string "Breakpoints : ";
	     List.iter (function x -> print_int x; print_string " ") breakpoints;
      	     print_newline ());
        show_point mdle point (current_event_is_before ()) true
  | Some {rep_type = Exited} ->
      print_newline (); print_string "Program exit."; print_newline ();
      show_no_point ()
  | Some {rep_type = Uncaught_exc} ->
      print_newline ();
      print_string "Program end.";
      print_newline ();
      open_box 0;
      print_string "Uncaught exception:"; print_space();
      (*print_value (get_accu ()) type_exn;*)
      close_box();
      print_newline();
      show_no_point ()
  | Some {rep_type = Trap_barrier} ->
					(* Trap_barrier not visible outside *)
	       	       	       	       	(* of module `time_travel'. *)
      Misc.fatal_error "Show_information.show_current_event"

(* Display short information about one frame. *)

let show_one_frame framenum event =
  print_string "#";
  print_int framenum;
  print_string "  Pc : ";
  print_int event.ev_pos;
  print_string "  ";
  print_string event.ev_module;
  print_string " char ";
  print_int event.ev_char;
  print_newline ()

(* Display information about the current frame. *)
(* --- `select frame' must have succeded before calling this function. *)
let show_current_frame selected =
  match !selected_event with
    None ->
      print_newline ();
      print_string "No frame selected.";
      print_newline ()
  | Some sel_ev ->
      show_one_frame !current_frame sel_ev;
      begin match breakpoints_at_pc sel_ev.ev_pos with
        [] ->
          ()
      | [breakpoint] ->
          print_string "Breakpoint : "; print_int breakpoint; print_newline ()
      | breakpoints ->
          print_string "Breakpoints : ";
          List.iter (function x -> print_int x; print_string " ") breakpoints;
          print_newline ()
      end;
      show_point sel_ev.ev_module sel_ev.ev_char
                 (selected_event_is_before ()) selected
