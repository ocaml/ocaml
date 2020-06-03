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

(****************************** Frames *********************************)

open Events

(* Current frame number *)
val current_frame : int ref

(* Fragment and event at selected position. *)
val selected_event : code_event option ref

(* Selected position in source (module, line, column). *)
(* Raise `Not_found' if not on an event. *)
val selected_point : unit -> string * int * int

val selected_event_is_before : unit -> bool

(* Select a frame. *)
(* Raise `Not_found' if no such frame. *)
(* --- Assume the currents events have already been updated. *)
val select_frame : int -> unit

(* Select a frame. *)
(* Same as `select_frame' but raise no exception if the frame is not found. *)
(* --- Assume the currents events have already been updated. *)
val try_select_frame : int -> unit

(* Return to default frame (frame 0). *)
val reset_frame : unit -> unit

(* Perform a stack backtrace.
   Call the given function with the events for each stack frame,
   or None if we've encountered a stack frame with no debugging info
   attached. Stop when the function returns false, or frame with no
   debugging info reached, or top of stack reached. *)
val do_backtrace : (code_event option -> bool) -> unit

(* Return the number of frames in the stack, or (-1) if it can't be
   determined because some frames have no debugging info. *)
val stack_depth : unit -> int
