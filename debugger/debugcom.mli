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

(* Low-level communication with the debuggee *)

type execution_summary =
    Event
  | Breakpoint
  | Exited
  | Trap_barrier
  | Uncaught_exc

type report =
  { rep_type : execution_summary;
    rep_event_count : int;
    rep_stack_pointer : int;
    rep_program_pointer : int }

type checkpoint_report =
    Checkpoint_done of int
  | Checkpoint_failed

(* Set the current connection with the debuggee *)
val set_current_connection : Primitives.io_channel -> unit

(* Put an event at given pc *)
val set_event : int -> unit

(* Put a breakpoint at given pc *)
val set_breakpoint : int -> unit

(* Remove breakpoint or event at given pc *)
val reset_instr : int -> unit

(* Create a new checkpoint (the current process forks). *)
val do_checkpoint : unit -> checkpoint_report

(* Step N events. *)
val do_go : int -> report

(* Tell given process to terminate *)
val stop :  Primitives.io_channel -> unit

(* Tell given process to wait for its children *)
val wait_child : Primitives.io_channel -> unit

(* Move to initial frame (that of current function). *)
(* Return stack position and current pc *)
val initial_frame : unit -> int * int

(* Get the current frame position *)
(* Return stack position and current pc *)
val get_frame : unit -> int * int

(* Set the current frame *)
val set_frame : int -> unit

(* Move up one frame *)
(* Return stack position and current pc.
   If there's no frame above, return (-1, 0). *)
val up_frame : int -> int * int

(* Set the trap barrier to given stack position. *)
val set_trap_barrier : int -> unit

(* Handling of remote values *)
type remote_value
val remote_value_is_int : remote_value -> bool
val int_value : remote_value -> int
val value_int : int -> remote_value
val get_local : int -> remote_value
val get_environment : int -> remote_value
val get_global : int -> remote_value
val get_accu : unit -> remote_value
val get_obj : remote_value -> int * remote_value array
val get_header : remote_value -> int * int
val get_field : remote_value -> int -> remote_value
val marshal_obj : remote_value -> 'a
val get_closure_code : remote_value -> int

exception Marshalling_error
