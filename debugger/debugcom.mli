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

(* Low-level communication with the debuggee *)

module Sp : sig
  type t
  val null : t
  val base : t -> int -> t
  val compare : t -> t -> int
end

type pc =
  { frag : int;
    pos : int; }

val main_frag : int

type execution_summary =
    Event
  | Breakpoint
  | Exited
  | Trap_barrier
  | Uncaught_exc
  | Debug_info of Instruct.debug_event list array
  | Code_loaded of int
  | Code_unloaded of int

type report =
  { rep_type : execution_summary;
    rep_event_count : int64;
    rep_stack_pointer : Sp.t;
    rep_program_pointer : pc }

type checkpoint_report =
    Checkpoint_done of int
  | Checkpoint_failed

type follow_fork_mode =
    Fork_child
  | Fork_parent

(* Set the current connection with the debuggee *)
val set_current_connection : Primitives.io_channel -> unit

(* Put an event at given pc *)
val set_event : pc -> unit

(* Put a breakpoint at given pc *)
val set_breakpoint : pc -> unit

(* Remove breakpoint or event at given pc *)
val reset_instr : pc -> unit

(* Create a new checkpoint (the current process forks). *)
val do_checkpoint : unit -> checkpoint_report

(* Step N events. *)
val do_go : int64 -> report

(* Tell given process to terminate *)
val stop :  Primitives.io_channel -> unit

(* Tell given process to wait for its children *)
val wait_child : Primitives.io_channel -> unit

(* Move to initial frame (that of current function). *)
(* Return stack position and current pc *)
val initial_frame : unit -> Sp.t * pc
val set_initial_frame : unit -> unit

(* Get the current frame position *)
(* Return stack position and current pc *)
val get_frame : unit -> Sp.t * pc

(* Set the current frame *)
val set_frame : Sp.t -> unit

(* Move up one frame *)
(* Return stack position and current pc.
   If there's no frame above, return (null_sp, _).
   The argument is the size of the current frame.
 *)
val up_frame : int -> Sp.t * pc

(* Set the trap barrier to given stack position. *)
val set_trap_barrier : Sp.t -> unit

(* Set whether the debugger follow the child or the parent process on fork *)
val fork_mode : follow_fork_mode ref
val update_follow_fork_mode : unit -> unit

(* Handling of remote values *)

exception Marshalling_error

module Remote_value :
  sig
    type t

    val repr : 'a -> t
    val obj : t -> 'a
    val is_block : t -> bool
    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
    val double_field : t -> int -> float
    val double_array_tag : int
    val same : t -> t -> bool

    val of_int : int -> t

    val local : int -> t
    val from_environment : int -> t
    val global : int -> t
    val accu : unit -> t
    val closure_code : t -> pc

    (* Returns a hexadecimal representation of the remote address,
       or [""] if the value is local. *)
    val pointer : t -> string
  end
