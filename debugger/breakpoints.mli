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

(******************************* Breakpoints ***************************)

(*** Debugging. ***)
val debug_breakpoints : bool ref

(*** Information about breakpoints. ***)

val breakpoints_count : unit -> int

(* Breakpoint number -> code_event. *)
type breakpoint_id = int
val breakpoints : (breakpoint_id * Events.code_event) list ref

(* Is there a breakpoint at `pc' ? *)
val breakpoint_at_pc : Debugcom.pc -> bool

(* List of breakpoints at `pc'. *)
val breakpoints_at_pc : Debugcom.pc -> breakpoint_id list

(*** Set and remove breakpoints ***)

(* Ensure the current version is installed in current checkpoint. *)
val update_breakpoints : unit -> unit

(* Execute given function with no breakpoint in current checkpoint. *)
(* --- `goto' run faster so (does not stop on each breakpoint). *)
val execute_without_breakpoints : (unit -> unit) -> unit

(* Insert a new breakpoint in lists. *)
val new_breakpoint : Events.code_event -> unit

(* Remove a breakpoint from lists. *)
val remove_breakpoint : breakpoint_id -> unit

val remove_all_breakpoints : unit -> unit

(*** Temporary breakpoints. ***)

(* Temporary breakpoint position. *)
val temporary_breakpoint_position : Debugcom.pc option ref

(* Execute `funct' with a breakpoint added at `pc'. *)
(* --- Used by `finish'. *)
val exec_with_temporary_breakpoint : Debugcom.pc -> (unit -> unit) -> unit
