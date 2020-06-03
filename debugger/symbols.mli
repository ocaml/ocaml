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

open Events

(* Modules used by the program. *)
val modules : string list ref

(* Absolute directories containing source code on machine where source was
 * compiled *)
val program_source_dirs : string list ref

(* Clear loaded symbols *)
val clear_symbols : unit -> unit

(* Read debugging info from executable or dynlinkable file
   and associate with given code fragment *)
val read_symbols : int -> string -> unit

(* Add debugging info from memory and associate with given
   code fragment *)
val add_symbols : int -> Instruct.debug_event list list -> unit

(* Erase debugging info associated with given code fragment *)
val erase_symbols : int -> unit

(* Return the list of all code fragments that have debug info associated *)
val code_fragments : unit -> int list

(* Flip "event" bit on all instructions in given fragment *)
val set_all_events : int -> unit

(* Return event at given PC, or raise Not_found *)
(* Can also return pseudo-event at beginning of functions *)
val any_event_at_pc : Debugcom.pc -> code_event

(* Return event at given PC, or raise Not_found *)
val event_at_pc : Debugcom.pc -> code_event

(* Set event at given PC *)
val set_event_at_pc : Debugcom.pc -> unit

(* List the events in `module'. *)
val events_in_module : string -> int * Instruct.debug_event list

(* List the modules in given code fragment. *)
val modules_in_code_fragment : int -> string list

(* First event after the given position. *)
(* --- Raise `Not_found' if no such event. *)
val event_at_pos : string -> int -> code_event

(* Closest event from given position. *)
(* --- Raise `Not_found' if no such event. *)
val event_near_pos : string -> int -> code_event

(* Recompute the current event *)
val update_current_event : unit -> unit
