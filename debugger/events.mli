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

open Instruct

(* A debug event associated with a code fragment. *)
type code_event =
  { ev_frag : int;
    ev_ev : Instruct.debug_event }

val get_pos : debug_event -> Lexing.position;;

(** Current events. **)

(* The event at current position. *)
val current_event : code_event option ref

(* Current position in source. *)
(* Raise `Not_found' if not on an event (beginning or end of program). *)
val get_current_event : unit -> code_event

val current_event_is_before : unit -> bool
