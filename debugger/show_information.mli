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

(* Display information about the current event. *)
val show_current_event : unit -> unit;;

(* Display information about the current frame. *)
(* --- `select frame' must have succeded before calling this function. *)
val show_current_frame : bool -> unit;;

(* Display short information about one frame. *)
val show_one_frame : int -> Instruct.debug_event -> unit
