(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of Objective Caml            *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the Objective Caml source tree. *)
(*                                                                     *)
(***********************************************************************)
(* Synthetic events *)
open Camltk
open Widget


val send : string -> widget -> unit
  (* [send event_name widget] *)

val broadcast : string -> unit
  (* [broadcase event_name] *)

val bind : widget -> string -> (widget -> unit) -> unit
  (* [bind event_name callback] *)

val remove : widget -> string -> unit
  (* [remove widget event_name] *)
