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
open Camltk
open Widget

val debug: bool ref
val vert: widget -> (float -> float -> unit) * (unit -> unit)

(* [vert widget]
   can be applied to a text widget so that it expands to show its full
   contents. Returns [scroll] and [check]. [scroll] must be used as
   the YScrollCommand of the widget. [check] can be called when some
   modification occurs in the content of the widget (such as a size change
   in some embedded windows.
   This feature is a terrible hack and should be used with extreme caution.
 *)
