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
val new_label_entry :
  Widget.widget ->
  string -> (string -> unit) -> Widget.widget * Widget.widget
   (* [new_label_entry parent label action]
      creates a "labelled" entry widget where [action] will be invoked
      when the user types Return in the widget.
      Returns (frame widget, entry widget)
    *)
val new_labelm_entry :
  Widget.widget ->
  string -> Textvariable.textVariable -> Widget.widget * Widget.widget
   (* [new_labelm_entry parent label variable]
      creates a "labelled" entry widget whose contents is [variable].
      Returns (frame widget, entry widget)
    *)
